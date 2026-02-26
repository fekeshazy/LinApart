(* ::Package:: *)

(* ::Subsection:: *)
(*Partial fraction function*)


(*
  mathematicaPartialFraction[coeff, ignoreFrac, keepForDivision, keepFrac, var/vars, options]

  Core partial fraction decomposition function. Handles three methods:

  Single-variable:
    1. "ExtendedLaurentSeries" - Computes residues at each pole using 
       derivatives and the Laurent series formula. Supports parallelization.
    2. "Euclidean" - Uses polynomial extended GCD to iteratively reduce 
       pairs of denominators via B\[EAcute]zout's identity.

  Multivariate:
    3. "NullRelationElimination" - Eliminates common singularities via 
       null relations, then computes residues at each basis.

  Parameters:
    coeff          - Variable-free coefficient from preprocessing.
    ignoreFrac     - Factors with non-integer powers, factored out by SeparateFrac.
    keepForDivision - Numerator factors for improper fraction handling.
                     If this contains the variable(s), a polynomial part is 
                     extracted via Series at infinity.
    keepFrac       - The proper fraction to decompose.
    var            - Single variable (Symbol) or list of variables (List).
    options        - Options inherited from LinApart.

  Returns:
    The partial fraction decomposition of coeff * ignoreFrac * keepForDivision * keepFrac.

  Notes:
    - coeff * ignoreFrac is placed OUTSIDE the residue computation 
      for efficiency. Large coefficients significantly slow down the 
      internal sums in ResidueForLaurentSeries/ResidueForBasis.
    - Plus has to unpack packed-arrays to add elements, while Total 
      can operate on packed-arrays directly. This is why Total is used 
      for the final summation.
*)

ClearAll[mathematicaPartialFraction]

Options[mathematicaPartialFraction] = $LinApartOptions;

mathematicaPartialFraction[args___] := Null /; !CheckArguments[mathematicaPartialFraction[args], {5,6}]


		(* ============================================== *)
		(*    Single-variable: Extended Laurent Series    *)
		(* ============================================== *)

	(*
	  Method "ExtendedLaurentSeries":
	    1. Extract numerator and denominator factors with multiplicities.
	    2. Extract polynomial coefficients (constants) of each denominator.
	    3. If improper fraction, extract polynomial part via Series at infinity.
	    4. Build residue data for each {pole, poleOrder} combination.
	    5. Compute residues via ResidueForLaurentSeries (sequential or parallel).
	    6. Sum all contributions.
	*)

mathematicaPartialFraction[
    coeff_, ignoreFrac_, keepForDivision_, keepFrac_, var_Symbol, 
    options:OptionsPattern[]
] := Module[
    {
        tmp = keepFrac,
        tmp1a, tmp1b,
        m, den, const,
        tmpPolynomialPart,
        tmpFractionedPart,
        dummyIndex
    },

    (* Extract numerator. *)
    tmp1a = Numerator[tmp];

    (* 
      Extract denominator factors as a list.
      Handle single Power vs product of Powers.
    *)
    tmp1b = 1/Denominator[tmp];
    tmp1b = If[Head[tmp1b] === Power, {tmp1b}, List @@ tmp1b];

    (* Get base and exponent for each denominator factor. *)
    tmp = GetExponent[#, var] & /@ List @@ tmp1b;
    den = tmp[[All, 1]];

    (*
      Extract polynomial coefficients of each denominator factor.
      Drop the leading coefficient (last element) since denominators 
      are already normalized. Store degree and remaining coefficients.
    *)
    const = CoefficientList[#, var] & /@ den;
    const = const[[All, ;; -2]];
    const = {Length[#], #} & /@ const;

    (* Multiplicities (negated exponents since they appear in denominator). *)
    m = -tmp[[All, 2]];

    (*
      Extract polynomial part for improper fractions.
      If keepForDivision contains the variable, use Series at infinity 
      to separate the polynomial and proper fraction parts.
    *)
    tmpPolynomialPart = If[FreeQ[keepForDivision, var],
        0,
        coeff * ignoreFrac * (
            Series[keepForDivision * keepFrac, {var, Infinity, 0}] // Normal
        )
    ];

    (*
      Build residue data for each {pole, poleOrder} combination.
      
      For each denominator factor i with multiplicity m_i, we need 
      residue contributions for poleOrder = 1, 2, ..., m_i.
      
      Each entry is:
        {restOfFunction, pole, multiplicity, poleOrder, {degree, constants}, var}
      
      where restOfFunction = keepForDivision * numerator * (all other denominators).
    *)
    tmpFractionedPart = Table[
        Table[
            {
                keepForDivision * tmp1a * Times @@ Delete[tmp1b, i],
                1/tmp1b[[i]],
                m[[i]],
                poleOrder,
                const[[i]],
                var
            },
            {poleOrder, 1, m[[i]]}
        ],
        {i, 1, Length[tmp1b]}
    ] // Flatten[#, 1] &;

    (*
      Compute residues: parallel or sequential.
      
      coeff * ignoreFrac is placed outside for efficiency \[LongDash] large 
      coefficients slow down the derivative computations inside 
      ResidueForLaurentSeries.
      
      Total is used instead of Plus @@ because Total operates on 
      packed-arrays without unpacking.
    *)
    (If[OptionValue["Parallel"][[1]],

        (* 
          Parallel computation:
          RandomSample for load balancing, then partition into chunks 
          for each core.
        *)
        tmpFractionedPart = tmpFractionedPart // RandomSample // 
            Partition[#, UpTo[Length[#]/OptionValue["Parallel"][[2]] // Ceiling]] &;
        
        ComputeParallel[
            tmpFractionedPart,
            (ResidueForLaurentSeries[coeff * ignoreFrac, #]) &,
            OptionValue["Parallel"][[2]],
            OptionValue["Parallel"][[3]]
        ] // Flatten,

        (* Sequential computation. *)
        Table[
            coeff * ignoreFrac * ResidueForLaurentSeries @@ dummyIndex,
            {dummyIndex, tmpFractionedPart}
        ]
    ] // Total) + tmpPolynomialPart

] /; OptionValue["Method"] === "ExtendedLaurentSeries"


		(* ============================================== *)
		(*      Single-variable: Euclidean Method         *)
		(* ============================================== *)

	(*
	  Method "Euclidean":
	    1. If improper fraction, extract polynomial part via Series at infinity.
	    2. Extract denominator factors and find all pairs.
	    3. For each pair, compute B\[EAcute]zout coefficients via PolynomialExtendedGCD.
	       Make coefficients symbolic to avoid expression blowup.
	    4. Build substitution rules from B\[EAcute]zout's identity:
	         1/(f*g) = a/g + b/f  where a*f + b*g = 1
	    5. Apply rules iteratively (ReplaceRepeated) until no more pairs remain.
	    6. Clean up: gather by dependency, apply Apart on remaining structures,
	       remove polynomial parts (which are artifacts of the reduction).
	    7. Substitute back symbolic coefficients.
	
	  Notes:
	    - MakeCoefficientsSymbolic is crucial here: without it, the 
	      expansion during rule application causes massive expression blowup.
	    - The gathering step is necessary because the iterative reduction 
	      produces many redundant terms that can cancel.
	    - Although gathering costs time, it prevents the kernel from 
	      crashing when trying to expand the result.
	*)

mathematicaPartialFraction[
    coeff_, ignoreFrac_, keepForDivision_, keepFrac_, var_Symbol, 
    options:OptionsPattern[]
] := Module[
    {
        tmpPolynomialPart,
        tmp, tmpRules,
        tmp1, tmp2, tmp1Rules, tmp2Rules,
        dens, pairs,
        rulesGCD, dummyRulesGCD,
        pow1, pow2, tmpCoeff
    },

    (* Extract polynomial part for improper fractions. *)
    tmpPolynomialPart = If[FreeQ[keepForDivision, var],
        0,
        coeff * ignoreFrac * (
            Series[keepForDivision * keepFrac, {var, Infinity, 0}] // Normal
        )
    ];

    (* Extract denominator factors. *)
    tmp = Denominator[keepFrac];
    tmp = If[Head[tmp] === Power, {tmp}, List @@ tmp];

    (* Get base and exponent for each factor. *)
    tmp = GetExponent[#, var] & /@ List @@ tmp;
    dens = tmp[[All, 1]];

    (* All pairs of denominators for B\[EAcute]zout reduction. *)
    pairs = Subsets[dens, {2}];

    (*
      Compute B\[EAcute]zout coefficients for each pair.
      
      For polynomials f, g with gcd(f,g) = 1:
        a*f + b*g = 1  (B\[EAcute]zout's identity)
      
      Make coefficients symbolic to prevent expression blowup 
      during the iterative substitution.
    *)
    tmp = Table[
        tmp = PolynomialExtendedGCD[pairs[[i, 1]], pairs[[i, 2]], var][[2]];

        {tmp1, tmp1Rules} = MakeCoefficientsSymbolic[tmp[[1]], var, Unique[dummyF]];
        {tmp2, tmp2Rules} = MakeCoefficientsSymbolic[tmp[[2]], var, Unique[dummyF]];

        {
            {pairs[[i, 1]] tmp1, pairs[[i, 2]] tmp2},
            Flatten[{tmp1Rules, tmp2Rules}]
        },

        {i, 1, Length[pairs]}
    ];

    (* Separate rules and coefficient restoration rules. *)
    {rulesGCD, tmpRules} = {tmp[[All, 1]], Flatten[tmp[[All, 2]]]};

    (*
      Build substitution rules from B\[EAcute]zout's identity.
      
      Using With to inject the computed values into RuleDelayed, 
      preventing premature evaluation.
      
      The rule transforms:
        coeff / (f^pow1 * g^pow2) -> coeff * (a/g + b/f) / (f^pow1 * g^pow2)
      which reduces the total number of denominator factors.
      
      Condition: both powers must be negative (both in denominator).
    *)
    dummyRulesGCD = MapThread[
        With[
            {
                tmp11 = #1[[1]], tmp12 = #1[[2]],
                tmp21 = #2[[1]], tmp22 = #2[[2]]
            },
            RuleDelayed[
                tmpCoeff_. Times[tmp11^pow1_, tmp12^pow2_] /; (pow1 < 0 && pow2 < 0),
                tmpCoeff * tmp21 * tmp11^pow1 * tmp12^pow2 + 
                    tmpCoeff * tmp22 * tmp11^pow1 * tmp12^pow2
            ]
        ] &,
        {pairs, rulesGCD}
    ] // Dispatch;

    (* Apply rules iteratively until no more reductions possible. *)
    tmp = coeff keepForDivision ignoreFrac keepFrac //. dummyRulesGCD;

    (*
      Clean up the result:
      
      1. Gather by variable dependency, apply Apart on each dependent 
         structure. This handles remaining x^pow/denom^pow structures 
         that can be reduced further by polynomial division.
      
      2. Remove polynomial parts (set to 0). The proper partial fraction 
         decomposition has no polynomial terms \[LongDash] any that appear are 
         artifacts of the reduction process.
      
      Although gathering costs time and efficiency, it prevents the 
      kernel from crashing when expanding the result. The cancellations 
      must be realized either by expanding coefficients or checking 
      numerically with random primes.
    *)
    tmp = tmp // GatherByDependency[#, var, None, Apart[#, var] &] &;
    tmp = tmp // GatherByDependency[#, var, None, If[PolynomialQ[#, var], 0, #] &] &;

    (* Substitute back symbolic coefficients and add polynomial part. *)
    tmp + tmpPolynomialPart /. tmpRules

] /; OptionValue["Method"] === "Euclidean"


		
		(* ============================================== *)
		(*    Multivariate: Null Relation Elimination     *)
		(* ============================================== *)


	(*
	  Multivariate partial fraction decomposition via null relation 
	  elimination and basis residue computation.
	
	  Algorithm:
	    1. Extract polynomial part via multivariate Series at infinity.
	    2. Eliminate null relations to disentangle common singularities.
	    3. Separate already-decomposed terms from those needing basis decomposition.
	    4. For remaining terms, find all n-element bases.
	    5. Group terms sharing a common basis for efficient computation.
	    6. Compute residue contribution from each basis via ResidueForBasis.
	    7. Sum all contributions with the polynomial part.
	
	  Notes:
	    - coeff * ignoreFrac is placed outside the residue computation 
	      for efficiency, matching the single-variable pattern.
	    - Supports parallel computation over bases when "Parallel" option is set.
	*)

mathematicaPartialFraction[
    coeff_, ignoreFrac_, keepForDivision_, keepFrac_, vars_List, 
    options:OptionsPattern[]
] := Module[
    {
        tmp,
        tmpPolynomialPart,
        tmpFractionedPart,

        varPat,
        eliminated, denomOrdering,
        
        allBases,

        terms,
        decomposedTerms,
        remainingTerms,
        
        termsWithoutBases,
        termsWithBases,

        bareDenoms, bases,
        basisData
    },

    varPat = Alternatives @@ vars;

    (*
      Extract polynomial part for improper fractions.
      For multivariate, Series is applied sequentially in each variable.
    *)
    tmpPolynomialPart = If[FreeQ[keepForDivision, varPat],
        0,
        coeff * ignoreFrac * (
            Series[
                keepForDivision * keepFrac,
                Sequence @@ ({#, Infinity, 0} & /@ vars)
            ] // Normal
        )
    ];

    (*
      Null relation elimination.
      Compute priority ordering (lowest multiplicity first) and 
      eliminate all null relations recursively.
    *)
    denomOrdering = {
        GetBestDenominatorToReplace[keepFrac, vars],
        GetBareDenoms[keepFrac, vars]
    };

    eliminated = EliminateNullRelations[
        keepForDivision * keepFrac, vars, denomOrdering, Unique[dummyD]
    ];

    (* Catch failure from EliminateNullRelations. *)
    If[eliminated === $Failed,
        Message[mathematicaPartialFraction::eliminationFailed];
        Return[$Failed]
    ];

    (*
      Expand variable-dependent numerators.
      
      After null relation elimination, some terms may have numerators 
      that depend on the variables (e.g., x^2 / ((x-y)^2 y (x+y-1))).
      
      We expand these by recursively transforming through all bases.
      In each basis's denominator space (w-coordinates), w-monomials 
      in the numerator cancel against w_i poles.
      
      This must happen BEFORE basis decomposition to avoid 
      double-counting across bases.
    *)
    allBases = FindBases[GetBareDenoms[keepFrac, vars], vars];
    eliminated = ExpandNumeratorInDenomSpace[eliminated, vars, allBases];


    (*
      Separate already-decomposed terms (fewer than n denominators) 
      from those needing basis decomposition (n or more denominators).
    *)
    terms = If[Head[eliminated] === Plus, List @@ eliminated, {eliminated}];
	
    decomposedTerms = Select[terms, 
        (CountBareDenoms[#, vars] < Length[vars]) &
    ];

    remainingTerms = Select[terms, 
        (CountBareDenoms[#, vars] >= Length[vars]) &
    ];
    
    (* If everything is already decomposed, return early. *)
    If[remainingTerms === {},
        Return[
            coeff * ignoreFrac * (Plus @@ decomposedTerms) + tmpPolynomialPart
        ]
    ];
    
    (*
      Find all n-element bases for each remaining term.
      
      Some terms may have denominators that don't form a valid basis 
      (singular coefficient matrix). These terms cannot be decomposed 
      further and should be returned as-is.
    *)
    bareDenoms = MapIndexed[{First[#2], GetBareDenoms[#1, vars]} &, remainingTerms];
    bases = Map[{#[[1]], FindBases[#[[2]], vars]} &, bareDenoms];
    
    (* Separate terms with valid bases from those without *)
    termsWithoutBases = Select[
        MapThread[{#1, #2[[2]]} &, {remainingTerms, bases}],
        #[[2]] === {} &
    ][[All, 1]];
    
    termsWithBases = Select[
        MapThread[{#1, #2[[2]], #2[[1]]} &, {remainingTerms, bases}],
        #[[2]] =!= {} &
    ];
    
    (* Add terms without valid bases to decomposedTerms *)
    decomposedTerms = Join[decomposedTerms, termsWithoutBases];
    
    (* Update remainingTerms and bases to only include processable terms *)
    If[termsWithBases === {},
        (* No terms have valid bases, return everything as decomposed *)
        Return[
            coeff * ignoreFrac * (Plus @@ decomposedTerms) + tmpPolynomialPart
        ]
    ];
    
    remainingTerms = termsWithBases[[All, 1]];
    bases = MapIndexed[{First[#2], #1[[2]]} &, termsWithBases];

    (*
      Bookkeeping: group terms that share a common basis.
      This allows computing residues more efficiently by processing 
      all terms contributing to a basis together.
    *)

    (* Flatten to pairs {termIndex, basis}. *)
    basisData = Table[
        {bases[[i, 1]], bases[[i, 2, j]]},
        {i, 1, Length[bases]},
        {j, 1, Length[bases[[i, 2]]]}
    ] // Flatten[#, 1] &;

    (* Group by basis. *)
    basisData = GatherBy[basisData, Last];

    (* Clean up: {list of term indices, basis}. *)
    basisData = Map[{Flatten[#[[All, 1]], 1], #[[1, 2]]} &, basisData];

    (* Sum terms sharing each basis. *)
    basisData = Map[{Plus @@ remainingTerms[[#[[1]]]], #[[2]]} &, basisData];

    (*
      Compute residue contributions.
      
      coeff * ignoreFrac is placed outside for efficiency, matching 
      the single-variable pattern.
      
      Total is used instead of Plus @@ for packed-array efficiency.
    *)
    tmpFractionedPart = If[OptionValue["Parallel"][[1]],

        (* Parallel: distribute basis contributions across cores. *)
        ComputeParallel[
            basisData // RandomSample,
            (coeff * ignoreFrac * ResidueForBasis[#[[1]], #[[2]], vars]) &,
            OptionValue["Parallel"][[2]],
            OptionValue["Parallel"][[3]]
        ] // Total,

        (* Sequential: compute each basis contribution. *)
        Table[
            coeff * ignoreFrac * ResidueForBasis[
                basisData[[i, 1]],
                basisData[[i, 2]],
                vars
            ],
            {i, 1, Length[basisData]}
        ] // Total
    ];

    (* Assemble final result. *)
    coeff * ignoreFrac * (Plus @@ decomposedTerms) + 
        tmpFractionedPart + 
        tmpPolynomialPart
]


mathematicaPartialFraction::eliminationFailed = 
    "Null relation elimination failed. The expression may contain affine relations or unsupported denominator structures.";


(* ::Subsection::Closed:: *)
(*ResidueForLaurentSeries*)


ClearAll[ResidueForLaurentSeries]

	(*
	Notes:
		-I had to put the condition inside the rule, cuz' somehow it did not work at the end.
		-I have to add everything up here otherwise I must use Flatten and that takes a lot of time, thus
		we lose the advantage of parallel computation.
	*)
ResidueForLaurentSeries[coeff_, list_List/;ArrayDepth[list]>1]:=(list//Map[(coeff*ResidueForLaurentSeries@@#)&,#]&)
ResidueForLaurentSeries[coeff_, list_List/;ArrayDepth[list]==1]:=coeff*ResidueForLaurentSeries@@list

ResidueForLaurentSeries[
	restOfFunction_, pole_,
	multiplicity_Integer,
	poleOrder_Integer,
	{1,constant_List}, var_
		]:=
	Module[
		{
		barePole=If[Head[pole]===Power, pole/.Power[(a_.+b_. var), power_Integer]:>a+b var, pole]
		},
		
			1/barePole^poleOrder (1/(multiplicity-poleOrder)!*
				D[restOfFunction,{var,multiplicity-poleOrder}]/.var->-constant[[1]])
				
	]
	
ResidueForLaurentSeries[
	restOfFunction_, pole_,
	multiplicity_Integer,
	poleOrder_Integer,
	{orderOfPolynomial_Integer,listOfConstans_List}, var_
		]:=
	Module[
		{
		tmp,tmpTiming,tmpRules,tmpR,tmpS,power,
		dens,simplifiedDens,ruleDens,
		order,tmpRuleReduction,ruleReduction,
		barePole=If[Head[pole]===Power, pole/.Power[expr_,power_Integer]/;!FreeQ[expr,var]:>expr, pole],
		
		tmpNum,tmpNumRules,tmpDen
		},
		
			
		(*
		Final Sum for the residue, we must go through every order starting form 1 to the pole's multiplicity.
		
			1. We derivate the R[x]/S[x] rational polynomial. R[x] is the rest of the function, S[x] is the denominator
			without the pole (x-a[i]).
			
			2. We simplfy the expression with the PolynomialGCD method.
			
			3. We express the remaning expressions in terms of the original coefficients of the 
			bare root.
			
		*)
					
			tmp=1/(multiplicity-poleOrder)! *
					D[tmpR[var]/tmpS[var]^multiplicity,{var,multiplicity-poleOrder}];
			
			tmp=tmp/.tmpR[var]:>restOfFunction;
			tmp=tmp/.D[tmpR[var],{var,power_}]:>D[restOfFunction,{var,power}];
			
			tmp=tmp/.tmpS[var]:>D[barePole,{var,1}];
			tmp=tmp/.D[tmpS[var],{var,power_}]:>D[barePole,{var,power+1}]/(power+1); (*S can be expressed as the derivative of the bare pole.*)
			
			
			tmp=tmp//Together;
			tmp=Collect[Numerator[tmp],var]/Denominator[tmp];
			
				(*Major bottleneck!!*)
			tmp=tmp//ReducePolynomialForResidue[#,barePole,var]&;
			tmp=tmp//CoefficientList[#,var]&;
			
			
			Sum[
					tmp[[powL+1]]
					Sum[
						Binomial[powL, i]*
							(
								var^i (-1)^(powL-i)*
								LinApartU[-poleOrder+powL-i, 
											var,
											barePole,
											{orderOfPolynomial, Reverse[listOfConstans]}
									]
							)//Apart[#,var]&
							
						,{i,0,powL}
					]
					
				,{powL,0,Length[tmp]-1}
			]
		
	]


(* ::Subsection::Closed:: *)
(*ResidueForBasis*)


(*
  GetDataForResidue[coeff, expr, basis, vars, varsInW]

  Extracts the data needed to compute the residue contribution of a 
  single term at a given basis.

  For a term like coeff * (numerator) / (D\:2081^m\:2081 D\:2082^m\:2082 ... D\:2096^m\:2096), this 
  function separates:
    - The coefficient (variable-free part)
    - The basis denominators and their multiplicities
    - The non-basis denominators, expressed in w-space as a "generating function"

  The generating function encodes the non-basis denominators in the 
  coordinate system defined by the basis. Its derivatives (evaluated 
  at w=0) give the residue coefficients.

  Parameters:
    coeff   - Coefficient from the original expression (passed through).
    expr    - A single term (monomial in denominators) to analyze.
    basis   - The n-element basis (list of n linear forms).
    vars    - The n original variables.
    varsInW - Rules {x -> f(w\:2081,...,w\:2099), y -> g(w\:2081,...,w\:2099), ...} expressing 
              original variables in terms of the w-coordinates (denominator space).

  Returns:
    {powersBasis, totalCoeff, generatingFunction} where:
      - powersBasis: list of multiplicities {m\:2081, m\:2082, ..., m\:2099} for the 
        basis denominators in this term.
      - totalCoeff: the complete coefficient (original coeff times any 
        constants introduced during null relation elimination).
      - generatingFunction: the non-basis denominators expressed in 
        w-space as 1/Product[wLinearCombo^power].
    
    Returns Nothing if the term does not contain all basis elements 
    (contributes zero to this basis's residue).

  Notes:
    - Null relation elimination can introduce additional constants, so 
      we re-separate dependent/independent parts.
    - A term contributes to a basis only if it contains ALL basis 
      denominators. Missing any one means the residue is zero.
*)



ClearAll[GetDataForResidue]



GetDataForResidue[args___] := Null /; !CheckArguments[GetDataForResidue[args], 5]

GetDataForResidue[coeff_, expr_, basis_, vars_, varsInW_] := (
    Message[GetDataForResidue::notList, basis, 3];
    $Failed
) /; !ListQ[basis]

GetDataForResidue[coeff_, expr_, basis_, vars_, varsInW_] := (
    Message[GetDataForResidue::notList, vars, 4];
    $Failed
) /; !ListQ[vars]

GetDataForResidue[coeff_, expr_, basis_, vars_, varsInW_] := (
    Message[GetDataForResidue::notList, varsInW, 5];
    $Failed
) /; !ListQ[varsInW]

GetDataForResidue[coeff_, expr_, basis_List, vars_List, varsInW_List] := Module[
    {
        tmp,
        tmpCoeff, dependentPart,
        dataDenoms, dataBasis, dataNonBasis, powersBasis,
        generatingFunction
    },

    (*
      Null relation elimination can introduce additional constants.
      Re-separate to capture any new coefficient factors.
    *)
    {tmpCoeff, dependentPart} = SeparateDependency[expr, Alternatives @@ vars];
    tmpCoeff = coeff * tmpCoeff;
    
    (* Extract all denominators with their multiplicities. *)
    dataDenoms = GetDenomData[dependentPart, vars];

    (* Partition into basis and non-basis denominators. *)
    dataBasis = Select[dataDenoms, MemberQ[basis, #[[1]]] &];
    dataNonBasis = Select[dataDenoms, !MemberQ[basis, #[[1]]] &];

    (* Extract multiplicities of basis denominators. *)
    powersBasis = dataBasis[[All, 2]];

    (*
      If any basis denominator is missing (multiplicity 0), this term 
      does not contribute to this basis's residue. Return Nothing to 
      filter it out automatically.
    *)
    If[Length[powersBasis] < Length[basis] || MemberQ[powersBasis, 0], 
        Return[Nothing]
    ];

    (*
      Build the generating function in w-space.
      
      Non-basis denominators are expressed in terms of w-variables 
      using varsInW. The generating function is:
        1 / Product[(non-basis denom in w-space)^power]
      
      Derivatives of this function (evaluated at w=0) give the 
      coefficients needed for the residue formula.
    *)
    tmp = dataNonBasis /. varsInW;
    generatingFunction = 1 / Times @@ (Power @@@ tmp);

    {powersBasis, tmpCoeff, generatingFunction}
]



GetDataForResidue::notList = "Argument `1` at position `2` must be a list.";


(*
  CalculateResidueInDenominatorSpace[groups, basis, wVars]

  Computes the residue contribution from a group of terms sharing the 
  same basis, using the multivariate residue formula in w-space.

  The multivariate residue at w=0 for a function f(w)/w\:2081^m\:2081...w\:2099^m\:2099 is:
    Sum over all (j\:2081,...,j\:2099) with 1 \[LessEqual] j\:1d62 \[LessEqual] m\:1d62 of:
      (1/\[Product](m\:1d62-j\:1d62)!) * [\[PartialD]^(m\:2081-j\:2081)...\[PartialD]^(m\:2099-j\:2099) f](0) / w\:2081^j\:2081...w\:2099^j\:2099

  This function:
    1. Sums the generating functions from all terms in the group
    2. Enumerates all combinations of basis powers (j\:2081,...,j\:2099)
    3. Computes the corresponding derivatives and evaluates at w=0
    4. Assembles the final partial fraction contribution

  Parameters:
    groups - List of {powersBasis, coeff, generatingFunction} records 
             from GetDataForResidue, all sharing the same powersBasis.
    basis  - The n-element basis (list of n linear forms).
    wVars  - The n w-variables representing the basis in denominator space.

  Returns:
    The partial fraction contribution from this basis, expressed as a 
    sum of terms coeff / (D\:2081^j\:2081 D\:2082^j\:2082 ... D\:2099^j\:2099).

  Notes:
    - All terms in groups have the same multiplicities (powersBasis), 
      which is why they were grouped together.
    - Summing generating functions before differentiating is more 
      efficient than differentiating each separately.
*)



ClearAll[CalculateResidueInDenominatorSpace]



CalculateResidueInDenominatorSpace[args___] := Null /; !CheckArguments[CalculateResidueInDenominatorSpace[args], 3]

CalculateResidueInDenominatorSpace[groups_, basis_, wVars_] := (
    Message[CalculateResidueInDenominatorSpace::notList, groups, 1];
    $Failed
) /; !ListQ[groups]

CalculateResidueInDenominatorSpace[groups_, basis_, wVars_] := (
    Message[CalculateResidueInDenominatorSpace::notList, basis, 2];
    $Failed
) /; !ListQ[basis]

CalculateResidueInDenominatorSpace[groups_, basis_, wVars_] := (
    Message[CalculateResidueInDenominatorSpace::notList, wVars, 3];
    $Failed
) /; !ListQ[wVars]

CalculateResidueInDenominatorSpace[groups_List, basis_List, wVars_List] := Module[
    {
        multiplicities,
        fullGeneratingFunction,
        bPowers, mPowers,
        residues
    },

    (* 
      All terms in this group share the same basis multiplicities.
      Extract from the first record.
    *)
    multiplicities = groups[[1, 1]];

    (*
      Sum the individual generating functions (weighted by their coefficients) 
      to form the complete generating function for this group.
      
      This is the key efficiency gain: differentiate once on the sum, 
      rather than differentiating each term separately.
    *)
    fullGeneratingFunction = groups // Map[(#[[2]] * #[[3]]) &] // Total;

    (*
      Enumerate all possible final denominator powers.
      
      For multiplicities {m\:2081, m\:2082, ..., m\:2099}, the final answer has terms 
      with powers {j\:2081, j\:2082, ..., j\:2099} where 1 \[LessEqual] j\:1d62 \[LessEqual] m\:1d62.
      
      bPowers = all such combinations (Tuples of Range[1,m\:1d62]).
    *)
    bPowers = Tuples[Range[1, #] & /@ multiplicities];

    (*
      Compute derivative orders for each combination.
      
      The residue formula involves \[PartialD]^(m\:1d62-j\:1d62) with respect to w\:1d62.
      mPowers = {m\:2081-j\:2081, m\:2082-j\:2082, ...} for each combination.
    *)
    mPowers = (multiplicities - #) & /@ bPowers;

    (*
      Calculate residue coefficients using the multivariate residue formula:
        coeff = (1/\[Product](m\:1d62-j\:1d62)!) * [\[PartialD]^(m-j) f](0)
      
      where f is the generating function and m-j are the derivative orders.
    *)
    residues = Table[
        (1 / Times @@ (m!)) *
        (D[fullGeneratingFunction, Sequence @@ MapThread[{#1, #2} &, {wVars, m}]] /. 
            Thread[wVars -> 0]),
        {m, mPowers}
    ];

    (*
      Assemble the final contribution:
        Sum of residue[j] / (D\:2081^j\:2081 D\:2082^j\:2082 ... D\:2099^j\:2099)
    *)
    Plus @@ (residues / Times @@@ Map[Power[basis, #] &, bPowers])
]



CalculateResidueInDenominatorSpace::notList = "Argument `1` at position `2` must be a list.";


(*
  ResidueForBasis[restOfFunction, basisDenoms, multiplicities, vars]

  Computes the partial fraction contribution from a single basis in 
  multivariate partial fraction decomposition.

  This is the multivariate analogue of ResidueForLaurentSeries. While 
  the single-variable version computes residues at poles, this computes 
  residues at the intersection point defined by a basis of linear forms.

  Parameters:
    restOfFunction - The numerator part (expression after factoring out 
                     the basis denominators), analogous to single-variable.
    basisDenoms    - List of n linearly independent linear denominators 
                     defining the basis.
    multiplicities - List of multiplicities {m\:2081, m\:2082, ..., m\:2099} for each 
                     basis denominator.
    vars           - List of n variables.

  Returns:
    The partial fraction contribution from this basis, as a sum of 
    terms with only the basis denominators (each raised to various powers).

  Algorithm:
    1. Create n w-variables (one per basis element).
    2. Solve for original variables in terms of w's: coordinate 
       transformation to denominator space.
    3. For each term in the expression, extract {multiplicities, coeff, 
       generatingFunction} via GetDataForResidue.
    4. Group terms by multiplicities.
    5. For each group, compute residues using CalculateResidueInDenominatorSpace.
    6. Sum all contributions.

  Notes:
    - The w-coordinates are created fresh using Unique["w"] to avoid conflicts.
    - Grouping terms by multiplicities before differentiation significantly 
      improves efficiency for expressions with many terms.
*)



ClearAll[ResidueForBasis]

ResidueForBasis[args___] := Null /; !CheckArguments[ResidueForBasis[args], 3]

ResidueForBasis[restOfFunction_, basisDenoms_, vars_] := (
    Message[ResidueForBasis::notList, basisDenoms, 2];
    $Failed
) /; !ListQ[basisDenoms]

ResidueForBasis[restOfFunction_, basisDenoms_, vars_] := (
    Message[ResidueForBasis::notList, vars, 3];
    $Failed
) /; !ListQ[vars]

ResidueForBasis[restOfFunction_, basisDenoms_, vars_] := (
    Message[ResidueForBasis::lengthMismatch, Length[basisDenoms], Length[vars]];
    $Failed
) /; Length[basisDenoms] =!= Length[vars]

ResidueForBasis[restOfFunction_, basisDenoms_List, vars_List] := Module[
    {
        lengthVars,
        wVars, varsInW,
        terms, records, groups
    },

    lengthVars = Length[vars];

    wVars = Table[Unique["w"], {lengthVars}];

    varsInW = MapThread[
        Rule, 
        {vars, vars /. First@Solve[Thread[wVars == basisDenoms], vars]}
    ];

    terms = If[Head[restOfFunction] === Plus, List @@ restOfFunction, {restOfFunction}];

    records = GetDataForResidue[1, #, basisDenoms, vars, varsInW] & /@ terms;

    If[records === {}, Return[0]];

    groups = GatherBy[records, First];
    groups=groups // Map[CalculateResidueInDenominatorSpace[#, basisDenoms, wVars] &] // Total;
	groups
]

ResidueForBasis::notList = "Argument `1` at position `2` must be a list.";
ResidueForBasis::lengthMismatch = "Length `1` does not match number of variables `2`.";
