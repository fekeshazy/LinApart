(* ::Package:: *)

(* ::Subsection:: *)
(*Partial fraction function*)


(* ::Subsubsection:: *)
(*Comment*)


(*
  mathematicaPartialFraction[coeff, ignoreFrac, keepForDivision, keepFrac, var/vars, options]

  Core partial fraction decomposition function.

  Supported methods:

  Single-variable:
    1. "ExtendedLaurentSeries"
       Computes residues at each pole using derivatives and the Laurent-series formula.
    2. "Euclidean"
       Uses polynomial extended GCD identities to iteratively reduce pairs of denominators.
    3. "EquationSystem"
       Delegates to Mathematica's Apart.

  Multivariate:
    4. "MultivariateResidue"
       Eliminates affine/null relations among linear denominators and computes
       basis residues.
    5. "Leinartas"
       Uses polynomial syzygies to perform recursive multivariate decomposition.
    6. "Groebner"
       Uses a Gr\[ODoubleDot]bner-basis reduction in q-space, with optional iterated
       q-factor insertion.

  Parameters:
    coeff           - Variable-free coefficient from preprocessing.
    ignoreFrac      - Factors with unsupported powers, factored out by SeparateFrac.
    keepForDivision - Numerator factors for improper fraction handling.
                      If this contains the variable(s), a polynomial part is
                      extracted via Series at infinity.
    keepFrac        - The proper fraction to decompose.
    var             - Single variable (Symbol) or list of variables (List).
    options         - Options inherited from LinApart.

  Returns:
    The partial fraction decomposition of
      coeff * ignoreFrac * keepForDivision * keepFrac.

  Notes:
    - coeff * ignoreFrac is often kept outside the internal decomposition
      step for efficiency.
    - The multivariate Gr\[ODoubleDot]bner method supports the options
        "IterativeGroebner"
        "GroebnerParameterSymbolization"
      to control the q-space reduction strategy and coefficient handling.
*)

ClearAll[mathematicaPartialFraction]

Options[mathematicaPartialFraction] = $LinApartOptions;

mathematicaPartialFraction[args___] := Null /; !CheckArguments[mathematicaPartialFraction[args], {5,6}]


(* ::Subsubsection::Closed:: *)
(*Single-variable*)


		(* ============================================== *)
		(*             Extended Laurent Series            *)
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
		(*                Euclidean Method                *)
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


(* ::Subsubsection:: *)
(*Multi-variable*)


		(* ============================================== *)
		(*         Multivariate: Leinartas Method         *)
		(* ============================================== *)

	(*
	  Method "Leinartas":
	    1. Extract the polynomial part at infinity, if present.
	    2. Decompose the remaining rational part by computing syzygies once.
	    3. Separate syzygies into inhomogeneous and homogeneous types.
	    4. Eliminate inhomogeneous syzygies recursively.
	    5. Eliminate homogeneous syzygies recursively.
	    6. Recurse until the expression is reduced.
	    7. Reattach coeff * ignoreFrac and add the polynomial part.

	  Notes:
	    - The recursive syzygy-elimination core is kept identical in spirit
	      to the prototype wrapper.
	    - coeff * ignoreFrac is placed outside the recursive decomposition
	      for consistency with the other methods.
	*)

mathematicaPartialFraction[
    coeff_, ignoreFrac_, keepForDivision_, keepFrac_, vars_List,
    options:OptionsPattern[]
] := Module[
    {
        varPat,
        tmpPolynomialPart,
        rationalPart,
        LeinartasCore
    },

    varPat = Alternatives @@ vars;

    (*
      Extract polynomial part for improper fractions.
      As in the production multivariate branch, Series is applied
      sequentially in each variable at infinity.
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
      Remove the polynomial part before entering the recursive syzygy
      elimination to avoid counting it twice.
    *)
    rationalPart = If[FreeQ[keepForDivision, varPat],
        keepForDivision * keepFrac,
        Together[
            keepForDivision * keepFrac -
            Series[
                keepForDivision * keepFrac,
                Sequence @@ ({#, Infinity, 0} & /@ vars)
            ] // Normal
        ]
    ];

    (*
      Local recursive core implementing the prototype Leinartas wrapper.
    *)
    LeinartasCore[expr_Plus] := Map[LeinartasCore, expr];

    LeinartasCore[expr_] := expr /; FreeQ[expr, varPat];

    LeinartasCore[expr_] := expr /; PolynomialQ[expr, vars];

    LeinartasCore[expr_] := Module[
        {
            tmp,
            coeffLocal, dependentPart,
            denoms, denomOrdering,
            allSyzygies, inhomogeneous, homogeneous,
            result
        },

        (* Separate variable-free coefficient. *)
        {coeffLocal, dependentPart} = SeparateDependency[expr, Alternatives @@ vars];

        (* No denominator left to decompose. *)
        If[Denominator[dependentPart] === 1,
            Return[coeffLocal * dependentPart]
        ];
        
        If[CountBareDenoms[dependentPart, vars] <= 1,
		    Return[coeffLocal * dependentPart]
		];

        (* Get current bare denominators. *)
        denoms = GetBareDenoms[dependentPart, vars];

        If[denoms === $Failed,
            Message[mathematicaPartialFraction::leinartasDenomFailed, dependentPart];
            Return[$Failed]
        ];

        (* Already reduced. *)
        If[Length[denoms] <= Length[vars],
            Return[coeffLocal * dependentPart]
        ];

        (* Compute syzygies once for the current denominator set. *)
        allSyzygies = FindSyzygies[denoms, vars];

        If[allSyzygies === $Failed,
            Message[mathematicaPartialFraction::leinartasSyzygyFailed, denoms];
            Return[$Failed]
        ];

        (* Separate into inhomogeneous and homogeneous syzygies. *)
        tmp = SeparateSyzygiesByType[allSyzygies];

        If[tmp === $Failed,
            Message[mathematicaPartialFraction::leinartasTypeFailed, allSyzygies];
            Return[$Failed]
        ];

        {inhomogeneous, homogeneous} = tmp;

        (* Stage 1: Eliminate inhomogeneous syzygies. *)
        tmp = EliminateInhomogeneousSyzygies[
            dependentPart,
            vars,
            inhomogeneous,
            denoms,
            Unique[dummyD]
        ];

        If[tmp === $Failed,
            Message[mathematicaPartialFraction::leinartasInhomogeneousFailed, dependentPart];
            Return[$Failed]
        ];

        (* Stage 2: Eliminate homogeneous syzygies. *)
        denomOrdering = {GetBestDenominatorToReplace[dependentPart, vars], denoms};

        result = EliminateHomogeneousSyzygies[
            tmp,
            vars,
            homogeneous,
            denoms,
            denomOrdering,
            Unique[dummyD]
        ];

        If[result === $Failed,
            Message[mathematicaPartialFraction::leinartasHomogeneousFailed, tmp];
            Return[$Failed]
        ];

        (* Recurse exactly as in the prototype wrapper. *)
        coeffLocal * (LeinartasCore[result] // Expand)
    ];

    coeff * ignoreFrac * LeinartasCore[rationalPart] + tmpPolynomialPart

] /; OptionValue["Method"] === "Leinartas"

mathematicaPartialFraction::leinartasDenomFailed =
    "Leinartas decomposition failed while extracting denominators from expression `1`.";
mathematicaPartialFraction::leinartasSyzygyFailed =
    "Leinartas decomposition failed while computing syzygies for denominator set `1`.";
mathematicaPartialFraction::leinartasTypeFailed =
    "Leinartas decomposition failed while separating syzygies by type for `1`.";
mathematicaPartialFraction::leinartasInhomogeneousFailed =
    "Leinartas decomposition failed during inhomogeneous syzygy elimination for expression `1`.";
mathematicaPartialFraction::leinartasHomogeneousFailed =
    "Leinartas decomposition failed during homogeneous syzygy elimination for expression `1`.";


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
]/; OptionValue["Method"] === "MultivariateResidue"

mathematicaPartialFraction::eliminationFailed = 
    "Null relation elimination failed. The expression may contain affine relations or unsupported denominator structures.";


		(* ============================================== *)
		(*           Multivariate: Groebner Method        *)
		(* ============================================== *)

(*
  Method "Groebner":
    1. Extract the polynomial part at infinity, if present.
    2. Expand the remaining rational part into additive terms.
    3. Terms with at most Length[vars] variable-dependent denominator
       factors are already minimal and are kept unchanged.
    4. For terms with more than Length[vars] denominator factors:
         - build a local denominator set,
         - optionally make non-polynomial parameter sub-expressions symbolic,
         - clear remaining parameter denominators in the denominator set,
         - build the local Gr\[ODoubleDot]bner-apart ideal,
         - compute a local Gr\[ODoubleDot]bner basis,
         - convert the term into q-space,
         - reduce it either
             * iteratively, or
             * in one shot,
         - substitute back from q-space,
         - restore the original parameter expressions.
    5. Sum all reduced terms and add the polynomial part.

  Options:
    "IterativeGroebner" -> True|False
      True  : multiply q-factors in one at a time and reduce after each step
      False : reduce the full q-space expression in one shot

    "GroebnerParameterSymbolization" -> True|False
      True  : replace non-polynomial parameter-dependent coefficient pieces
              by temporary symbols before Gr\[ODoubleDot]bner-basis computations
      False : leave coefficient expressions unchanged

  Notes:
    - coeff * ignoreFrac is kept outside the Gr\[ODoubleDot]bner-based termwise reduction
      for consistency with the other methods.
    - Denominator sets and Gr\[ODoubleDot]bner bases are constructed locally for each term.
*)

mathematicaPartialFraction[
    coeff_, ignoreFrac_, keepForDivision_, keepFrac_, vars_List,
    options:OptionsPattern[]
] := Module[
    {
        varPat,
        tmpSeriesPart = 0,
        tmpPolynomialPart,
        rationalPart,
        terms
    },

    varPat = Alternatives @@ vars;

    If[!FreeQ[keepForDivision, varPat],
        tmpSeriesPart = Series[
            keepForDivision * keepFrac,
            Sequence @@ ({#, Infinity, 0} & /@ vars)
        ] // Normal
    ];

    tmpPolynomialPart = coeff * ignoreFrac * tmpSeriesPart;

    rationalPart = If[tmpSeriesPart === 0,
        keepForDivision * keepFrac,
        Together[keepForDivision * keepFrac - tmpSeriesPart]
    ];

    If[rationalPart === 0,
        Return[tmpPolynomialPart, Module]
    ];

    If[CountBareDenoms[rationalPart, vars] <= Length[vars],
        Return[coeff * ignoreFrac * rationalPart + tmpPolynomialPart, Module]
    ];

    terms = Expand[rationalPart];
    terms = If[Head[terms] === Plus, List @@ terms, {terms}];

    terms = GroebnerApart[#, vars, OptionValue["IterativeGroebner"],OptionValue["GroebnerParameterSymbolization"]] & /@ terms;

    If[MemberQ[terms, $Failed],
        Return[$Failed, Module]
    ];

    coeff * ignoreFrac * (Plus @@ terms // Expand[#, varPat] &) + tmpPolynomialPart

] /; OptionValue["Method"] === "Groebner"
