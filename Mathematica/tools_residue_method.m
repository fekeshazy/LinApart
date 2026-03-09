(* ::Package:: *)

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
