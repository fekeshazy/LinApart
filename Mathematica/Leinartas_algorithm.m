(* ::Package:: *)

(* ::Section::Closed:: *)
(*FindSyzygies*)


(*
  FindSyzygies[denoms, vars]

  Finds all polynomial syzygies (both homogeneous and inhomogeneous) 
  among a list of polynomial denominators using Gr\[ODoubleDot]bner basis.

  A syzygy is a polynomial relation among denominators:
    p(D\:2081, D\:2082, ..., D\:2096) = c
  
  Returns the relation as CoefficientArrays to maintain clear mapping
  between coefficients and denominators.

  Parameters:
    denoms - List of polynomial denominators
    vars   - List of variables

  Returns:
    List of {coefficientArrays, constant} pairs where:
      - coefficientArrays: output of CoefficientArrays[relationPoly, dummyVars]
                          gives coefficients indexed by powers of dummyD[1],...,dummyD[k]
      - constant: the constant term c
    
    For homogeneous: constant = 0
    For inhomogeneous: constant \[NotEqual] 0

  The relation can be reconstructed:
    ReconstructRelation[coefficientArrays, dummyVars] + constant = 0
    where dummyVars = {dummyD[1], ..., dummyD[k]}

  Examples:
    FindSyzygies[{x + y, x - y, 2*x}, {x, y}]
      -> Relation: dummyD[1] + dummyD[2] - 2*dummyD[3] = 0
    
    FindSyzygies[{x^2 - 1, x + 1, x - 1}, {x}]
      -> Relation: dummyD[1] - dummyD[2]*dummyD[3] = 0

  Notes:
    - Automatically verifies each syzygy by substituting back
    - Uses EliminationOrder and RationalFunctions for generality
*)

ClearAll[FindSyzygies]

FindSyzygies[args___] := Null /; !CheckArguments[FindSyzygies[args], {2,3}]

FindSyzygies[denoms_, vars_] := (
    Message[FindSyzygies::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

FindSyzygies[denoms_, vars_] := (
    Message[FindSyzygies::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

FindSyzygies[denoms_List, vars_List] := (
    Message[FindSyzygies::tooFewDenoms];
    $Failed
) /; Length[denoms] < 2

FindSyzygies[denoms_List, vars_List] := 
Module[
	{
		k = Length[denoms],
		dummyD, dummyVars,
		allVars, idealGenerators,
		groebnerBasis,
		relations, syzygies,
		constantTerm, coeffArrays, relationPoly
	},
	
	(* Create dummy variables for denominators. *)
	dummyVars = Table[dummyD[i], {i, k}];
	
	(* All variables: original vars plus dummy variables. *)
	allVars = Join[vars, dummyVars];
	
	(* Ideal generators: Di - dummyD[i]. *)
	idealGenerators = Table[denoms[[i]] - dummyVars[[i]], {i, k}];
	
	(*
	  Compute Gr\[ODoubleDot]bner basis with elimination ordering.
	  Eliminates vars first, keeping relations among dummyD[i].
	*)
	groebnerBasis = GroebnerBasis[
						        idealGenerators,
						        allVars,
						        MonomialOrder -> EliminationOrder,
						        CoefficientDomain -> RationalFunctions
						    ];
	
	(*Extract relations: GB elements free of original variables.*)
	relations = Select[groebnerBasis, FreeQ[#, Alternatives @@ vars] &];
	
	(*For each relation, convert to CoefficientArrays format.*)
	syzygies = relations//Map[(CoefficientArrays[#,dummyVars]//Normal)&,#]&;
	
	syzygies

]

(*
  ReconstructRelation[coeffArrays, vars]
  
  Reconstructs a polynomial from CoefficientArrays output.
  
  coeffArrays = {constant, linear, quadratic, ...} from CoefficientArrays
  vars = list of variables
  
  Returns: polynomial such that CoefficientArrays[result, vars] == coeffArrays
*)

ClearAll[ReconstructRelation]

ReconstructRelation[coeffArrays_List, vars_List]:=Sum[coeffArrays[[i]]//Nest[Dot[#,vars]&,#,i-1]& ,{i,1,Length[coeffArrays]}]

FindSyzygies::notList = "Argument `1` at position `2` must be a list.";
FindSyzygies::tooFewDenoms = "Need at least 2 denominators to find syzygies.";
FindSyzygies::verificationFailed = "Syzygy verification failed for relation `1`.";


(*
This was an attempt to try and reduce the run-time, 
but since in this case we calculate much more Gr\[ODoubleDot]bner-basis then needed, this attempt failed
*)

(*ClearAll[PrecomputeAllSyzygies];

PrecomputeAllSyzygies[denoms_List, vars_List] := Module[
    {
    dummyD,
    dummyVars,
    dummyVarRules,
    k = Length[denoms], 
    
    subsets, 
    allSyzygies
    },

    (* Create dummy variables for denominators. *)
    dummyVars = Table[dummyD[i], {i, k}];
    dummyVarRules = MapThread[Rule, {denoms,dummyVars}]//Association;
    
    (* Leinartas Theorem: Any n+1 denominators in n variables have a syzygy.
       We only need to check subsets of size 2 up to n+1. *)
    subsets = Subsets[denoms, {2, k}];
    
    allSyzygies= Map[ FindSyzygies[#, vars, dummyVarRules]& , subsets]//Flatten[#,1]&;

	    (*For each relation, convert to CoefficientArrays format.*)
    allSyzygies = allSyzygies//Map[(CoefficientArrays[#,dummyVars]//Normal)&,#]&;

    allSyzygies
]


FindSyzygies[denoms_List, vars_List, dummyVarsRules_Association] := Module[
    {
        k = Length[denoms],
        
        allVars, 
        tmpDummyVars,
        
        idealGenerators,
        groebnerBasis,
        
        relations, syzygies,
        constantTerm, coeffArrays, relationPoly
    },

	(*Get the relevant dummy vars for the reconstruction.*)
	tmpDummyVars=denoms//Lookup[dummyVarsRules,#]&;
	
    (* All variables: original vars plus dummy variables. *)
    allVars = Join[vars, tmpDummyVars];

    (* Ideal generators: Di - dummyD[i]. *)
    idealGenerators = Table[denoms[[i]] - tmpDummyVars[[i]], {i, k}];

    (*Compute Gr\[ODoubleDot]bner basis with elimination ordering. Eliminates vars first, keeping relations among dummyD[i].*)
    groebnerBasis = GroebnerBasis[
						        idealGenerators,
						        allVars,
						        MonomialOrder -> EliminationOrder,
						        CoefficientDomain -> RationalFunctions
						    ];

    (*Extract relations: GB elements free of original variables.*)
    relations = Select[groebnerBasis, FreeQ[#, Alternatives @@ vars] &];

	(*If there are no realtions, return Nothing so it will disappear from the list.*)
	If[relations==={}, Return[Nothing,Module]];

	relations
]*)


(* ::Section::Closed:: *)
(*SeparateSyzygiesByType*)


(*
  SeparateSyzygiesByType[syzygies]

  Separates syzygies into homogeneous and inhomogeneous types based on 
  their constant terms.

  - Homogeneous syzygy: \[CapitalSigma] h\:1d62 D\:1d62 = 0 (constant term is 0)
    These reduce the number of distinct denominators.
  
  - Inhomogeneous syzygy: \[CapitalSigma] h\:1d62 D\:1d62 = c \[NotEqual] 0 (constant term is nonzero)
    These reduce multiplicities of denominators.
    The one-operator is simply: 1 = (1/c) \[CapitalSigma] h\:1d62 D\:1d62

  Parameters:
    syzygies - Output from FindSyzygies: list of {coeffArrays, constant} pairs

  Returns:
    {inhomogeneousSyzygies, homogeneousSyzygies}
    where each is a list of {coeffArrays, constant} pairs.

  Examples:
    syzygies = {{coeffArrays1, 0}, {coeffArrays2, 5}, {coeffArrays3, 0}};
    SeparateSyzygiesByType[syzygies]
      -> {{coeffArrays2, 5}}, {{coeffArrays1, 0}, {coeffArrays3, 0}}}
    
    (* Inhomogeneous has priority for elimination *)

  Notes:
    - Inhomogeneous syzygies are returned first (higher priority)
    - Empty list returned if no syzygies of that type exist
*)

ClearAll[SeparateSyzygiesByType]

SeparateSyzygiesByType[args___] := Null /; !CheckArguments[SeparateSyzygiesByType[args], 1]

SeparateSyzygiesByType[syzygies_] := (
    Message[SeparateSyzygiesByType::notList, syzygies];
    $Failed
) /; !ListQ[syzygies]

SeparateSyzygiesByType[syzygies_List] := Module[
    {inhomogeneous, homogeneous},

    (* Inhomogeneous: constant term is nonzero *)
    inhomogeneous = Select[syzygies, #[[1]] =!= 0 &];

    (* Homogeneous: constant term is zero *)
    homogeneous = Select[syzygies, #[[1]] === 0 &];

    {inhomogeneous, homogeneous}
]

SeparateSyzygiesByType::notList = "Argument `1` must be a list.";


(* ::Section::Closed:: *)
(*FilterSyzygiesToCurrentDenoms*)


ClearAll[doesTheSyzygyContainTheDenoms]

doesTheSyzygyContainTheDenoms[syzygy_List, presentIndices_, originalDenoms_List]:=
Module[
	{
	dummyVars,
	syzygyPoly,
	participatingDenoms
	}
	,
        
            (* Reconstruct to find which denominators participate *)
            dummyVars = Table[Unique["d"], {Length[originalDenoms]}];
            syzygyPoly = ReconstructRelation[syzygy, dummyVars];
            
            (* Find which dummy variables appear (non-zero coefficients) *)
            participatingDenoms = Position[dummyVars, d_ /; !FreeQ[syzygyPoly, d]][[All, 1]];
            
            (* Check if all participating denominators are still present *)
            SubsetQ[presentIndices, participatingDenoms]
        ]


(*
  FilterSyzygiesToCurrentDenoms[allSyzygies, currentDenoms, originalDenoms]

  Filters a list of syzygies to keep only those that involve denominators
  currently present in the expression.

  During elimination, some denominators disappear. Syzygies that reference
  eliminated denominators are no longer applicable and should be filtered out.

  Parameters:
    allSyzygies    - List of {coeffArrays, constant} pairs computed on originalDenoms
    currentDenoms  - List of bare denominators currently present
    originalDenoms - List of bare denominators from initial expression

  Returns:
    Filtered list of syzygies involving only current denominators.

  Algorithm:
    1. Find which original denominators are still present
    2. For each syzygy, check if all non-zero entries correspond to present denominators
    3. Keep only syzygies where all participating denominators are current

  Notes:
    - Does NOT re-index - the dummy variable mapping handles that
    - Returns empty list if no valid syzygies remain
*)

ClearAll[FilterSyzygiesToCurrentDenoms]

FilterSyzygiesToCurrentDenoms[args___] := Null /; !CheckArguments[FilterSyzygiesToCurrentDenoms[args], 3]

FilterSyzygiesToCurrentDenoms[allSyzygies_, currentDenoms_, originalDenoms_] := (
    Message[FilterSyzygiesToCurrentDenoms::notList, allSyzygies, 1];
    $Failed
) /; !ListQ[allSyzygies]

FilterSyzygiesToCurrentDenoms[allSyzygies_, currentDenoms_, originalDenoms_] := (
    Message[FilterSyzygiesToCurrentDenoms::notList, currentDenoms, 2];
    $Failed
) /; !ListQ[currentDenoms]

FilterSyzygiesToCurrentDenoms[allSyzygies_, currentDenoms_, originalDenoms_] := (
    Message[FilterSyzygiesToCurrentDenoms::notList, originalDenoms, 3];
    $Failed
) /; !ListQ[originalDenoms]

FilterSyzygiesToCurrentDenoms[allSyzygies_List, currentDenoms_List, originalDenoms_List] := Module[
    {
        presentIndices,
        dummyVars, syzygyPoly, participatingDenoms
    },

    (* Find positions of current denominators in original list *)
    presentIndices = Flatten[currentDenoms /. PositionIndex[originalDenoms]];

    (* Filter syzygies: keep only those where all participating denoms are present *)
    Select[allSyzygies, doesTheSyzygyContainTheDenoms[#, presentIndices, originalDenoms]&]
]

FilterSyzygiesToCurrentDenoms::notList = "Argument `1` at position `2` must be a list.";


(* ::Section::Closed:: *)
(*EliminateHomogeneousSyzygies*)


(*
  DenomAppearsInSyzygy[syzygy, denomIndex, dummyVars]

  Checks whether the denominator with index denomIndex appears in the
  given syzygy.

  Parameters:
    syzygy     - A syzygy in CoefficientArrays format
    denomIndex - Integer index of the denominator to test
    dummyVars  - List of dummy variables corresponding to denominators

  Returns:
    True if dummyVars[[denomIndex]] appears in the reconstructed syzygy
    polynomial, False otherwise.
*)

ClearAll[DenomAppearsInSyzygy]

DenomAppearsInSyzygy[args___] := Null /; !CheckArguments[DenomAppearsInSyzygy[args], 3]

DenomAppearsInSyzygy[syzygy_, denomIndex_, dummyVars_] := (
    Message[DenomAppearsInSyzygy::notInteger, denomIndex];
    $Failed
) /; !IntegerQ[denomIndex]

DenomAppearsInSyzygy[syzygy_, denomIndex_, dummyVars_] := (
    Message[DenomAppearsInSyzygy::notList, dummyVars];
    $Failed
) /; !ListQ[dummyVars]

DenomAppearsInSyzygy[syzygy_, denomIndex_Integer, dummyVars_List] := (
    Message[DenomAppearsInSyzygy::badIndex, denomIndex, Length[dummyVars]];
    $Failed
) /; denomIndex < 1 || denomIndex > Length[dummyVars]

DenomAppearsInSyzygy[syzygy_, denomIndex_Integer, dummyVars_List] := Module[
    {
        syzygyPoly
    },

    syzygyPoly = ReconstructRelation[syzygy, dummyVars];

    !FreeQ[syzygyPoly, dummyVars[[denomIndex]]]
]

DenomAppearsInSyzygy::notInteger = "Second argument `1` must be an integer.";
DenomAppearsInSyzygy::notList = "Third argument `1` must be a list.";
DenomAppearsInSyzygy::badIndex = "Denominator index `1` is out of range for a dummy variable list of length `2`.";


(*
  DeleteNonPresentDenomFromOrderedDenoms[originalDenoms, denoms, bestDenom]

  Updates the denominator ordering when some denominators have been 
  eliminated during the reduction process.

  During recursive elimination, denominators can disappear. The priority 
  list (bestDenom) refers to positions in the original denominator list, 
  but we need positions in the current (reduced) denominator list.

  This function:
    1. Finds which original denominators are no longer present
    2. Removes their indices from the priority list
    3. Re-indexes to match current denominator positions

  Parameters:
    originalDenoms - The original list of bare denominators before elimination
    denoms         - The current list of bare denominators
    bestDenom      - The priority list (indices into originalDenoms)

  Returns:
    Updated priority list with indices into denoms
*)

ClearAll[DeleteNonPresentDenomFromOrderedDenoms]

DeleteNonPresentDenomFromOrderedDenoms[originalDenoms_List, denoms_List, bestDenom_List] := Module[
    {
        tmp, tmpBestDenom,
        indexOriginal, indexNew, indexingRule
    },

    (* Find denominators that are no longer present. *)
    tmp = Complement[originalDenoms, denoms];
    
    (* Get positions of missing denominators in original list. *)
    tmp = tmp /. PositionIndex[originalDenoms];
    tmp = Flatten[tmp];
    
    (* Remove missing denominators from priority list. *)
    tmpBestDenom = DeleteCases[bestDenom, Alternatives @@ tmp];

    (* Re-index to match current denominator positions. *)
    indexOriginal = PositionIndex[originalDenoms] // Normal;
    indexNew = PositionIndex[denoms] // Normal;
    indexingRule = indexNew /. indexOriginal /. {arg_} :> arg;

    tmpBestDenom = tmpBestDenom /. indexingRule;

    tmpBestDenom
]


(*
  EliminateHomogeneousSyzygies[expr, vars, allHomSyzygies, originalDenoms, denomOrdering, dummyD]

  Recursively eliminates homogeneous syzygies using a pre-computed list.

  Parameters:
    expr             - The expression to reduce
    vars             - List of variables
    allHomSyzygies   - Pre-computed list of ALL homogeneous syzygies
    originalDenoms   - Original bare denominators (for syzygy mapping)
    denomOrdering    - {priorityList, originalDenoms} for elimination order
    dummyD           - Symbol for denominator space mapping

  Returns:
    Expression with all homogeneous syzygies eliminated
*)

ClearAll[EliminateHomogeneousSyzygies]

EliminateHomogeneousSyzygies[args___] := Null /; !CheckArguments[EliminateHomogeneousSyzygies[args], 6]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::notList, allHomSyzygies, 3];
    $Failed
) /; !ListQ[allHomSyzygies]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::notList, originalDenoms, 4];
    $Failed
) /; !ListQ[originalDenoms]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::notList, denomOrdering, 5];
    $Failed
) /; !ListQ[denomOrdering]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::notSymbol, dummyD];
    $Failed
) /; !MatchQ[dummyD, _Symbol]

EliminateHomogeneousSyzygies[expr_, vars_, allHomSyzygies_, originalDenoms_, denomOrdering_, dummyD_] := (
    Message[EliminateHomogeneousSyzygies::badOrdering, denomOrdering];
    $Failed
) /; Length[denomOrdering] =!= 2

(* For sums: map over each additive term independently *)
EliminateHomogeneousSyzygies[expr_Plus, vars_List, allHomSyzygies_List, originalDenoms_List, denomOrdering_List, dummyD_Symbol] :=
    Map[EliminateHomogeneousSyzygies[#, vars, allHomSyzygies, originalDenoms, denomOrdering, dummyD] &, expr]

(* Base case: expression free of all variables *)
EliminateHomogeneousSyzygies[expr_, vars_List, allHomSyzygies_List, originalDenoms_List, denomOrdering_List, dummyD_Symbol] :=
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Main recursive definition *)
EliminateHomogeneousSyzygies[expr_, vars_List, allHomSyzygies_List, originalDenoms_List, denomOrdering_List, dummyD_Symbol] := Module[
    {
        tmp,
        k, denoms, dummyVars,
        relevantSyzygies,
        bestDenom = denomOrdering[[1]],
        
        denomMapping, reversedDenomMapping,
        mappedExpr,
        
        syzygy, syzygyPoly, chosenDenom, coefficient,
        replacementRule, oneOperator
    },

    (* Check if denominator is a single term (not Times) *)
    If[Head[Denominator[expr]] =!= Times,
        Return[expr]
    ];

    (* Get current bare denominators *)
    denoms = GetBareDenoms[expr, vars];
    k = Length[denoms];

    (* Base case: fewer denominators than variables + 1, nothing to reduce *)
    If[k <= Length[vars], Return[expr]];

    (* Filter syzygies to those relevant for current denominators *)
    relevantSyzygies = FilterSyzygiesToCurrentDenoms[allHomSyzygies, denoms, originalDenoms];

    (* Check for filtering failure *)
    If[relevantSyzygies === $Failed,
        Message[EliminateHomogeneousSyzygies::filterFailed];
        Return[$Failed]
    ];

    (* Base case: no relevant syzygies remain *)
    If[relevantSyzygies === {}, Return[expr]];

    (* Update denominator ordering *)
    bestDenom = DeleteNonPresentDenomFromOrderedDenoms[originalDenoms, denoms, bestDenom];

    (* Create dummy variables and mappings *)
    dummyVars = Table[dummyD[i], {i, Length[originalDenoms]}];
    denomMapping = MapThread[Rule, {originalDenoms, dummyVars}];
    reversedDenomMapping = MapThread[Rule, {dummyVars, originalDenoms}];

    (* Map expression to denominator space *)
    mappedExpr = expr /. denomMapping;

    (* Select best syzygy based on denominator ordering *)
    tmp = Table[
        {Select[relevantSyzygies, DenomAppearsInSyzygy[#, bestDenom[[i]], dummyVars] &], bestDenom[[i]]},
        {i, Length[bestDenom]}
    ] // DeleteCases[#, {{}, _}] &;


    (* If no syzygy contains any preferred denominator, return expr *)
    If[tmp === {}, Return[expr]];

    (* Take first available syzygy and chosen denominator *)
    syzygy = tmp[[1, 1, 1]];
    chosenDenom = tmp[[1, 2]];

    (* Reconstruct syzygy polynomial *)
    syzygyPoly = ReconstructRelation[syzygy, dummyVars];

    (* Extract coefficient of chosen denominator *)
    coefficient = Coefficient[syzygyPoly, dummyVars[[chosenDenom]]];

    (* Build replacement rule *)
    replacementRule = -(syzygyPoly - coefficient * dummyVars[[chosenDenom]]) / coefficient;

    (* One operator *)
    oneOperator = replacementRule / dummyVars[[chosenDenom]];

    (* Multiply expression by one-operator and expand *)
    tmp = mappedExpr * oneOperator // Expand;

    (* Map back to variable space and recurse *)
    tmp /. reversedDenomMapping //
        EliminateHomogeneousSyzygies[#, vars, allHomSyzygies, originalDenoms, {bestDenom, originalDenoms}, dummyD] &
]

EliminateHomogeneousSyzygies::notList = "Argument `1` at position `2` must be a list.";
EliminateHomogeneousSyzygies::notSymbol = "Sixth argument `1` must be a Symbol.";
EliminateHomogeneousSyzygies::badOrdering = "denomOrdering `1` must be a list of length 2: {priorityList, originalDenoms}.";
EliminateHomogeneousSyzygies::filterFailed = "Filtering syzygies to current denominators failed.";


(* ::Section::Closed:: *)
(*EliminateInhomogeneousSyzygies*)


(*
  EliminateInhomogeneousSyzygies[expr, vars, allInhomSyzygies, originalDenoms, dummyD]

  Recursively eliminates inhomogeneous syzygies using a pre-computed list.

  Instead of computing syzygies at each step (expensive Gr\[ODoubleDot]bner basis),
  we filter the pre-computed list to current denominators.

  Parameters:
    expr              - The expression to reduce
    vars              - List of variables
    allInhomSyzygies  - Pre-computed list of ALL inhomogeneous syzygies
    originalDenoms    - Original bare denominators (for syzygy mapping)
    dummyD            - Symbol for denominator space mapping

  Returns:
    Expression with all inhomogeneous syzygies eliminated
*)

ClearAll[EliminateInhomogeneousSyzygies]

EliminateInhomogeneousSyzygies[args___] := Null /; !CheckArguments[EliminateInhomogeneousSyzygies[args], 5]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, allInhomSyzygies, 3];
    $Failed
) /; !ListQ[allInhomSyzygies]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, originalDenoms, 4];
    $Failed
) /; !ListQ[originalDenoms]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notSymbol, dummyD];
    $Failed
) /; !MatchQ[dummyD, _Symbol]

(* For sums: map over each additive term independently *)
EliminateInhomogeneousSyzygies[expr_Plus, vars_List, allInhomSyzygies_List, originalDenoms_List, dummyD_Symbol] :=
    Map[EliminateInhomogeneousSyzygies[#, vars, allInhomSyzygies, originalDenoms, dummyD] &, expr]

(* Base case: expression free of all variables *)
EliminateInhomogeneousSyzygies[expr_, vars_List, allInhomSyzygies_List, originalDenoms_List, dummyD_Symbol] :=
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Main recursive definition *)
EliminateInhomogeneousSyzygies[expr_, vars_List, allInhomSyzygies_List, originalDenoms_List, dummyD_Symbol] := Module[
    {
        tmp,
        k, denoms, dummyVars,
        relevantSyzygies, syzygyWithoutConstant,
        denomMapping, reversedDenomMapping,
        mappedExpr,
        syzygy, constant, oneOperator
    },

    (* Check if denominator is a single term (not Times) *)
    If[Head[Denominator[expr]] =!= Times,
        Return[expr]
    ];

    (* Get current bare denominators *)
    denoms = GetBareDenoms[expr, vars];
    k = Length[denoms];

    (* Base case: fewer than or equal to n denominators, nothing to reduce *)
    If[k <= Length[vars], Return[expr]];

    (* Filter syzygies to those relevant for current denominators *)
    relevantSyzygies = FilterSyzygiesToCurrentDenoms[allInhomSyzygies, denoms, originalDenoms];

    (* Check for filtering failure *)
    If[relevantSyzygies === $Failed,
        Message[EliminateInhomogeneousSyzygies::filterFailed];
        Return[$Failed]
    ];

    (* Base case: no relevant syzygies remain *)
    If[relevantSyzygies === {}, Return[expr]];

    (* Create dummy variables and mappings *)
    dummyVars = Table[dummyD[i], {i, Length[originalDenoms]}];
    denomMapping = MapThread[Rule, {originalDenoms, dummyVars}];
    reversedDenomMapping = MapThread[Rule, {dummyVars, originalDenoms}];

    (* Map expression to denominator space *)
    mappedExpr = expr /. denomMapping;

    (* Take first relevant syzygy *)
    syzygy = relevantSyzygies[[1]];
    constant = syzygy[[1]];
    syzygyWithoutConstant = ReplacePart[syzygy, 1 -> 0];
    oneOperator = -1/constant * ReconstructRelation[syzygyWithoutConstant, dummyVars];

    (* Multiply expression by one-operator and expand *)
    tmp = mappedExpr * oneOperator // Expand;

    (* Map back to variable space and recurse *)
    tmp /. reversedDenomMapping //
        EliminateInhomogeneousSyzygies[#, vars, allInhomSyzygies, originalDenoms, dummyD] &
]

EliminateInhomogeneousSyzygies::notList = "Argument `1` at position `2` must be a list.";
EliminateInhomogeneousSyzygies::notSymbol = "Fifth argument `1` must be a Symbol.";
EliminateInhomogeneousSyzygies::filterFailed = "Filtering syzygies to current denominators failed.";


(* ::Section::Closed:: *)
(*LeinartasDecomposition*)


(*
  LeinartasDecomposition[expr, vars]

  Optimized version: computes syzygies once, then passes them through
  the elimination process.
*)

ClearAll[LeinartasDecomposition]

LeinartasDecomposition[args___] := Null /; !CheckArguments[LeinartasDecomposition[args], 2]

LeinartasDecomposition[expr_, vars_] := (
    Message[LeinartasDecomposition::notList, vars];
    $Failed
) /; !ListQ[vars]

LeinartasDecomposition[expr_, vars_List] := (
    Message[LeinartasDecomposition::emptyVars];
    $Failed
) /; Length[vars] === 0

(* Map over sums at the top level *)
LeinartasDecomposition[expr_Plus, vars_List] := 
    Map[LeinartasDecomposition[#, vars] &, expr]

(* Expression free of all variables *)
LeinartasDecomposition[expr_, vars_List] := 
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Expression is a polynomial *)
LeinartasDecomposition[expr_, vars_List] := 
    expr /; PolynomialQ[expr, vars]

(* Main entry point *)
LeinartasDecomposition[expr_, vars_List] := Module[
    {
        tmp,
        coeff, dependentPart,
        factoredExpr,
        denoms, denomOrdering,
        allSyzygies, inhomogeneous, homogeneous,
        result
    },

    (* Separate variable-free coefficient *)
    {coeff, dependentPart} = SeparateDependency[expr, Alternatives @@ vars];

    (* Check if there's actually a denominator to decompose *)
    If[Denominator[dependentPart] === 1, Return[expr]];

    (* Get denominators and compute syzygies *)
    denoms = GetBareDenoms[dependentPart, vars];
    
    (*This fraction is already reduced *)
    If[Length[denoms] <= Length[vars],
        Return[coeff * dependentPart]
    ];

    allSyzygies = FindSyzygies[denoms, vars];

    (* Check for failure *)
    If[allSyzygies === $Failed,
        Message[LeinartasDecomposition::syzygyFailed];
        Return[$Failed]
    ];

    (* Separate into inhomogeneous and homogeneous *)
    {inhomogeneous, homogeneous} = SeparateSyzygiesByType[allSyzygies];

    (* Stage 1: Eliminate inhomogeneous syzygies *)
    tmp = EliminateInhomogeneousSyzygies[dependentPart, vars, inhomogeneous, denoms, Unique[dummyD]];

    (* Check for failure *)
    If[tmp === $Failed,
        Message[LeinartasDecomposition::inhomogeneousFailed];
        Return[$Failed]
    ];

    (* Stage 2: Eliminate homogeneous syzygies *)
    denomOrdering = {GetBestDenominatorToReplace[dependentPart, vars], denoms};
    result = EliminateHomogeneousSyzygies[tmp, vars, homogeneous, denoms, denomOrdering, Unique[dummyD]];
    
    (* Check for failure *)
    If[result === $Failed,
        Message[LeinartasDecomposition::homogeneousFailed];
        Return[$Failed]
    ];

    (* Return with coefficient *)
    coeff * LeinartasDecomposition[result,vars]//Expand
]

LeinartasDecomposition::notList = "Second argument `1` must be a list of variables.";
LeinartasDecomposition::emptyVars = "Variable list must not be empty.";
LeinartasDecomposition::syzygyFailed = "FindSyzygies failed.";
LeinartasDecomposition::inhomogeneousFailed = "Inhomogeneous syzygy elimination failed.";
LeinartasDecomposition::homogeneousFailed = "Homogeneous syzygy elimination failed.";
