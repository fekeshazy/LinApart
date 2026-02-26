(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Functions for denominators*)


(*
  GetDenoms[expr, vars]

  Extracts all variable-dependent denominator factors from an expression.

  Parameters:
    expr - The expression to analyze.
    vars - List of variables. Denominator factors containing any of these 
           variables are returned.

  Returns:
    List of denominator factors (with their powers) that depend on at 
    least one variable in vars.

  Examples:
    GetDenoms[a/(x-1)/(y-2)^3, {x,y}]     -> {x-1, (y-2)^3}
    GetDenoms[a/(x-1)/b^2, {x}]            -> {x-1}
    GetDenoms[a/b/c, {x}]                  -> {}

  Notes:
    - Denominator factors free of all variables are excluded.
    - Powers are preserved: (x-1)^3 appears as (x-1)^3, not (x-1).
    - Requires at least two denominator factors (the denominator must 
      have head Times). Single denominator expressions must be handled 
      separately before calling this function.
*)



ClearAll[GetDenoms]



GetDenoms[args___] := Null /; !CheckArguments[GetDenoms[args], 2]

GetDenoms[expr_, vars_] := (
    Message[GetDenoms::notList, vars];
    $Failed
) /; !ListQ[vars]

GetDenoms[expr_, vars_List] := (
    Message[GetDenoms::emptyVars];
    $Failed
) /; Length[vars] === 0

GetDenoms[expr_, vars_List] := (
    Message[GetDenoms::singleDenom, Denominator[expr]];
    $Failed
) /; Head[Denominator[expr]] =!= Times && !FreeQ[Denominator[expr], Alternatives @@ vars]

GetDenoms[expr_, vars_List] := Module[
    {},

    expr // Denominator // Apply[List, #] & //
        Select[#, !FreeQ[#, Alternatives @@ vars] &] &
]



GetDenoms::notList = "Second argument `1` must be a list of variables.";
GetDenoms::emptyVars = "Variable list must not be empty.";
GetDenoms::singleDenom = "Expression has a single denominator factor `1`. This function requires a product of at least two denominator factors.";


(*
  GetBareDenoms[expr, vars]

  Extracts all variable-dependent denominator factors from an expression 
  with powers stripped off.

  Parameters:
    expr - The expression to analyze.
    vars - List of variables.

  Returns:
    List of bare denominator bases (without exponents) that depend on 
    at least one variable in vars.

  Examples:
    GetBareDenoms[a/(x-1)/(y-2)^3, {x,y}]  -> {x-1, y-2}
    GetBareDenoms[a/(x-1)^5/b^2, {x}]      -> {x-1}

  Notes:
    - Uses Replace at level {1} to strip powers only from the top-level 
      factors of the denominator. This avoids incorrectly modifying 
      subexpressions like ((1-x)^2 + y) where the Power is structural, 
      not a multiplicity.
    - Requires at least two denominator factors.
*)



ClearAll[GetBareDenoms]



GetBareDenoms[args___] := Null /; !CheckArguments[GetBareDenoms[args], 2]

GetBareDenoms[expr_, vars_] := (
    Message[GetBareDenoms::notList, vars];
    $Failed
) /; !ListQ[vars]

GetBareDenoms[expr_, vars_List] := (
    Message[GetBareDenoms::emptyVars];
    $Failed
) /; Length[vars] === 0

GetBareDenoms[expr_, vars_List] := (
    Message[GetBareDenoms::singleDenom, Denominator[expr]];
    $Failed
) /; Head[Denominator[expr]] =!= Times && !FreeQ[Denominator[expr], Alternatives @@ vars]



GetBareDenoms[expr_, vars_List] := Module[
    {},

    (* 
      Replace (not ReplaceAll) at level {1} is essential here.
      ReplaceAll would descend into subexpressions like ((1-x)^2 + y) 
      and incorrectly strip the Power.
    *)
    expr // Denominator // Apply[List, #] & //
        Replace[#,
            Power[arg1_, power_] /; !FreeQ[arg1, Alternatives @@ vars] :> arg1,
            {1}
        ] & //
        Select[#, !FreeQ[#, Alternatives @@ vars] &] &
]


GetBareDenoms::notList = "Second argument `1` must be a list of variables.";
GetBareDenoms::emptyVars = "Variable list must not be empty.";
GetBareDenoms::singleDenom = "Expression has a single denominator factor `1`. This function requires a product of at least two denominator factors.";


(*
  CountBareDenoms[expr, vars]

  Counts the number of distinct var-dependent denominator factors 
  in an expression, stripping powers.

  Parameters:
    expr - The expression to analyze
    vars - List of variables

  Returns:
    Integer count of var-dependent denominator bases.

  Examples:
    CountBareDenoms[1/((x-y)^2 y (x+y)), {x,y}]  ->  3
    CountBareDenoms[1/y, {x,y}]                   ->  1
    CountBareDenoms[1/(a b), {x,y}]               ->  0
    CountBareDenoms[x + y, {x,y}]                 ->  0
*)

ClearAll[CountBareDenoms]

CountBareDenoms[expr_, vars_List] := Module[
    {den, factors, varPat},
    
    varPat = Alternatives @@ vars;
    den = Denominator[expr];
    
    (* No denominator *)
    If[den === 1, Return[0]];
    
    (* Convert to list: handle Times vs single factor *)
    factors = If[Head[den] === Times, List @@ den, {den}];
    
    (* Strip powers to get bases *)
    factors = factors /. Power[base_, _] :> base;
    
    (* Count var-dependent factors *)
    Length[Select[factors, !FreeQ[#, varPat] &]]
]


(*
  GetDenomData[expr, vars]

  Extracts all variable-dependent denominator factors with their 
  multiplicities (powers).

  Combines the functionality of GetDenoms and GetBareDenoms, returning 
  paired data for each denominator factor.

  Parameters:
    expr - The expression to analyze.
    vars - List of variables.

  Returns:
    List of {base, multiplicity} pairs for each variable-dependent 
    denominator factor.

  Examples:
    GetDenomData[a/(x-1)/(y-2)^3, {x,y}]
      -> {{x-1, 1}, {y-2, 3}}

    GetDenomData[a/(x-1)^5/(y+1)^2/b^3, {x,y}]
      -> {{x-1, 5}, {y+1, 2}}

  Notes:
    - Factors free of all variables are excluded.
    - Aborts with error message if a denominator factor has an unexpected 
      head (not Power, Plus, or Symbol).
    - Requires at least two denominator factors.
*)



ClearAll[GetDenomData]



GetDenomData[args___] := Null /; !CheckArguments[GetDenomData[args], 2]

GetDenomData[expr_, vars_] := (
    Message[GetDenomData::notList, vars];
    $Failed
) /; !ListQ[vars]

GetDenomData[expr_, vars_List] := (
    Message[GetDenomData::emptyVars];
    $Failed
) /; Length[vars] === 0

GetDenomData[expr_, vars_List] := (
    Message[GetDenomData::singleDenom, Denominator[expr]];
    $Failed
) /; Head[Denominator[expr]] =!= Times && !FreeQ[Denominator[expr], Alternatives @@ vars]



GetDenomData[expr_, vars_List] := Module[
    {denoms, multiplicities, bareDenoms},

    (* Get all variable-dependent denominator factors with powers. *)
    denoms = GetDenoms[expr, vars];

    (* 
      Extract multiplicities:
        - Power head: extract the exponent
        - Plus or Symbol head: multiplicity is 1
        - Anything else: unexpected, abort
    *)
    multiplicities = Which[
        Head[#] === Power,
            # /. Power[arg_, p_] /; !FreeQ[arg, Alternatives @@ vars] :> p,
        Head[#] === Plus || Head[#] === Symbol,
            1,
        True,
            Message[GetDenomData::unexpectedHead, #, Head[#]];
            Abort[]
    ] & /@ denoms;

    (* Strip powers to get bare denominator bases. *)
    bareDenoms = denoms // Replace[#,
        Power[arg1_, power_] /; !FreeQ[arg1, Alternatives @@ vars] :> arg1,
        {1}
    ] &;

    (* Pair each base with its multiplicity. *)
    MapThread[{#1, #2} &, {bareDenoms, multiplicities}]
]


GetDenomData::notList = "Second argument `1` must be a list of variables.";
GetDenomData::emptyVars = "Variable list must not be empty.";
GetDenomData::singleDenom = "Expression has a single denominator factor `1`. This function requires a product of at least two denominator factors.";
GetDenomData::unexpectedHead = "Denominator factor `1` has unexpected head `2`. Expected Power, Plus, or Symbol.";


(* ::Subsection::Closed:: *)
(*FindNullRelations*)


(*
  ExtendedCoefficientVector[expr, vars]

  Constructs the coefficient vector of a linear expression with respect to 
  the given variables, extended with the constant term.

  For an expression like a*x + b*y + c, the result is {a, b, c}.

  This is simpler and faster than CoefficientArrays, which has a more 
  complicated output structure.

  Parameters:
    expr - A linear expression in vars.
    vars - List of variables.

  Returns:
    List of coefficients {coeff_var1, coeff_var2, ..., constant_term}.

  Examples:
    ExtendedCoefficientVector[2x + 3y + 5, {x, y}]  -> {2, 3, 5}
    ExtendedCoefficientVector[x - 1, {x, y}]         -> {1, 0, -1}
*)



ClearAll[ExtendedCoefficientVector]



ExtendedCoefficientVector[args___] := Null /; !CheckArguments[ExtendedCoefficientVector[args], 2]

ExtendedCoefficientVector[expr_, vars_] := (
    Message[ExtendedCoefficientVector::notList, vars];
    $Failed
) /; !ListQ[vars]

ExtendedCoefficientVector[expr_, vars_List] :=
    Append[Coefficient[expr, #] & /@ vars, expr /. Thread[vars -> 0]]
    
    
    
ExtendedCoefficientVector::notList = "Second argument `1` must be a list of variables.";


(*
  ExtendedCoefficientMatrix[denoms, vars]

  Builds the extended coefficient matrix from a list of linear expressions 
  (typically denominators). Each row is the ExtendedCoefficientVector of 
  one expression.

  Parameters:
    denoms - List of linear expressions.
    vars   - List of variables.

  Returns:
    Matrix where row i is ExtendedCoefficientVector[denoms[[i]], vars].

  Examples:
    ExtendedCoefficientMatrix[{x + y - 1, 2x - y + 3}, {x, y}]
      -> {{1, 1, -1}, {2, -1, 3}}
*)


ClearAll[ExtendedCoefficientMatrix]



ExtendedCoefficientMatrix[args___] := Null /; !CheckArguments[ExtendedCoefficientMatrix[args], 2]

ExtendedCoefficientMatrix[denoms_, vars_] := (
    Message[ExtendedCoefficientMatrix::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

ExtendedCoefficientMatrix[denoms_, vars_] := (
    Message[ExtendedCoefficientMatrix::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

ExtendedCoefficientMatrix[denoms_List, vars_List] :=
    ExtendedCoefficientVector[#, vars] & /@ denoms
    
    
ExtendedCoefficientMatrix::notList = "Argument `1` at position `2` must be a list.";


(*
  FindSafeNullRelations[denoms, vars]

  Finds all null relations among a list of linear denominators. A null 
  relation is a vector {a_1, ..., a_k} such that:
    a_1 * D_1 + a_2 * D_2 + ... + a_k * D_k = 0
  where D_i are the denominators.

  These relations are the key ingredient for multivariate partial fraction 
  decomposition: it distangles the common singularities. Each null relation 
  allows us to reduce the number of denominators in a term by one for the 
  price of increasing the multiplictiy of one denominator.

  Parameters:
    denoms - List of linear denominator expressions.
    vars   - List of variables.

  Returns:
    List of null vectors. Each vector has length Length[denoms], with 
    nonzero entries corresponding to the denominators participating in 
    that null relation.

  Algorithm:
    1. Build the extended coefficient matrix (coefficients + constant term) 
       for all denominators.
    2. Enumerate all subsets of denominators of size 2 to Length[vars]+1.
       (A null relation among linear forms in d variables requires at 
       most d+1 forms.)
    3. For each subset, compute the NullSpace of the transposed coefficient 
       submatrix. A nonzero null space means those denominators satisfy 
       a linear relation.
    4. Embed each null vector back into a full-length vector (length = 
       number of denominators), with zeros for non-participating denominators.

  Examples:
    FindSafeNullRelations[{x + y, x - y, 2x}, {x, y}]
      -> {{1, 1, -1}}  (meaning (x+y) + (x-y) - (2x) = 0)

  Notes:
    - The subset enumeration can be expensive when the number of 
      denominators is large (combinatorial growth).
    - "Safe" refers to finding all possible relations, not just a basis.
      This is more thorough than simply computing the NullSpace of the 
      full matrix.
*)



ClearAll[FindSafeNullRelations]



FindSafeNullRelations[args___] := Null /; !CheckArguments[FindSafeNullRelations[args], 2]

FindSafeNullRelations[denoms_, vars_] := (
    Message[FindSafeNullRelations::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

FindSafeNullRelations[denoms_, vars_] := (
    Message[FindSafeNullRelations::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

FindSafeNullRelations[denoms_List, vars_List] := (
    Message[FindSafeNullRelations::emptyList, "denoms"];
    $Failed
) /; Length[denoms] === 0

FindSafeNullRelations[denoms_List, vars_List] := (
    Message[FindSafeNullRelations::emptyList, "vars"];
    $Failed
) /; Length[vars] === 0

FindSafeNullRelations[denoms_List, vars_List] := (
    Message[FindSafeNullRelations::tooFewDenoms];
    $Failed
) /; Length[denoms] < 2



FindSafeNullRelations[denoms_List, vars_List] := Module[
    {
        lengthVars, lengthDenoms,
        extendedMatrix, subSetsOfDenoms,
        allNullRelations, tmpNullVector
    },

    lengthVars = Length[vars];
    lengthDenoms = Length[denoms];

    (* Build the extended coefficient matrix for all denominators. *)
    extendedMatrix = ExtendedCoefficientMatrix[denoms, vars];

    (*
      Generate all subsets of denominator indices of size 2 to d+1, 
      where d = number of variables. A linear relation among linear 
      forms in d variables involves at most d+1 forms.
      Warning: combinatorially expensive for many denominators.
    *)
    subSetsOfDenoms = Subsets[Range[lengthDenoms], {2, lengthVars + 1}];

    (*
      For each subset, compute the NullSpace of the transposed 
      coefficient submatrix. If the null space is nonempty, those 
      denominators are linearly dependent.
    *)
    allNullRelations = Map[
        {#, NullSpace[Transpose[extendedMatrix[[#]]]]} &,
        subSetsOfDenoms
    ];

    (* Remove subsets with no null relations. *)
    allNullRelations = DeleteCases[allNullRelations, {_, {}}];

    (*
      Embed each null vector into a full-length vector.

      For each subset with a null relation:
        - Create a zero vector of length = number of denominators.
        - Place the null vector coefficients at the positions 
          corresponding to the subset indices.
      
      Double loop: a subset can have multiple linearly independent 
      null vectors (when the subset is larger than needed).
    *)
    Table[
        tmpNullVector = ConstantArray[0, lengthDenoms];
        tmpNullVector[[allNullRelations[[i, 1]]]] = allNullRelations[[i, 2, j]];
        tmpNullVector,

        {i, 1, Length[allNullRelations]},
        {j, 1, Length[allNullRelations[[i, 2]]]}
    ] // Flatten[#, 1] &
]



FindSafeNullRelations::notList = "Argument `1` at position `2` must be a list.";
FindSafeNullRelations::emptyList = "The `1` list must not be empty.";
FindSafeNullRelations::tooFewDenoms = "Need at least 2 denominators to find null relations.";


(* ::Subsection::Closed:: *)
(*EliminateNullRelations*)


(*
  GetBestDenominatorToReplace[expr, vars]

  Returns a priority ordering of denominators by their multiplicities 
  (lowest first). The denominator with the lowest multiplicity is the 
  best candidate for elimination, because replacing it avoids producing 
  high-multiplicity terms.

  This ordering must be computed BEFORE(!) the recursive elimination starts, 
  because during elimination denominators disappear. Using a dynamic
  priority list for recursion will lead to infinity loops.

  Parameters:
    expr - The expression to analyze.
    vars - List of variables.

  Returns:
    A list of indices sorted by multiplicity (ascending). This serves as 
    a priority list: try to eliminate index 1 first, then 2, etc.

  Example:
    For expr with denominators {(x+1)^3, (y-1)^1, (x-y)^2}:
    Returns {2, 3, 1} (multiplicity 1 first, then 2, then 3).
*)


ClearAll[GetBestDenominatorToReplace]


GetBestDenominatorToReplace[args___] := Null /; !CheckArguments[GetBestDenominatorToReplace[args], 2]

GetBestDenominatorToReplace[expr_, vars_] := (
    Message[GetBestDenominatorToReplace::notList, vars];
    $Failed
) /; !ListQ[vars]


GetBestDenominatorToReplace[expr_, vars_List] :=
    expr // GetDenomData[#, vars] & // Part[#, All, 2] & // Ordering


GetBestDenominatorToReplace::notList = "Second argument `1` must be a list of variables.";


(*
  NullSupportSize[nullVec]

  Counts the number of nonzero entries in a null vector. This gives the 
  number of denominators participating in the corresponding null relation.

  Used by SafeNullRelations to filter out relations involving too many 
  denominators. A null relation among linear forms in d variables can 
  involve at most d+1 forms.

  Parameters:
    nullVec - A list (null vector).

  Returns:
    Integer count of nonzero entries.
*)

ClearAll[NullSupportSize]

NullSupportSize[args___] := Null /; !CheckArguments[NullSupportSize[args], 1]

NullSupportSize[nullVec_List] := Count[nullVec, x_ /; x =!= 0]


(*
  SafeNullRelations[nulls, vars]

  Filters a list of null vectors to keep only those involving at most 
  d+1 denominators, where d = Length[vars]. A null relation among linear 
  forms in d variables requires at most d+1 forms.

  Parameters:
    nulls - List of null vectors from FindSafeNullRelations.
    vars  - List of variables.

  Returns:
    Filtered list of null vectors.
*)

ClearAll[SafeNullRelations]

SafeNullRelations[args___] := Null /; !CheckArguments[SafeNullRelations[args], 2]

SafeNullRelations[nulls_List, vars_List] :=
    Select[nulls, NullSupportSize[#] <= Length[vars] + 1 &]


(*
  EliminateNullRelations[expr, vars, denomOrdering, dummyD]

  Recursively eliminates common singularities in a multivariate partial 
  fraction using null relations between linear denominators.

  This is the core recursive routine. Each step:
    - Finds a null relation Sum[alpha_i * D_i] = 0 among the denominators
    - Uses it to construct a "one operator": an expression equal to 1 
      that, when multiplied into the expression, reduces the number of
      denominators and thus gets rid of a null-realation,
    - This reduces the number of distinct denominators by one, at the 
      cost of increasing the multiplicity of one denominator
    - Recurses on the resulting terms until no more null relations exist

  WARNING: This is a recursive function. The a priori denominator 
  ordering prevents infinite loops by ensuring a consistent elimination 
  order. Without it, the algorithm could loop infinitely!

  Parameters:
    expr          - The expression to reduce. Can be a single term or 
                    a sum (Plus), in which case each term is processed 
                    independently.
    vars          - List of variables.
    denomOrdering - A list {priorityList, originalDenoms} where:
                      priorityList  - indices into originalDenoms sorted 
                                      by multiplicity (lowest first), 
                                      from GetBestDenominatorToReplace.
                      originalDenoms - the bare denominators of the 
                                       original expression before any 
                                       elimination.
                    This structure provides stable bookkeeping across 
                    recursive calls as denominators appear and disappear.
    dummyD        - A symbol used to create a "denominator space" that 
                    prevents Mathematica from sorting/simplifying during 
                    the substitution step.

  Returns:
    The expression with all null relations eliminated. Each resulting 
    term has linearly independent denominators.

  Algorithm in detail:

    Step 1 - Update denominator ordering:
    
      Compare current denominators to the original list. Remove any 
      that have disappeared during previous eliminations. Re-index 
      the priority list to match current denominator positions.
      This is necessary because the priority ordering is tied to the 
      original numbering, but denominators may vanish during reduction.

    Step 2 - Map to denominator space:
    
      Replace each denominator D_i with a dummy symbol dummyD[i].
      This prevents Mathematica from automatically simplifying 
      expressions like (x+y) - (x-y) -> 2y during the one-operator 
      construction. The elimination must happen structurally, not 
      algebraically.
      
      Technical note: when mapping forward, rules must be applied 
      one-at-a-time per denominator (MapThread[Replace, ...]) because 
      one denominator may be a translated version of another 
      (e.g., x+y and 1+x+y), and rule ordering with ReplaceAll could 
      cause incorrect substitutions. This is not a problem for the 
      reverse mapping.

    Step 3 - Find null relations:
    
      Compute all null relations among the current denominators using 
      FindSafeNullRelations, then filter to safe size with 
      SafeNullRelations.
      If no relations exist, the denominators are linearly independent 
      and we return the expression unchanged (recursion base case).

    Step 4 - Select best null relation:
    
      From the priority list, find the highest-priority denominator 
      that participates in at least one null relation. This is the 
      denominator we will eliminate.
      If the very best denominator is not in any null relation (can 
      happen after several elimination steps), fall back to the next 
      best, and so on. This is why we maintain a priority list rather 
      than a single choice.
      Among available relations for the chosen denominator, we simply 
      take the first one. More sophisticated cost functions were tried 
      but gave only incremental improvement.
      Important: we apply only ONE null relation per step, then recurse. 
      Applying all at once could introduce unwanted denominators in 
      the numerator.

    Step 5 - Construct and apply the one operator:
      From the null relation Sum[alpha_i D_i] = 0, solve for the 
      eliminated denominator:
        D_k = -1/alpha_k * Sum_{j != k} alpha_j D_j
      The "one operator" is (replacement / D_k) = 1.
      Multiply the expression by this operator, expand, map back to 
      variable space, and recurse.

  Notes:
    - The bookkeeping in Step 4 (indicies, constants, eliminatedDenoms) 
      is inherited from an older version and could potentially be 
      simplified.
    - The main performance bottleneck is not this function itself but 
      the number of terms and multiplicities produced during elimination.
*)



ClearAll[EliminateNullRelations]



EliminateNullRelations[args___] := Null /; !CheckArguments[EliminateNullRelations[args], 4]

EliminateNullRelations[expr_, vars_, denomOrdering_, dummyD_] := (
    Message[EliminateNullRelations::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

EliminateNullRelations[expr_, vars_, denomOrdering_, dummyD_] := (
    Message[EliminateNullRelations::notList, denomOrdering, 3];
    $Failed
) /; !ListQ[denomOrdering]

EliminateNullRelations[expr_, vars_, denomOrdering_, dummyD_] := (
    Message[EliminateNullRelations::notSymbol, dummyD];
    $Failed
) /; !MatchQ[dummyD, _Symbol]

EliminateNullRelations[expr_, vars_, denomOrdering_, dummyD_] := (
    Message[EliminateNullRelations::badOrdering, denomOrdering];
    $Failed
) /; Length[denomOrdering] =!= 2



(* For sums: map over each additive term independently. *)
EliminateNullRelations[expr_Plus, vars_List, denomOrdering_List, dummyD_Symbol] :=
    Map[EliminateNullRelations[#, vars, denomOrdering, dummyD] &, expr]

(* Base case: expression free of all variables, nothing to eliminate. *)
EliminateNullRelations[expr_, vars_List, denomOrdering_List, dummyD_Symbol] :=
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Main recursive definition. *)
EliminateNullRelations[expr_, vars_List, denomOrdering_List, dummyD_Symbol] := Module[
    {
        tmp,
        denoms, nulls,
        bestDenom = denomOrdering[[1]],
        originalDenoms = denomOrdering[[2]],

        indexOriginal, indexNew, indexingRule,

        costsOfNulls, bestNull,
		
		dummyVars,
        denomMapping, reversedDenomMapping,
        mappedDenoms, mappedExpr,

        indicies, constants, eliminatedDenoms,
        replacementRule, oneOperator
    },




    (* ==================== *)
    (* Step 1: Update the denominator ordering. *)
    (* ==================== *)


    (* Get current bare denominators. *)
    denoms = GetBareDenoms[expr, vars];

    (* Find which original denominators are no longer present. *)
    tmp = Complement[originalDenoms, denoms];

    (* Get positions of missing denominators in the original list. *)
    tmp = tmp /. PositionIndex[originalDenoms];

    (* Remove missing denominators from the priority list. *)
    tmp = Flatten[tmp];
    bestDenom = DeleteCases[bestDenom, Alternatives @@ tmp];

    (*
      Re-index the priority list to match current denominator positions.

      Technical note: indexOriginal must remain in Association form to 
      prevent garbage substitutions. Without this, rules like 
      z -> {7} could appear instead of {7} -> {7}.
    *)
    indexOriginal = PositionIndex[originalDenoms];
    indexNew = PositionIndex[denoms] // Normal;
    indexingRule = indexNew /. indexOriginal /. {arg_} :> arg;

    bestDenom = bestDenom /. indexingRule;




    (* ==================== *)
    (* Step 2: Map to denominator space. *)
    (* ==================== *)

	(*Making dummy variables.*)
	dummyVars=Table[dummyD[i], {i, 1, Length[denoms]}];

    (* Forward mapping: denominators -> dummy symbols. *)
    denomMapping = MapThread[Rule, {denoms, dummyVars}];

    (* Reverse mapping: dummy symbols -> denominators. *)
    reversedDenomMapping = MapThread[Rule, {dummyVars, denoms}];

    (*
      Map denominators to dummy space.

      MapThread[Replace, ...] applies each rule to its corresponding 
      denominator individually. This is essential because one denominator 
      can be a translation of another (e.g., x+y and 1+x+y), and 
      applying all rules via ReplaceAll could cause incorrect matches 
      due to rule ordering.

      This asymmetry does not affect the reverse mapping.
    *)
    mappedDenoms = MapThread[Replace, {denoms, denomMapping}];

    (* Map the full expression to denominator space. *)
    mappedExpr = expr /. denomMapping;



    (* ==================== *)
    (* Step 3: Find null relations. *)
    (* ==================== *)


    nulls = FindSafeNullRelations[denoms, vars];
    nulls = SafeNullRelations[nulls, vars];

    (* Base case: no null relations means denominators are independent. *)
    If[nulls === {}, Return[expr]];




    (* ==================== *)
    (* Step 4: Select the best null relation. *)
    (* ==================== *)


    (*
      For each priority level, filter null relations to those containing 
      the corresponding denominator. Remove empty groups.
    *)
    tmp = Table[
        {DeleteCases[nulls, arg_ /; arg[[bestDenom[[i]]]] === 0], bestDenom[[i]]},
        {i, Length[bestDenom]}
    ] // DeleteCases[#, {{}, _}] &;

    (* 
      Take the first group (highest priority denominator that appears 
      in at least one null relation).
    *)
    nulls = tmp[[1, 1]];
    bestDenom = tmp[[1, 2]];

    (* Among available relations, simply take the first one. *)
    bestNull = 1;
    nulls = {nulls[[bestNull]]};




    (* ==================== *)
    (* Step 5: Construct and apply the one operator. *)
    (* ==================== *)


    (*
      Bookkeeping: identify which entry in the null vector corresponds 
      to the denominator being eliminated (bestDenom).
    *)

    (* Get positions of all nonzero entries, grouped by null relation. *)
    indicies = nulls // 
        Position[#, (x_ /; x =!= 0), {2}, Heads -> False] & // 
        GatherBy[#, First] &;

    (* Keep only the entry corresponding to bestDenom. *)
    indicies = indicies // 
        Cases[#, {arg1_, arg2_} /; arg2 === bestDenom :> {arg1, arg2}, 2] &;

    (* Extract the coefficient alpha_k for normalization. *)
    constants = Extract[nulls, indicies];

    (*
      Get the eliminated denominator in dummy space.
      Copy the denominator list for each null relation (in case of multiple).
    *)
    eliminatedDenoms = Table[denoms, {i, 1, Length[nulls]}];
    eliminatedDenoms = Extract[eliminatedDenoms, indicies];
    eliminatedDenoms = eliminatedDenoms /. denomMapping;

    (*
      Construct the replacement:
        D_k = -1/alpha_k * Sum_{j != k} alpha_j D_j

      The one operator is (replacement / D_k) = 1 by the null relation.
    *)
    replacementRule = Table[
        -1/constants[[i]] * 
            Plus @@ (Delete[nulls[[i]], indicies[[i, 2]]] * Delete[mappedDenoms, indicies[[i, 2]]]),
        {i, 1, Length[indicies]}
    ];
    
    (*
      Safety check: the replacement rule should be a homogeneous linear 
      combination of the variables. If we set all variables to zero and 
      the result is nonzero, the relation is affine (contains a constant 
      term), which would not reduce the number of denominators and cause 
      infinite recursion.
    *)
    If[!(replacementRule /. Thread[dummyVars -> 0] // Flatten // Total) === 0,
        Message[EliminateNullRelations::affineRelation, 
            replacementRule /. reversedDenomMapping];
        Return[$Failed, Module]
    ];
    

    (* Construct the one operator. *)
    oneOperator = Times @@ (replacementRule / eliminatedDenoms);

    (* Multiply expression by the one operator and expand. *)
    tmp = mappedExpr * oneOperator // Expand;

    (* Map back to variable space and recurse. *)
    tmp /. reversedDenomMapping // 
        EliminateNullRelations[#, vars, denomOrdering, dummyD] &
]



EliminateNullRelations::notList = "Argument `1` at position `2` must be a list.";
EliminateNullRelations::notSymbol = "Fourth argument `1` must be a Symbol.";
EliminateNullRelations::badOrdering = "denomOrdering `1` must be a list of length 2: {priorityList, originalDenoms}.";
EliminateNullRelations::affineRelation = 
    "Encountered an affine relation `1`. This would not reduce the number of denominators. Skipping to avoid infinite recursion.";


(* ::Subsection::Closed:: *)
(*ExpandNumeratorInDenomSpace*)


ClearAll[ExpandDenominatorTerms]

(* Handle sums: map over each term *)
ExpandDenominatorTerms[expr_Plus] := ExpandDenominatorTerms /@ expr

(* Handle single terms *)
ExpandDenominatorTerms[expr_] := Module[
    {
        numerator,
        denominator,
        factors
    },

    numerator = Numerator[expr];
    denominator = Denominator[expr];

    (* If no denominator, return as-is *)
    If[denominator === 1, Return[expr]];

    (* Convert to list of factors: handle Times vs single factor *)
    factors = If[Head[denominator] === Times, 
        List @@ denominator, 
        {denominator}
    ];

    (* Expand the base of each factor *)
    factors = Map[
        If[Head[#] === Power,
            Power[Expand[#[[1]]], #[[2]]],
            Expand[#]
        ] &,
        factors
    ];

    (* Reconstruct *)
    denominator = Times @@ factors;

    numerator / denominator
]


(*
  ExpandNumeratorInDenomSpace[expr, vars, bases]

  Recursively expands variable-dependent numerators by transforming 
  to denominator space for each basis in the list.

  For each basis:
    1. Change coordinates: w_i = B_i(x), invert to get x(w)
    2. Expand: w-monomials in numerator cancel against w_i poles
    3. Transform back to original variables
    4. Recurse with remaining bases

  Base case: empty basis list, return expression unchanged.

  Parameters:
    expr  - The expression (can be a sum)
    vars  - List of variables
    bases - List of bases, each a list of n linear forms

  Returns:
    The expression with numerators expanded in all denominator spaces.
*)

ClearAll[ExpandNumeratorInDenomSpace]

ExpandNumeratorInDenomSpace[args___] := Null /; !CheckArguments[ExpandNumeratorInDenomSpace[args], 3]

(* Base case: no more bases *)
ExpandNumeratorInDenomSpace[expr_, vars_List, {}] := expr

(* Recursive case *)
ExpandNumeratorInDenomSpace[expr_, vars_List, bases_List] := Module[
    {
        basis, wVars, varsInW, reverseW, result
    },

    basis = First[bases];

    wVars = Table[Unique["w"], {Length[vars]}];

    varsInW = MapThread[
        Rule,
        {vars, vars /. First@Solve[Thread[wVars == basis], vars]}
    ];

    reverseW = MapThread[Rule, {wVars, basis}];

    result = expr /. varsInW // Expand;
    
    result = result /. reverseW// ExpandDenominatorTerms;

    ExpandNumeratorInDenomSpace[result, vars, Rest[bases]]
]


(* ::Subsection::Closed:: *)
(*FindBases*)


(*
  FindBases[denoms, vars]

  Finds all subsets of denominators that form a basis for the space of 
  linear forms in the given variables.

  A set of n linear forms in n variables forms a basis if and only if 
  their coefficient matrix has nonzero determinant. Geometrically, this 
  means the n hyperplanes defined by these forms intersect at exactly 
  one point (the common singularity).

  This is essential for multivariate partial fraction decomposition:
    - Each basis corresponds to a set of denominators 
      whose simultaneous vanishing defines an isolated point.
    - The partial fraction decomposition expresses the original expression 
      as a sum over all such bases.
    - Non-basis subsets (with zero determinant) correspond to degenerate 
      configurations where the hyperplanes don't intersect at a single point.

  Parameters:
    denoms - List of linear denominator expressions in vars.
    vars   - List of n variables.

  Returns:
    List of sorted n-element subsets of denoms that form bases 
    (i.e., have linearly independent coefficient vectors).

  Examples:
    FindBases[{x, y, x + y}, {x, y}]
      -> {{x, y}, {x, x + y}, {y, x + y}}
      (All three pairs are linearly independent.)

    FindBases[{x, 2x, y}, {x, y}]
      -> {{x, y}, {2x, y}}
      ({x, 2x} has zero determinant since 2x is proportional to x.)

  Algorithm:
    1. Generate all n-element subsets of denoms.
    2. For each subset, build the coefficient matrix by extracting 
       coefficients with respect to each variable.
    3. Keep only subsets where Det[coeffMatrix] != 0.

  Why the determinant check is necessary:
    The null relation elimination (EliminateNullRelations) checks for 
    AFFINE dependence \[LongDash] relations like a\:2081D\:2081 + a\:2082D\:2082 + ... = 0 where D\:1d62 
    are the full linear forms including constant terms.

    The determinant check here tests LINEAR independence of the 
    COEFFICIENT VECTORS only (ignoring constant terms).

    These are different! Consider {x + y, x + y - 1}:
      - No null relation exists: a(x+y) + b(x+y-1) = 0 implies 
        ax + ay + bx + by - b = 0, so a+b=0 for x, a+b=0 for y, 
        and -b=0, giving a=b=0. Affinely independent.
      - But coefficient vectors are {1,1} and {1,1} \[LongDash] linearly DEPENDENT!
        Det[{{1,1},{1,1}}] = 0.

    If the coefficient matrix is singular (Det = 0), we cannot solve 
    the system {w\:2081 = D\:2081, w\:2082 = D\:2082, ...} for the original variables. 
    This is required in ResidueForBasis to compute the coordinate 
    transformation to denominator space:
      varsInW = Solve[Thread[wVars == basisDenoms], vars]
    
    With Det = 0, Solve returns {} and the algorithm fails.

  Notes:
    - Subsets are sorted for canonical ordering.
    - The number of subsets grows as Binomial[Length[denoms], n], which 
      can be large for many denominators.
    - This finds ALL bases; for large denominator sets, this can be 
      computationally expensive.
*)


ClearAll[FindBases]



FindBases[args___] := Null /; !CheckArguments[FindBases[args], 2]

FindBases[denoms_, vars_] := (
    Message[FindBases::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

FindBases[denoms_, vars_] := (
    Message[FindBases::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

FindBases[denoms_List, vars_List] := (
    Message[FindBases::emptyList, "denoms"];
    $Failed
) /; Length[denoms] === 0

FindBases[denoms_List, vars_List] := (
    Message[FindBases::emptyList, "vars"];
    $Failed
) /; Length[vars] === 0

FindBases[denoms_List, vars_List] := (
    Message[FindBases::tooFewDenoms, Length[denoms], Length[vars]];
    $Failed
) /; Length[denoms] < Length[vars]



FindBases[denoms_List, vars_List] := Module[
    {n, subsets},

    n = Length[vars];

    (* All n-element subsets, sorted for canonical ordering. *)
    subsets = Subsets[denoms, {n}] // Map[Sort];

    (*
      Keep only linearly independent subsets.
      The coefficient matrix (without constant term) must have 
      nonzero determinant, ensuring we can solve for vars.
    *)
    Select[subsets, Det[Coefficient[#, vars] & /@ #] =!= 0 &]
]

FindBases::notList = "Argument `1` at position `2` must be a list.";
FindBases::emptyList = "The `1` list must not be empty.";
FindBases::tooFewDenoms = "Need at least `2` denominators to form a basis in `2` variables, but only `1` were provided.";
