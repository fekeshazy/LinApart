(* ::Package:: *)

(* ::Section::Closed:: *)
(*FindSyzygies*)


(*
  FindSyzygies[denoms, vars]

  Finds polynomial syzygies among a list of polynomial denominators 
  using Gr\[ODoubleDot]bner basis elimination.

  A syzygy is a polynomial relation among the denominators:
    p(D\:2081, D\:2082, ..., D\:2096) = 0
  where p is a polynomial in k abstract variables representing the D\:1d62.
  
  IMPORTANT:
    Despite the name, this function does NOT return "all syzygies"
    (which would generally be infinite), but rather a generating set of
    polynomial relations obtained from the Gr\[ODoubleDot]bner basis.
  
  Relations may be homogeneous (constant term = 0) or inhomogeneous 
  (constant term \[NotEqual] 0). For inhomogeneous relations, the constant term 
  encodes a Nullstellensatz-type certificate: dividing by the constant 
  gives a "one operator" 1 = (1/c) \[CapitalSigma] h\:1d62 D\:1d62.

  Algorithm:
    1. Introduce dummy variables d\:2081, ..., d\:2096 representing D\:2081, ..., D\:2096.
    2. Build the ideal \:27e8D\:2081 - d\:2081, ..., D\:2096 - d\:2096\:27e9 in K[x\:2081,...,x\:2099,d\:2081,...,d\:2096].
    3. Compute a Gr\[ODoubleDot]bner basis with elimination ordering that eliminates 
       the original variables x\:2081, ..., x\:2099 first.
    4. Extract basis elements free of x\:2081, ..., x\:2099 \[LongDash] these are the 
       polynomial relations among d\:2081, ..., d\:2096.
    5. Convert each relation to CoefficientArrays format for structured 
       downstream access.

  Parameters:
    denoms - List of polynomial denominators {D\:2081, ..., D\:2096} with k \[GreaterEqual] 2.
    vars   - List of variables {x\:2081, ..., x\:2099}.

  Returns:
    List of syzygies, each in CoefficientArrays normal form:
      {c\:2080, c\:2081, c\:2082, ...}
    where:
      c\:2080       = scalar constant term (degree 0)
      c\:2081       = vector of linear coefficients (degree 1)
      c\:2082       = matrix of quadratic coefficients (degree 2)
      ...and so on for higher degrees.
    
    The original polynomial can be reconstructed via ReconstructRelation.
    
    A syzygy is homogeneous if c\:2080 = 0, inhomogeneous if c\:2080 \[NotEqual] 0.

  Examples:
    FindSyzygies[{x + y, x - y, 2*x}, {x, y}]
      Returns a syzygy encoding: d\:2081 + d\:2082 - d\:2083 = 0

    FindSyzygies[{x^2 - 1, x + 1, x - 1}, {x}]
      Returns a syzygy encoding: d\:2081 - d\:2082 * d\:2083 = 0

  Notes:
    - Uses EliminationOrder to cleanly separate relations from the 
      Gr\[ODoubleDot]bner basis computation.
    - CoefficientDomain -> RationalFunctions is used so that the 
      computation works for denominators with parametric coefficients.
    - Each returned syzygy is verified by substituting back the 
      original denominators and checking that the result vanishes.
    - Returns an empty list if no polynomial relations exist among 
      the given denominators (e.g., algebraically independent set).
*)

ClearAll[FindSyzygies]

FindSyzygies[args___] := Null /; !CheckArguments[FindSyzygies[args], 2]

FindSyzygies[denoms_, vars_] := (
    Message[FindSyzygies::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

FindSyzygies[denoms_, vars_] := (
    Message[FindSyzygies::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

FindSyzygies[denoms_List, vars_List] := (
    Message[FindSyzygies::emptyList, "denoms"];
    $Failed
) /; Length[denoms] === 0

FindSyzygies[denoms_List, vars_List] := (
    Message[FindSyzygies::emptyList, "vars"];
    $Failed
) /; Length[vars] === 0

FindSyzygies[denoms_List, vars_List] := (
    Message[FindSyzygies::tooFewDenoms];
    $Failed
) /; Length[denoms] < 2

FindSyzygies[denoms_List, vars_List] := Module[
    {
        k = Length[denoms],
        dummyD, dummyVars,
        allVars, idealGenerators,
        groebnerBasis,
        relations, syzygies,
        verificationRules
    },

    (* Create unique dummy variables for denominators. *)
    dummyD = Unique["dummyD"];
    dummyVars = Table[dummyD[i], {i, k}];

    (*
      All variables for the Gr\[ODoubleDot]bner basis computation.
      Original variables come first so EliminationOrder eliminates them,
      leaving relations purely in the dummy variables.
    *)
    allVars = Join[vars, dummyVars];

    (* 
      Ideal generators: D\:1d62 - d\:1d62 = 0 encodes the substitution d\:1d62 = D\:1d62.
      Any polynomial in the Gr\[ODoubleDot]bner basis that is free of the original 
      variables is a polynomial relation among the D\:1d62.
    *)
    idealGenerators = Table[denoms[[i]] - dummyVars[[i]], {i, k}];

    (*
      Compute Gr\[ODoubleDot]bner basis with elimination ordering.
      
      CoefficientDomain -> RationalFunctions allows denominators with 
      parametric (non-numeric) coefficients to be handled correctly.
    *)
    groebnerBasis = GroebnerBasis[
        idealGenerators,
        allVars,
        MonomialOrder -> EliminationOrder,
        CoefficientDomain -> RationalFunctions
    ];

    (* 
      Extract relations: Gr\[ODoubleDot]bner basis elements free of original variables.
      These are the polynomial syzygies among dummyVars.
    *)
    relations = Select[groebnerBasis, FreeQ[#, Alternatives @@ vars] &];

    (* 
      Convert each relation to CoefficientArrays normal form.
      
      CoefficientArrays[poly, {d\:2081,...,d\:2096}] returns {c\:2080, c\:2081, c\:2082, ...} where:
        c\:2080 = constant (scalar)
        c\:2081 = linear coefficients (vector of length k)
        c\:2082 = quadratic coefficients (k \[Times] k matrix)
        etc.
      
      Normal converts any SparseArray entries to regular arrays.
    *)
    syzygies = Map[(CoefficientArrays[#, dummyVars] // Normal)&, relations];

    syzygies
]

FindSyzygies::notList = "Argument `1` at position `2` must be a list.";
FindSyzygies::emptyList = "The `1` list must not be empty.";
FindSyzygies::tooFewDenoms = "Need at least 2 denominators to find syzygies.";

(*
  ReconstructRelation[coeffArrays, vars]

  Reconstructs a polynomial from its CoefficientArrays normal form.

  This is the inverse operation to CoefficientArrays[poly, vars] // Normal.
  Given the nested coefficient structure {c\:2080, c\:2081, c\:2082, ...} and the list 
  of variables, it rebuilds the original polynomial.

  The reconstruction formula:
    poly = c\:2080 + c\:2081 \[CenterDot] v + v \[CenterDot] c\:2082 \[CenterDot] v + ... (generalized tensor contraction)
  
  More precisely, for degree d:
    contribution_d = Nest[Dot[#, vars]&, c_d, d]
  which contracts all d indices of the coefficient tensor c_d with the 
  variable vector.

  Parameters:
    coeffArrays - List {c\:2080, c\:2081, c\:2082, ...} as returned by 
                  CoefficientArrays[poly, vars] // Normal.
                  c\:2080 is a scalar, c\:2081 is a vector, c\:2082 is a matrix, etc.
    vars        - List of variables {v\:2081, ..., v\:2096} matching those used 
                  in the original CoefficientArrays call.

  Returns:
    The reconstructed polynomial in vars.

  Examples:
    coeffs = CoefficientArrays[3 + 2x - x*y, {x, y}] // Normal;
    ReconstructRelation[coeffs, {x, y}]
      -> 3 + 2x - x*y

    coeffs = CoefficientArrays[x^2 + y^2, {x, y}] // Normal;
    ReconstructRelation[coeffs, {x, y}]
      -> x^2 + y^2

  Notes:
    - The variable list must match (in length and order) the one used 
      when CoefficientArrays was called.
    - Works for polynomials of any degree: linear, quadratic, cubic, etc.
    - The Nest[Dot[#, vars]&, tensor, d] pattern contracts a rank-d tensor 
      with the variable vector d times, producing a scalar.
*)

ClearAll[ReconstructRelation]

ReconstructRelation[args___] := Null /; !CheckArguments[ReconstructRelation[args], 2]

ReconstructRelation[coeffArrays_, vars_] := (
    Message[ReconstructRelation::notList, coeffArrays, 1];
    $Failed
) /; !ListQ[coeffArrays]

ReconstructRelation[coeffArrays_, vars_] := (
    Message[ReconstructRelation::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

ReconstructRelation[coeffArrays_List, vars_List] := (
    Message[ReconstructRelation::emptyList, "coeffArrays"];
    $Failed
) /; Length[coeffArrays] === 0

ReconstructRelation[coeffArrays_List, vars_List] := (
    Message[ReconstructRelation::emptyList, "vars"];
    $Failed
) /; Length[vars] === 0

ReconstructRelation[coeffArrays_List, vars_List] := 
    Sum[
        Nest[Dot[#, vars] &, coeffArrays[[i]], i - 1],
        {i, 1, Length[coeffArrays]}
    ]

ReconstructRelation::notList = "Argument `1` at position `2` must be a list.";
ReconstructRelation::emptyList = "The `1` list must not be empty.";


(* ::Section::Closed:: *)
(*SeparateSyzygiesByType*)


(*
  SeparateSyzygiesByType[syzygies]

  Separates syzygies into inhomogeneous and homogeneous classes according
  to their constant term.

  Input convention:
    Each syzygy is assumed to be in the raw format returned by
      CoefficientArrays[relationPoly, dummyVars] // Normal
    i.e. a list of the form
      {constant, linearCoefficients, quadraticCoefficients, ...}

    Thus the constant term is always syzygy[[1]].

  Mathematical meaning:
    - Homogeneous syzygy:
        relationPoly(D_1, ..., D_k) = 0
      so the constant term is zero.

    - Inhomogeneous syzygy:
        relationPoly(D_1, ..., D_k) = c
      with c != 0, equivalently
        relationPoly(D_1, ..., D_k) + c = 0
      depending on sign convention.

    In the present internal representation, the distinction is determined
    solely by whether the constant term syzygy[[1]] vanishes.

  Parameters:
    syzygies - List of syzygies in CoefficientArrays format.

  Returns:
    {inhomogeneousSyzygies, homogeneousSyzygies}

    where:
      - inhomogeneousSyzygies contains all syzygies with nonzero constant term
      - homogeneousSyzygies contains all syzygies with zero constant term

  Notes:
    - Inhomogeneous syzygies are returned first because they are intended
      to be used first in the elimination pipeline.
    - An empty input list returns {{}, {}}.
*)

ClearAll[SeparateSyzygiesByType]

SeparateSyzygiesByType[args___] := Null /; !CheckArguments[SeparateSyzygiesByType[args], 1]

SeparateSyzygiesByType[syzygies_] := (
    Message[SeparateSyzygiesByType::notList, syzygies];
    $Failed
) /; !ListQ[syzygies]

SeparateSyzygiesByType[syzygies_List] := (
    Message[
        SeparateSyzygiesByType::malformed,
        Select[syzygies, !ListQ[#] || # === {} &]
    ];
    $Failed
) /; !And @@ (ListQ[#] && # =!= {} & /@ syzygies)

SeparateSyzygiesByType[syzygies_List] := Module[
    {
        inhomogeneous,
        homogeneous
    },

    (* Inhomogeneous syzygies have nonzero constant term. *)
    inhomogeneous = Select[syzygies, First[#] =!= 0 &];

    (* Homogeneous syzygies have zero constant term. *)
    homogeneous = Select[syzygies, First[#] === 0 &];

    {inhomogeneous, homogeneous}
]

SeparateSyzygiesByType::notList = "Argument `1` must be a list.";
SeparateSyzygiesByType::malformed = "The following syzygies are malformed. Each syzygy must be a non-empty list in CoefficientArrays format: `1`.";


(* ::Section::Closed:: *)
(*FilterSyzygiesToCurrentDenoms*)


(*
  doesTheSyzygyContainTheDenoms[syzygy, presentIndices, originalDenoms]

  Checks whether a syzygy involves only denominators that are still present
  in the current expression.

  Input convention:
    syzygy is assumed to be in the raw format returned by
      CoefficientArrays[relationPoly, dummyVars] // Normal
    i.e. a list of coefficient arrays
      {constant, linearCoefficients, quadraticCoefficients, ...}.

  Parameters:
    syzygy         - A single syzygy in CoefficientArrays format.
    presentIndices - List of indices of denominators still present in the
                     current expression, with respect to originalDenoms.
    originalDenoms - Original list of bare denominators. Only its length is
                     needed here to reconstruct the correct dummy-variable space.

  Returns:
    True  if every denominator appearing in the syzygy is still present.
    False otherwise.

  Notes:
    - The syzygy is reconstructed in a fresh dummy-variable space and then
      scanned to determine which denominator variables actually occur.
    - A constant-only relation has no participating denominators, so it is
      considered valid with respect to any presentIndices.
*)

ClearAll[doesTheSyzygyContainTheDenoms]

doesTheSyzygyContainTheDenoms[args___] := Null /; 
    !CheckArguments[doesTheSyzygyContainTheDenoms[args], 3]

doesTheSyzygyContainTheDenoms[syzygy_, presentIndices_, originalDenoms_] := (
    Message[doesTheSyzygyContainTheDenoms::notList, syzygy, 1];
    $Failed
) /; !ListQ[syzygy]

doesTheSyzygyContainTheDenoms[syzygy_, presentIndices_, originalDenoms_] := (
    Message[doesTheSyzygyContainTheDenoms::notList, presentIndices, 2];
    $Failed
) /; !ListQ[presentIndices]

doesTheSyzygyContainTheDenoms[syzygy_, presentIndices_, originalDenoms_] := (
    Message[doesTheSyzygyContainTheDenoms::notList, originalDenoms, 3];
    $Failed
) /; !ListQ[originalDenoms]

doesTheSyzygyContainTheDenoms[syzygy_, presentIndices_List, originalDenoms_List] := (
    Message[doesTheSyzygyContainTheDenoms::emptySyzygy];
    $Failed
) /; syzygy === {}

doesTheSyzygyContainTheDenoms[syzygy_, presentIndices_List, originalDenoms_List] := (
    Message[
        doesTheSyzygyContainTheDenoms::badIndices,
        Select[presentIndices, !IntegerQ[#] & || # < 1 || # > Length[originalDenoms] &],
        Length[originalDenoms]
    ];
    $Failed
) /; !And @@ (IntegerQ[#] && 1 <= # <= Length[originalDenoms] & /@ presentIndices)

doesTheSyzygyContainTheDenoms[
    syzygy_List,
    presentIndices_List,
    originalDenoms_List
] := Module[
    {
        dummyVar,
        lengthDenoms,
        
        dummyVars,
        syzygyPoly,
        participatingDenoms
    },

	dummyVar=Unique["dummyVar"];
    lengthDenoms = Length[originalDenoms];

    (* Reconstruct the syzygy polynomial in a fresh dummy-variable space. *)
    dummyVars = Table[dummyVar[i], {i, 1, lengthDenoms}];
    syzygyPoly = ReconstructRelation[syzygy, dummyVars];

    If[syzygyPoly === $Failed,
        Return[$Failed, Module]
    ];

    (* Determine which denominator variables actually occur in the syzygy. *)
    participatingDenoms = Pick[
        Range[lengthDenoms],
        !FreeQ[syzygyPoly, #] & /@ dummyVars
    ];

    (* Keep the syzygy only if all participating denominators are still present. *)
    SubsetQ[presentIndices, participatingDenoms]
]

doesTheSyzygyContainTheDenoms::notList = 
    "Argument `1` at position `2` must be a list.";
doesTheSyzygyContainTheDenoms::emptySyzygy = 
    "The syzygy must be a non-empty list in CoefficientArrays format.";
doesTheSyzygyContainTheDenoms::badIndices = 
    "The present-indices list contains invalid entries `1` for an original denominator list of length `2`.";


(*
  FilterSyzygiesToCurrentDenoms[allSyzygies, currentDenoms, originalDenoms]

  Filters a precomputed list of syzygies, keeping only those involving
  denominators that are still present in the current expression.

  Parameters:
    allSyzygies    - List of syzygies in CoefficientArrays format, computed
                     with respect to originalDenoms.
    currentDenoms  - List of bare denominators currently present.
    originalDenoms - Original list of bare denominators used when the
                     syzygies were computed.

  Returns:
    Filtered list of syzygies involving only current denominators.

  Algorithm:
    1. Determine which original denominator indices are still present.
    2. For each syzygy, check whether all participating denominator
       variables belong to that index set.
    3. Keep only those syzygies that pass the test.

  Notes:
    - The syzygies are NOT re-indexed. They stay in the original dummy
      denominator space.
    - If currentDenoms is empty, the result is {}.
    - If allSyzygies is empty, the result is {}.
*)

ClearAll[FilterSyzygiesToCurrentDenoms]

FilterSyzygiesToCurrentDenoms[args___] := Null /; 
    !CheckArguments[FilterSyzygiesToCurrentDenoms[args], 3]

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

FilterSyzygiesToCurrentDenoms[allSyzygies_List, currentDenoms_List, originalDenoms_List] := (
    Message[FilterSyzygiesToCurrentDenoms::emptyOriginal];
    $Failed
) /; originalDenoms === {}

FilterSyzygiesToCurrentDenoms[allSyzygies_List, currentDenoms_List, originalDenoms_List] := 
    {} /; allSyzygies === {}

FilterSyzygiesToCurrentDenoms[allSyzygies_List, currentDenoms_List, originalDenoms_List] := 
    {} /; currentDenoms === {}

FilterSyzygiesToCurrentDenoms[allSyzygies_List, currentDenoms_List, originalDenoms_List] := (
    Message[
        FilterSyzygiesToCurrentDenoms::unknownDenoms,
        Complement[DeleteDuplicates[currentDenoms], originalDenoms]
    ];
    $Failed
) /; !SubsetQ[originalDenoms, DeleteDuplicates[currentDenoms]]

FilterSyzygiesToCurrentDenoms[
    allSyzygies_List,
    currentDenoms_List,
    originalDenoms_List
] := Module[
    {
        presentIndices
    },

    (* Map current denominators back to their positions in the original list. *)
    presentIndices = DeleteDuplicates @ Flatten[currentDenoms /. PositionIndex[originalDenoms]];

    Select[
        allSyzygies,
        doesTheSyzygyContainTheDenoms[#, presentIndices, originalDenoms] & 
    ]
]

FilterSyzygiesToCurrentDenoms::notList = 
    "Argument `1` at position `2` must be a list.";
FilterSyzygiesToCurrentDenoms::emptyOriginal = 
    "The originalDenoms list must not be empty.";
FilterSyzygiesToCurrentDenoms::unknownDenoms = 
    "The following current denominators are not contained in originalDenoms: `1`.";


(* ::Section:: *)
(*EliminateHomogeneousSyzygies*)


(*
  DenomAppearsInSyzygy[syzygy, denomIndex, dummyVars]

  Checks whether the denominator with index denomIndex appears in the
  given syzygy.

  Parameters:
    syzygy     - A syzygy in CoefficientArrays format.
    denomIndex - Integer index of the denominator to test.
    dummyVars  - List of dummy variables corresponding to denominators.

  Returns:
    True if dummyVars[[denomIndex]] appears in the reconstructed syzygy
    polynomial, False otherwise.

  Notes:
    - This function reconstructs the syzygy polynomial via
      ReconstructRelation[syzygy, dummyVars].
    - It does not inspect the coefficient-array structure directly.
*)

ClearAll[DenomAppearsInSyzygy]

DenomAppearsInSyzygy[args___] := Null /; !CheckArguments[DenomAppearsInSyzygy[args], 3]

DenomAppearsInSyzygy[syzygy_, denomIndex_, dummyVars_] := (
    Message[DenomAppearsInSyzygy::notList, syzygy, 1];
    $Failed
) /; !ListQ[syzygy]

DenomAppearsInSyzygy[syzygy_, denomIndex_, dummyVars_] := (
    Message[DenomAppearsInSyzygy::notInteger, denomIndex];
    $Failed
) /; !IntegerQ[denomIndex]

DenomAppearsInSyzygy[syzygy_, denomIndex_, dummyVars_] := (
    Message[DenomAppearsInSyzygy::notList, dummyVars, 3];
    $Failed
) /; !ListQ[dummyVars]

DenomAppearsInSyzygy[syzygy_, denomIndex_Integer, dummyVars_List] := (
    Message[DenomAppearsInSyzygy::emptySyzygy];
    $Failed
) /; syzygy === {}

DenomAppearsInSyzygy[syzygy_, denomIndex_Integer, dummyVars_List] := (
    Message[DenomAppearsInSyzygy::badIndex, denomIndex, Length[dummyVars]];
    $Failed
) /; denomIndex < 1 || denomIndex > Length[dummyVars]

DenomAppearsInSyzygy[syzygy_List, denomIndex_Integer, dummyVars_List] := Module[
    {
        syzygyPoly
    },

    (* Reconstruct the syzygy polynomial from CoefficientArrays format. *)
    syzygyPoly = ReconstructRelation[syzygy, dummyVars];

    (* Check whether the chosen dummy denominator variable appears. *)
    !FreeQ[syzygyPoly, dummyVars[[denomIndex]]]
]

DenomAppearsInSyzygy::notList = "Argument `1` at position `2` must be a list.";
DenomAppearsInSyzygy::notInteger = "Second argument `1` must be an integer.";
DenomAppearsInSyzygy::emptySyzygy = "The syzygy must not be an empty list.";
DenomAppearsInSyzygy::badIndex = "Denominator index `1` is out of range for a dummy variable list of length `2`.";


(*
  DeleteNonPresentDenomFromOrderedDenoms[originalDenoms, denoms, bestDenom]

  Updates the denominator ordering when some denominators have been
  eliminated during the reduction process.

  During recursive elimination, denominators can disappear. The priority
  list bestDenom refers to positions in the original denominator list,
  but the elimination step needs the positions in the current denominator
  list.

  This function:
    1. Finds which original denominators are no longer present.
    2. Removes their indices from the priority list.
    3. Re-indexes the surviving entries to match the current denominator list.

  Parameters:
    originalDenoms - Original list of bare denominators before elimination.
    denoms         - Current list of bare denominators.
    bestDenom      - Priority list, indexed with respect to originalDenoms.

  Returns:
    Updated priority list indexed with respect to denoms.
*)

ClearAll[DeleteNonPresentDenomFromOrderedDenoms]

DeleteNonPresentDenomFromOrderedDenoms[args___] := Null /; 
    !CheckArguments[DeleteNonPresentDenomFromOrderedDenoms[args], 3]

DeleteNonPresentDenomFromOrderedDenoms[originalDenoms_, denoms_, bestDenom_] := (
    Message[DeleteNonPresentDenomFromOrderedDenoms::notList, originalDenoms, 1];
    $Failed
) /; !ListQ[originalDenoms]

DeleteNonPresentDenomFromOrderedDenoms[originalDenoms_, denoms_, bestDenom_] := (
    Message[DeleteNonPresentDenomFromOrderedDenoms::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

DeleteNonPresentDenomFromOrderedDenoms[originalDenoms_, denoms_, bestDenom_] := (
    Message[DeleteNonPresentDenomFromOrderedDenoms::notList, bestDenom, 3];
    $Failed
) /; !ListQ[bestDenom]

DeleteNonPresentDenomFromOrderedDenoms[originalDenoms_List, denoms_List, bestDenom_List] := Module[
    {
        tmp, tmpBestDenom,
        indexOriginal, indexNew, indexingRule
    },

    (* Find denominators that are no longer present. *)
    tmp = Complement[originalDenoms, denoms];
    
    (* Get positions of missing denominators in the original list. *)
    tmp = tmp /. PositionIndex[originalDenoms];
    tmp = Flatten[tmp];
    
    (* Remove missing denominators from priority list. *)
    tmpBestDenom = DeleteCases[bestDenom, Alternatives @@ tmp];

    (* Re-index to match current denominator positions. *)
    indexOriginal = PositionIndex[originalDenoms];
    indexNew = PositionIndex[denoms] // Normal;
    indexingRule = indexNew /. indexOriginal /. {arg_} :> arg;

    tmpBestDenom = tmpBestDenom /. indexingRule;

    tmpBestDenom
]

DeleteNonPresentDenomFromOrderedDenoms::notList = "Argument `1` at position `2` must be a list.";


(*
  EliminateHomogeneousSyzygies[expr, vars, allHomSyzygies, originalDenoms, denomOrdering, dummyD]

  Recursively eliminates homogeneous syzygies using a pre-computed list.

  A homogeneous syzygy has zero constant term and is used to reduce the
  number of distinct denominator factors in the expression.

  Parameters:
    expr           - The expression to reduce.
    vars           - List of variables.
    allHomSyzygies - Pre-computed list of homogeneous syzygies.
    originalDenoms - Original bare denominators used to compute the syzygies.
    denomOrdering  - List of the form {priorityList, originalDenoms}, where
                     priorityList determines which denominator should be
                     preferred for elimination.
    dummyD         - Symbol used to create dummy denominator variables.

  Returns:
    Expression with homogeneous syzygies eliminated.

  Notes:
    - Sums are processed term by term.
    - The recursive structure is analogous to EliminateNullRelations:
      map to denominator space, construct a one-operator, expand, map back,
      recurse.
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

(* For sums: map over each additive term independently. *)
EliminateHomogeneousSyzygies[
    expr_Plus,
    vars_List,
    allHomSyzygies_List,
    originalDenoms_List,
    denomOrdering_List,
    dummyD_Symbol
] :=
    Map[
        EliminateHomogeneousSyzygies[#, vars, allHomSyzygies, originalDenoms, denomOrdering, dummyD] &,
        expr
    ]

(* Base case: expression free of all variables. *)
EliminateHomogeneousSyzygies[
    expr_,
    vars_List,
    allHomSyzygies_List,
    originalDenoms_List,
    denomOrdering_List,
    dummyD_Symbol
] :=
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Main recursive definition. *)
EliminateHomogeneousSyzygies[
    expr_,
    vars_List,
    allHomSyzygies_List,
    originalDenoms_List,
    denomOrdering_List,
    dummyD_Symbol
] := Module[
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

    (* Check if denominator is a single term (not Times). *)
    If[Head[Denominator[expr]] =!= Times,
        Return[expr]
    ];

    (* Get current bare denominators. *)
    denoms = GetBareDenoms[expr, vars];
    k = Length[denoms];

    (* Base case: fewer denominators than variables + 1, nothing to reduce. *)
    If[k <= Length[vars], Return[expr]];

    (* Filter syzygies to those relevant for current denominators. *)
    relevantSyzygies = FilterSyzygiesToCurrentDenoms[allHomSyzygies, denoms, originalDenoms];

    (* Check for filtering failure. *)
    If[relevantSyzygies === $Failed,
        Message[EliminateHomogeneousSyzygies::filterFailed];
        Return[$Failed]
    ];

    (* Base case: no relevant syzygies remain. *)
    If[relevantSyzygies === {}, Return[expr]];

    (* Update denominator ordering. *)
    bestDenom = DeleteNonPresentDenomFromOrderedDenoms[originalDenoms, denoms, bestDenom];

    (* Create dummy variables and mappings. *)
    dummyVars = Table[dummyD[i], {i, Length[originalDenoms]}];
    denomMapping = MapThread[Rule, {originalDenoms, dummyVars}];
    reversedDenomMapping = MapThread[Rule, {dummyVars, originalDenoms}];

    (* Map expression to denominator space. *)
    mappedExpr = expr /. denomMapping;

    (* Select best syzygy based on denominator ordering. *)
    tmp = Table[
        {
            Select[relevantSyzygies, DenomAppearsInSyzygy[#, bestDenom[[i]], dummyVars] &],
            bestDenom[[i]]
        },
        {i, Length[bestDenom]}
    ] // DeleteCases[#, {{}, _}] &;

    (* If no syzygy contains any preferred denominator, return expr. *)
    If[tmp === {}, Return[expr]];

    (* Take first available syzygy and chosen denominator. *)
    syzygy = tmp[[1, 1, 1]];
    chosenDenom = tmp[[1, 2]];

    (* Reconstruct syzygy polynomial. *)
    syzygyPoly = ReconstructRelation[syzygy, dummyVars];

    (* Extract coefficient of chosen denominator. *)
    coefficient = Coefficient[syzygyPoly, dummyVars[[chosenDenom]]];

    (* Build replacement rule. *)
    replacementRule = -(syzygyPoly - coefficient * dummyVars[[chosenDenom]]) / coefficient;

    (* Construct the one-operator. *)
    oneOperator = replacementRule / dummyVars[[chosenDenom]];

    (* Multiply expression by the one-operator and expand. *)
    tmp = mappedExpr * oneOperator // Expand;

    (* Map back to variable space and recurse. *)
    tmp /. reversedDenomMapping //
        EliminateHomogeneousSyzygies[#, vars, allHomSyzygies, originalDenoms, {bestDenom, originalDenoms}, dummyD] &
]

EliminateHomogeneousSyzygies::notList = "Argument `1` at position `2` must be a list.";
EliminateHomogeneousSyzygies::notSymbol = "Sixth argument `1` must be a Symbol.";
EliminateHomogeneousSyzygies::badOrdering = "denomOrdering `1` must be a list of length 2: {priorityList, originalDenoms}.";
EliminateHomogeneousSyzygies::filterFailed = "Filtering syzygies to current denominators failed.";


(* ::Section:: *)
(*EliminateInhomogeneousSyzygies*)


(*
  EliminateInhomogeneousSyzygies[expr, vars, allInhomSyzygies, originalDenoms, dummyD]

  Recursively eliminates inhomogeneous syzygies using a pre-computed list.

  Instead of recomputing syzygies at each recursive step, this function
  filters the pre-computed list to the denominators currently present in
  the expression.

  Mathematical role:
    For an inhomogeneous syzygy of the form
      c + p(D_1, ..., D_k) = 0
    with c != 0, one obtains the one-operator
      1 = -(1/c) p(D_1, ..., D_k),
    which is multiplied into the expression.

  Parameters:
    expr             - The expression to reduce.
    vars             - List of variables.
    allInhomSyzygies - Pre-computed list of inhomogeneous syzygies in
                       CoefficientArrays format.
    originalDenoms   - Original bare denominators used to compute syzygies.
    dummyD           - Symbol used to create dummy denominator variables.

  Returns:
    Expression with all applicable inhomogeneous syzygies eliminated.

  Notes:
    - Sums are processed term by term.
    - The first relevant inhomogeneous syzygy is used at each step.
    - The core recursive logic is intentionally kept unchanged.
*)

ClearAll[EliminateInhomogeneousSyzygies]

EliminateInhomogeneousSyzygies[args___] := Null /; 
    !CheckArguments[EliminateInhomogeneousSyzygies[args], 5]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::emptyList, "vars"];
    $Failed
) /; vars === {}

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, allInhomSyzygies, 3];
    $Failed
) /; !ListQ[allInhomSyzygies]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notList, originalDenoms, 4];
    $Failed
) /; !ListQ[originalDenoms]

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::emptyList, "originalDenoms"];
    $Failed
) /; originalDenoms === {}

EliminateInhomogeneousSyzygies[expr_, vars_, allInhomSyzygies_, originalDenoms_, dummyD_] := (
    Message[EliminateInhomogeneousSyzygies::notSymbol, dummyD];
    $Failed
) /; !MatchQ[dummyD, _Symbol]

(* For sums: map over each additive term independently. *)
EliminateInhomogeneousSyzygies[
    expr_Plus,
    vars_List,
    allInhomSyzygies_List,
    originalDenoms_List,
    dummyD_Symbol
] :=
    Map[
        EliminateInhomogeneousSyzygies[#, vars, allInhomSyzygies, originalDenoms, dummyD] &,
        expr
    ]

(* Base case: expression free of all variables. *)
EliminateInhomogeneousSyzygies[
    expr_,
    vars_List,
    allInhomSyzygies_List,
    originalDenoms_List,
    dummyD_Symbol
] :=
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Main recursive definition. *)
EliminateInhomogeneousSyzygies[
    expr_,
    vars_List,
    allInhomSyzygies_List,
    originalDenoms_List,
    dummyD_Symbol
] := Module[
    {
        tmp,
        k, denoms, dummyVars,
        relevantSyzygies, syzygyWithoutConstant,
        denomMapping, reversedDenomMapping,
        mappedExpr,
        syzygy, constant, oneOperator
    },

    (* Check if denominator is a single term (not Times). *)
    If[Head[Denominator[expr]] =!= Times,
        Return[expr]
    ];

    (* Get current bare denominators. *)
    denoms = GetBareDenoms[expr, vars];
    
    If[denoms === $Failed,
        Message[EliminateInhomogeneousSyzygies::denomExtractionFailed];
        Return[$Failed]
    ];
    
    k = Length[denoms];

    (* Base case: fewer than or equal to n denominators, nothing to reduce. *)
    If[k <= Length[vars], Return[expr]];

    (* Filter syzygies to those relevant for current denominators. *)
    relevantSyzygies = FilterSyzygiesToCurrentDenoms[
        allInhomSyzygies,
        denoms,
        originalDenoms
    ];

    If[relevantSyzygies === $Failed,
        Message[EliminateInhomogeneousSyzygies::filterFailed];
        Return[$Failed]
    ];

    (* Base case: no relevant syzygies remain. *)
    If[relevantSyzygies === {}, Return[expr]];

    (* Create dummy variables and mappings. *)
    dummyVars = Table[dummyD[i], {i, Length[originalDenoms]}];
    denomMapping = MapThread[Rule, {originalDenoms, dummyVars}];
    reversedDenomMapping = MapThread[Rule, {dummyVars, originalDenoms}];

    (* Map expression to denominator space. *)
    mappedExpr = expr /. denomMapping;

    (* Take first relevant syzygy. *)
    syzygy = relevantSyzygies[[1]];
    
    If[!ListQ[syzygy] || syzygy === {},
        Message[EliminateInhomogeneousSyzygies::badSyzygy, syzygy];
        Return[$Failed]
    ];
    
    constant = syzygy[[1]];
    
    (* Safety check: inhomogeneous syzygies must have nonzero constant term. *)
    If[constant === 0,
        Message[EliminateInhomogeneousSyzygies::zeroConstant, syzygy];
        Return[$Failed]
    ];

    syzygyWithoutConstant = ReplacePart[syzygy, 1 -> 0];

    oneOperator = -1/constant * ReconstructRelation[syzygyWithoutConstant, dummyVars];
    
    If[oneOperator === $Failed,
        Message[EliminateInhomogeneousSyzygies::reconstructFailed, syzygy];
        Return[$Failed]
    ];

    (* Multiply expression by one-operator and expand. *)
    tmp = mappedExpr * oneOperator // Expand;

    (* Map back to variable space and recurse. *)
    tmp /. reversedDenomMapping //
        EliminateInhomogeneousSyzygies[#, vars, allInhomSyzygies, originalDenoms, dummyD] &
]

EliminateInhomogeneousSyzygies::notList = "Argument `1` at position `2` must be a list.";
EliminateInhomogeneousSyzygies::emptyList = "The `1` list must not be empty.";
EliminateInhomogeneousSyzygies::varsNotSymbols = "The following variables are not Symbols: `1`.";
EliminateInhomogeneousSyzygies::notSymbol = "Fifth argument `1` must be a Symbol.";
EliminateInhomogeneousSyzygies::filterFailed = "Filtering syzygies to current denominators failed.";
EliminateInhomogeneousSyzygies::denomExtractionFailed = "Extraction of current denominators failed.";
EliminateInhomogeneousSyzygies::badSyzygy = "Encountered malformed syzygy `1`.";
EliminateInhomogeneousSyzygies::zeroConstant = "Encountered a syzygy with vanishing constant term in inhomogeneous elimination: `1`.";
EliminateInhomogeneousSyzygies::reconstructFailed = "Reconstruction of the syzygy polynomial failed for `1`.";
