(* ::Package:: *)

(* ::Section::Closed:: *)
(*GroebnerApartOrdering*)


(*
  GroebnerApartOrdering[denoms, vars, qVars]

  Constructs the block ordering used by the Gr\[ODoubleDot]bner-based multivariate
  partial fraction algorithm from MultivariateApart.

  The ordering is built as follows:

    1. Group denominator factors by the set of variables they depend on.
    2. Sort these groups by the number of variables they depend on:
         more variables  -> earlier block
    3. Within each group, sort by degree measure:
         higher degree   -> earlier in the block
    4. Replace each denominator by its corresponding inverse variable q_i.
    5. Append the original variables as the final block.

  Parameters:
    denoms - List of denominator polynomials {d1, ..., dm}
    vars   - List of original variables {x1, ..., xn}
    qVars  - List of inverse variables {q1, ..., qm}

  Returns:
    A list of blocks, suitable for constructing a variable ordering, e.g.
      {{q3, q1}, {q2}, {q4}, {x, y}}

  Example:
    denoms = {x^2 + y, x - y, x + 1, x^2 - 3, y + 1, y};
    vars   = {x, y};
    qVars  = {q1, q2, q3, q4, q5, q6};

    BuildApartOrdering[denoms, vars, qVars]
      -> {{q1, q2}, {q4, q3}, {q5, q6}, {x, y}}

  Notes:
    - Denominators depending on more variables are considered "greater"
      and therefore placed in earlier blocks.
    - Within one dependence group, larger degree comes first.
    - The final block always consists of the original variables vars.
    - This function only builds the ordering data structure. It does not
      compute any Gr\[ODoubleDot]bner basis by itself.
*)

ClearAll[GroebnerApartOrdering]

GroebnerApartOrdering[args___] := Null /; !CheckArguments[GroebnerApartOrdering[args], 3]

GroebnerApartOrdering[denoms_, vars_, qVars_] := (
    Message[GroebnerApartOrdering::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

GroebnerApartOrdering[denoms_, vars_, qVars_] := (
    Message[GroebnerApartOrdering::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

GroebnerApartOrdering[denoms_, vars_, qVars_] := (
    Message[GroebnerApartOrdering::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

GroebnerApartOrdering[denoms_List, vars_List, qVars_List] := (
    Message[GroebnerApartOrdering::emptyList, "vars"];
    $Failed
) /; Length[vars] === 0

GroebnerApartOrdering[denoms_List, vars_List, qVars_List] := (
    Message[GroebnerApartOrdering::lengthMismatch, Length[denoms], Length[qVars]];
    $Failed
) /; Length[denoms] =!= Length[qVars]

GroebnerApartOrdering[denoms_List, vars_List, qVars_List] := (
    Message[GroebnerApartOrdering::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

GroebnerApartOrdering[denoms_List, vars_List, qVars_List] := (
    Message[GroebnerApartOrdering::qVarsNotSymbols, Select[qVars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ qVars)

(* Edge case: no denominators, only the original variables remain. *)
GroebnerApartOrdering[{}, vars_List, {}] := {vars}

GroebnerApartOrdering[denoms_List, vars_List, qVars_List] := Module[
    {
        denomData,
        groups,
        sortedGroups,
        qBlocks
    },

    (*
      For each denominator store:
        1. the subset of vars it depends on,
        2. its degree measure,
        3. its corresponding q-variable.
    *)
    denomData = MapThread[
        {
            Select[vars, !FreeQ[#1, #] &],
            Total[Exponent[#1, vars]],
            #2
        } &,
        {denoms, qVars}
    ];

    (*
      Group denominators by their variable dependence pattern.
      Each group consists of entries of the form:
        {varSubset, degreeMeasure, qVar}
    *)
    groups = GatherBy[denomData, First];

    (*
      Sort the groups by the number of variables they depend on.
      More variables -> earlier block.
    *)
    sortedGroups = SortBy[groups, -Length[#[[1, 1]]] &];

    (*
      Within each group, sort by degree measure.
      Larger degree -> earlier in the block.
      Then keep only the q-variables.
    *)
    qBlocks = Map[
        SortBy[#, -#[[2]] &][[All, 3]] &,
        sortedGroups
    ];

    (* Append the original variables as the final block. *)
    Append[qBlocks, vars]
]

GroebnerApartOrdering::notList = "Argument `1` at position `2` must be a list.";
GroebnerApartOrdering::emptyList = "The `1` list must not be empty.";
GroebnerApartOrdering::lengthMismatch = "Length of denoms (`1`) must equal length of qVars (`2`).";
GroebnerApartOrdering::varsNotSymbols = "The following entries in vars are not Symbols: `1`.";
GroebnerApartOrdering::qVarsNotSymbols = "The following entries in qVars are not Symbols: `1`.";


(* ::Section::Closed:: *)
(*GroebnerApartIdeal*)


(*
  GroebnerApartIdeal[denoms, qVars]

  Constructs the generators of the Gr\[ODoubleDot]bner-apart ideal.

  For denominator factors d_1, ..., d_m and inverse variables q_1, ..., q_m,
  the ideal is

      I = < q_1 d_1 - 1, ..., q_m d_m - 1 >.

  Setting these generators equal to zero imposes the relations

      q_i = 1 / d_i.

  Parameters:
    denoms - List of denominator polynomials {d1, ..., dm}
    qVars  - List of inverse variables {q1, ..., qm}

  Returns:
    A list of ideal generators

      {q1 d1 - 1, ..., qm dm - 1}

  Example:
    GroebnerApartIdeal[{x + y, x - y}, {q1, q2}]
      -> {q1 (x + y) - 1, q2 (x - y) - 1}

  Notes:
    - This function only builds the ideal generators.
    - It does not compute the Gr\[ODoubleDot]bner basis itself.
*)

ClearAll[GroebnerApartIdeal]

GroebnerApartIdeal[args___] := Null /; !CheckArguments[GroebnerApartIdeal[args], 2]

GroebnerApartIdeal[denoms_, qVars_] := (
    Message[GroebnerApartIdeal::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

GroebnerApartIdeal[denoms_, qVars_] := (
    Message[GroebnerApartIdeal::notList, qVars, 2];
    $Failed
) /; !ListQ[qVars]

GroebnerApartIdeal[denoms_List, qVars_List] := (
    Message[GroebnerApartIdeal::lengthMismatch, Length[denoms], Length[qVars]];
    $Failed
) /; Length[denoms] =!= Length[qVars]

GroebnerApartIdeal[{}, {}] := {}

GroebnerApartIdeal[denoms_List, qVars_List] := 
    MapThread[#1 * #2 - 1 &, {qVars, denoms}]

GroebnerApartIdeal::notList = "Argument `1` at position `2` must be a list.";
GroebnerApartIdeal::lengthMismatch = "Length of denoms (`1`) must equal length of qVars (`2`).";


(* ::Section::Closed:: *)
(*GroebnerBackSubstitute*)


(*
  GroebnerBackSubstitute[expr, denoms, qVars]

  Converts an expression from q-space back to ordinary rational-function form.

  In q-space, the inverse denominator variables q_i represent

      q_i = 1 / d_i.

  This function applies the substitution rules

      q_i -> 1 / d_i

  to the input expression.

  Parameters:
    expr   - Expression in q-space.
    denoms - List of denominator polynomials {d1, ..., dm}.
    qVars  - List of inverse denominator variables {q1, ..., qm}.

  Returns:
    The expression with q-variables replaced by inverse denominator factors.

  Example:
    GroebnerBackSubstitute[
        2 q1 q3^2 + q2 q3^2,
        {x - y, y, x + y},
        {q1, q2, q3}
    ]
      -> 2/((x - y) (x + y)^2) + 1/(y (x + y)^2)

  Notes:
    - This function performs only the substitution q_i -> 1/d_i.
    - It does not simplify, combine, or collect the result.
*)

ClearAll[GroebnerBackSubstitute]

GroebnerBackSubstitute[args___] := Null /; !CheckArguments[GroebnerBackSubstitute[args], 3]

GroebnerBackSubstitute[expr_, denoms_, qVars_] := (
    Message[GroebnerBackSubstitute::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

GroebnerBackSubstitute[expr_, denoms_, qVars_] := (
    Message[GroebnerBackSubstitute::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

GroebnerBackSubstitute[expr_, denoms_List, qVars_List] := (
    Message[GroebnerBackSubstitute::lengthMismatch, Length[denoms], Length[qVars]];
    $Failed
) /; Length[denoms] =!= Length[qVars]

GroebnerBackSubstitute[expr_, {}, {}] := expr

GroebnerBackSubstitute[expr_, denoms_List, qVars_List] := 
    expr /. MapThread[#1 -> 1/#2 &, {qVars, denoms}]

GroebnerBackSubstitute::notList = "Argument `1` at position `2` must be a list.";
GroebnerBackSubstitute::lengthMismatch = "Length of denoms (`1`) must equal length of qVars (`2`).";


(* ::Section::Closed:: *)
(*ExprToApartForm*)


(*
  GroebnerApartConvertFactor[factor, denoms, qVars]

  Converts a single denominator factor into q-space.

  A factor base^pow is matched against the list denoms. If a match is found,
  it is replaced by the corresponding q-variable power q_i^pow. Matching is
  done by polynomial equivalence via Expand, first directly and then up to
  an overall sign.

  Parameters:
    factor - A single denominator factor, either base or base^pow.
    denoms - List of denominator polynomials {d1, ..., dm}.
    qVars  - List of inverse denominator variables {q1, ..., qm}.

  Returns:
    q_i^pow if the factor matches d_i,
    (-1)^pow q_i^pow if the factor matches -d_i,
    1/factor if no match is found.
*)

ClearAll[GroebnerApartConvertFactor]

GroebnerApartConvertFactor[args___] := Null /; !CheckArguments[GroebnerApartConvertFactor[args], 3]

GroebnerApartConvertFactor[factor_, denoms_, qVars_] := (
    Message[GroebnerApartConvertFactor::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

GroebnerApartConvertFactor[factor_, denoms_, qVars_] := (
    Message[GroebnerApartConvertFactor::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

GroebnerApartConvertFactor[factor_, denoms_List, qVars_List] := (
    Message[GroebnerApartConvertFactor::lengthMismatch, Length[denoms], Length[qVars]];
    $Failed
) /; Length[denoms] =!= Length[qVars]

GroebnerApartConvertFactor[factor_, denoms_List, qVars_List] := Module[
    {
        base, pow,
        posMatches, index
    },

    {base, pow} = If[Head[factor] === Power,
        {factor[[1]], factor[[2]]},
        {factor, 1}
    ];

    (* First try an exact polynomial match. *)
    posMatches = Position[
        Expand[# - base] === 0 & /@ denoms,
        True
    ];

    If[posMatches =!= {},
        index = posMatches[[1, 1]];
        Return[qVars[[index]]^pow, Module]
    ];

    (* Then try matching up to an overall minus sign. *)
    posMatches = Position[
        Expand[# + base] === 0 & /@ denoms,
        True
    ];

    If[posMatches =!= {},
        index = posMatches[[1, 1]];
        Return[(-1)^pow * qVars[[index]]^pow, Module]
    ];

    (* If not found, leave the factor untouched. *)
    1/factor
]

GroebnerApartConvertFactor::notList = "Argument `1` at position `2` must be a list.";
GroebnerApartConvertFactor::lengthMismatch = "Length of denoms (`1`) must equal length of qVars (`2`).";


(*
  ExprToApartForm[expr, denoms, qVars]
  ExprToApartForm[expr, denoms, qVars, "ReturnPieces" -> True|False]

  Converts a rational expression into q-space.

  By default, the result is returned as a single q-space expression.
  With "ReturnPieces" -> True, the result is returned as

      {numerator, qFactorList}

  which is useful for iterated Gr\[ODoubleDot]bner reduction.

  Parameters:
    expr   - Rational expression.
    denoms - List of denominator polynomials {d1, ..., dm}.
    qVars  - List of inverse denominator variables {q1, ..., qm}.

  Option:
    "ReturnPieces" -> False (default)
      False : return numerator * Product[q-factors]
      True  : return {numerator, qFactorList}

  Examples:
    ExprToApartForm[
        1/((x - y) y (x + y)),
        {x - y, y, x + y},
        {q1, q2, q3}
    ]
      -> q1 q2 q3

    ExprToApartForm[
        (2 x - y)/((x - y)^2 y),
        {x - y, y},
        {q1, q2},
        "ReturnPieces" -> True
    ]
      -> {2 x - y, {q1^2, q2}}
*)

ClearAll[ExprToApartForm]

Options[ExprToApartForm] = {
    "ReturnPieces" -> False
};

ExprToApartForm[args___] := Null /; !CheckArguments[ExprToApartForm[args], {3, 4}]

ExprToApartForm[expr_, denoms_, qVars_, options:OptionsPattern[]] := (
    Message[ExprToApartForm::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

ExprToApartForm[expr_, denoms_, qVars_, options:OptionsPattern[]] := (
    Message[ExprToApartForm::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

ExprToApartForm[expr_, denoms_List, qVars_List, options:OptionsPattern[]] := (
    Message[ExprToApartForm::lengthMismatch, Length[denoms], Length[qVars]];
    $Failed
) /; Length[denoms] =!= Length[qVars]

ExprToApartForm[expr_, denoms_List, qVars_List, options:OptionsPattern[]] := (
    Message[ExprToApartForm::qVarsNotSymbols, Select[qVars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ qVars)

ExprToApartForm[expr_, denoms_List, qVars_List, options:OptionsPattern[]] := (
    Message[ExprToApartForm::wrongOption, "ReturnPieces"];
    $Failed
) /; !MemberQ[{True, False}, OptionValue["ReturnPieces"]]

ExprToApartForm[expr_, {}, {}, options:OptionsPattern[]] := 
    If[OptionValue["ReturnPieces"], {expr, {}}, expr]

ExprToApartForm[expr_, denoms_List, qVars_List, options:OptionsPattern[]] := Module[
    {
        num, den, denFactors,
        convertedFactors
    },

    num = Numerator[expr];
    den = Denominator[expr];

    If[den === 1,
        Return[
            If[OptionValue["ReturnPieces"],
                {num, {}},
                num
            ],
            Module
        ]
    ];

    (* Split denominator into top-level factors. *)
    denFactors = If[Head[den] === Times, List @@ den, {den}];

    (* Convert each factor to q-space. *)
    convertedFactors = GroebnerApartConvertFactor[#, denoms, qVars] & /@ denFactors;

    If[MemberQ[convertedFactors, $Failed],
        Return[$Failed, Module]
    ];

    If[OptionValue["ReturnPieces"],
        {num, convertedFactors},
        num * Times @@ convertedFactors
    ]
]

ExprToApartForm::notList = "Argument `1` at position `2` must be a list.";
ExprToApartForm::lengthMismatch = "Length of denoms (`1`) must equal length of qVars (`2`).";
ExprToApartForm::qVarsNotSymbols = "The following entries in qVars are not Symbols: `1`.";
ExprToApartForm::wrongOption = "Problem with option `1`, OptionName or OptionValue not recognized.";


(* ::Section:: *)
(*GroebnerReduce*)


(*
  GroebnerReduceOrderFactors[qFactors, varOrder]

  Sorts q-factors according to the first variable from varOrder that
  appears in each factor.

  This is used by the iterated Gr\[ODoubleDot]bner reduction strategy, where the
  q-factors are multiplied in one at a time in a deterministic order.

  Parameters:
    qFactors - List of q-factors such as {q1^2, q3, q2^4}
    varOrder - Flattened variable ordering used for PolynomialReduce

  Returns:
    The q-factors sorted according to the variable ordering.

  Notes:
    - The first variable in varOrder with positive exponent in the factor
      determines the factor's position.
    - This helper does not perform any reduction itself.
*)

ClearAll[GroebnerReduceOrderFactors]

GroebnerReduceOrderFactors[args___] := Null /; !CheckArguments[GroebnerReduceOrderFactors[args], 2]

GroebnerReduceOrderFactors[qFactors_, varOrder_] := (
    Message[GroebnerReduceOrderFactors::notList, qFactors, 1];
    $Failed
) /; !ListQ[qFactors]

GroebnerReduceOrderFactors[qFactors_, varOrder_] := (
    Message[GroebnerReduceOrderFactors::notList, varOrder, 2];
    $Failed
) /; !ListQ[varOrder]

GroebnerReduceOrderFactors[qFactors_List, varOrder_List] := (
    Message[GroebnerReduceOrderFactors::emptyList, "varOrder"];
    $Failed
) /; Length[varOrder] === 0

GroebnerReduceOrderFactors[qFactors_List, varOrder_List] := 
    SortBy[
        qFactors,
        First @ FirstPosition[
            varOrder,
            var_ /; Exponent[#, var] > 0
        ] &
    ]

GroebnerReduceOrderFactors::notList = "Argument `1` at position `2` must be a list.";
GroebnerReduceOrderFactors::emptyList = "The `1` list must not be empty.";


(*
  GroebnerReduce[expr, groebnerBasis, varOrder]
  GroebnerReduce[expr, groebnerBasis, varOrder, options]

  Reduces an expression in q-space with respect to a Gr\[ODoubleDot]bner basis.

  Two reduction modes are supported:

    1. Single-shot reduction (default):
         the full expression expr is reduced at once.

    2. Iterated reduction:
         expr is treated as the numerator part, a list of q-factors is
         supplied via the option "QFactors", and the factors are multiplied
         in one at a time, reducing after each multiplication.

  Parameters:
    expr          - Expression in q-space.
                    In non-iterated mode, this is the full q-space expression.
                    In iterated mode, this is the numerator part.
    groebnerBasis - Gr\[ODoubleDot]bner basis of the ideal
                      <q_1 d_1 - 1, ..., q_m d_m - 1>.
    varOrder      - Flattened variable ordering used consistently with the
                    Gr\[ODoubleDot]bner basis computation.

  Options:
    "Iterated" -> True|False
      False : reduce expr in one shot
      True  : use staged reduction with q-factors

    "QFactors" -> Automatic | list
      In iterated mode, this must be the list of q-factors to multiply in.

  Returns:
    The reduced remainder modulo the Gr\[ODoubleDot]bner basis.

  Examples:
    GroebnerReduce[q1 q2 q3, gb, {q3, q1, q2, x, y}]
      -> one-shot reduction

    GroebnerReduce[
        num,
        gb,
        {q3, q1, q2, x, y},
        "Iterated" -> True,
        "QFactors" -> {q1, q2, q3}
    ]
      -> staged reduction

  Notes:
    - In single-shot mode, the function returns only the remainder from
      PolynomialReduce.
    - In iterated mode, the same reduction is performed incrementally.
    - The block structure is encoded through varOrder itself.
*)

ClearAll[GroebnerReduce]

Options[GroebnerReduce] = {
    "Iterated" -> False,
    "QFactors" -> Automatic
};

GroebnerReduce[args___] := Null /; !CheckArguments[GroebnerReduce[args], {3, 5}]

GroebnerReduce[expr_, groebnerBasis_, varOrder_, options:OptionsPattern[]] := (
    Message[GroebnerReduce::notList, groebnerBasis, 2];
    $Failed
) /; !ListQ[groebnerBasis]

GroebnerReduce[expr_, groebnerBasis_, varOrder_, options:OptionsPattern[]] := (
    Message[GroebnerReduce::notList, varOrder, 3];
    $Failed
) /; !ListQ[varOrder]

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := (
    Message[GroebnerReduce::emptyList, "varOrder"];
    $Failed
) /; Length[varOrder] === 0

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := (
    Message[GroebnerReduce::wrongOption, "Iterated"];
    $Failed
) /; !MemberQ[{True, False}, OptionValue["Iterated"]]

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := (
    Message[GroebnerReduce::wrongOption, "QFactors"];
    $Failed
) /; OptionValue["QFactors"] =!= Automatic && !ListQ[OptionValue["QFactors"]]

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := (
    Message[GroebnerReduce::missingQFactors];
    $Failed
) /; OptionValue["Iterated"] && OptionValue["QFactors"] === Automatic

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := (
    Message[GroebnerReduce::unexpectedQFactors];
    $Failed
) /; !OptionValue["Iterated"] && OptionValue["QFactors"] =!= Automatic

(* Single-shot reduction. *)
GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] :=
    Last @ PolynomialReduce[
        expr,
        groebnerBasis,
        varOrder,
        MonomialOrder -> DegreeReverseLexicographic,
        CoefficientDomain -> RationalFunctions
    ] /; !OptionValue["Iterated"]

(* Iterated reduction. *)
GroebnerReduce[expr_, groebnerBasis_List, varOrder_List, options:OptionsPattern[]] := Module[
    {
        orderedFactors
    },

    orderedFactors = GroebnerReduceOrderFactors[OptionValue["QFactors"], varOrder];

    If[orderedFactors === $Failed,
        Return[$Failed, Module]
    ];

    Fold[
        GroebnerReduce[Expand[#1 * #2], groebnerBasis, varOrder] &,
        expr,
        orderedFactors
    ]
] /; OptionValue["Iterated"]

GroebnerReduce::notList = "Argument `1` at position `2` must be a list.";
GroebnerReduce::emptyList = "The `1` list must not be empty.";
GroebnerReduce::wrongOption = "Problem with option `1`, OptionName or OptionValue not recognized.";
GroebnerReduce::missingQFactors = "Iterated Gr\[ODoubleDot]bner reduction requires the option \"QFactors\" to be set to a list of q-factors.";
GroebnerReduce::unexpectedQFactors = "Option \"QFactors\" may only be used together with \"Iterated\" -> True.";


(* ::Section::Closed:: *)
(*MakeParametersSymbolic*)


(*
  MakeParametersSymbolic[expr, vars]

  Replaces parameter-dependent but var-independent non-polynomial
  sub-expressions by temporary symbols.

  The purpose of this function is to turn an expression into a genuine
  polynomial in the variables vars together with a finite set of fresh
  parameter symbols. This is useful before Gr\[ODoubleDot]bner-basis computations,
  where objects such as

      1/(1 + b),  Sqrt[s t],  Beta[p, q],  ...

  should be treated as coefficient-like objects rather than as part of
  the polynomial structure in vars.

  Parameters:
    expr - Expression to process.
    vars - List of polynomial variables.

  Returns:
    {newExpr, forwardRules, backwardRules}

    where:
      - newExpr        is expr with problematic parameter pieces replaced
                       by fresh symbols par$...
      - forwardRules   maps original sub-expressions to temporary symbols
      - backwardRules  maps temporary symbols back to the original pieces

  Example:
    MakeParametersSymbolic[
        x + Sqrt[s t] y + z/(1 + b),
        {x, y}
    ]

    may return something like

      {
        x + par$1 y + z par$2,
        {Sqrt[s t] -> par$1, 1/(1 + b) -> par$2},
        {par$1 -> Sqrt[s t], par$2 -> 1/(1 + b)}
      }

  Notes:
    - Only sub-expressions free of vars are replaced.
    - Replacement proceeds from more complicated sub-expressions to
      simpler ones in order to avoid partial replacement.
    - This function does not attempt algebraic simplification; it only
      introduces temporary symbols.
*)

ClearAll[MakeParametersSymbolic]

MakeParametersSymbolic[args___] := Null /; !CheckArguments[MakeParametersSymbolic[args], 2]

MakeParametersSymbolic[expr_, vars_] := (
    Message[MakeParametersSymbolic::notList, vars];
    $Failed
) /; !ListQ[vars]

MakeParametersSymbolic[expr_, vars_List] := (
    Message[MakeParametersSymbolic::emptyVars];
    $Failed
) /; Length[vars] === 0

MakeParametersSymbolic[expr_, vars_List] := (
    Message[MakeParametersSymbolic::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

MakeParametersSymbolic[expr_, vars_List] := Module[
    {
        expanded,
        subExprs,
        replacementData,
        newExpr,
        forwardRules,
        backwardRules
    },

    (*
      Work with the expression as given. The function only scans for
      problematic var-free parameter sub-expressions.
    *)
    expanded = expr;

    (*
      Find compound sub-expressions that are free of vars and are not
      already simple polynomial-safe atoms.
    *)
    subExprs = Cases[
        expanded,
        s_ /; (
            FreeQ[s, Alternatives @@ vars] &&
            !AtomQ[s] &&
            !MatchQ[s, _Integer | _Rational | _Symbol]
        ),
        {0, Infinity},
        Heads -> False
    ];

    subExprs = DeleteDuplicates[subExprs];

    (*
      Keep only those var-free sub-expressions that can cause
      non-polynomial behavior in coefficient space.
    *)
    subExprs = Select[
        subExprs,
        MatchQ[
            #,
            Power[_, _?Negative] |
            Power[_, _Rational] |
            Power[_, _?(Not@*IntegerQ)] |
            _Beta |
            _Sqrt |
            HoldPattern[1/(_)]
        ] &
    ];

    (* Nothing to replace. *)
    If[subExprs === {},
        Return[{expanded, {}, {}}, Module]
    ];

    (*
      Replace more complicated sub-expressions first in order to avoid
      partial replacement of larger objects by their smaller parts.
    *)
    subExprs = SortBy[subExprs, -LeafCount[#] &];

    replacementData = Fold[
        Module[
            {
                currentExpr = #[[1]],
                currentForward = #[[2]],
                currentBackward = #[[3]],
                sub = #2,
                tmp
            },

            If[FreeQ[currentExpr, sub],
                Return[{currentExpr, currentForward, currentBackward}, Module]
            ];

            tmp = Unique["par"];

            {
                currentExpr /. sub -> tmp,
                Append[currentForward, sub -> tmp],
                Append[currentBackward, tmp -> sub]
            }
        ] &,
        {expanded, {}, {}},
        subExprs
    ];

    {newExpr, forwardRules, backwardRules} = replacementData;

    {newExpr, forwardRules, backwardRules}
]

MakeParametersSymbolic::notList = "Second argument `1` must be a list of variables.";
MakeParametersSymbolic::emptyVars = "Variable list must not be empty.";
MakeParametersSymbolic::varsNotSymbols = "The following variables are not Symbols: `1`.";


(* ::Section:: *)
(*GroebnerApart*)


(*
  NeedsGroebnerReductionQ[expr, vars]

  Checks whether an expression has strictly more variable-dependent bare
  denominator factors than variables.

  In the Gr\[ODoubleDot]bner-based multivariate partial fraction approach, terms with
  at most Length[vars] denominator factors are already considered minimal
  and do not need further reduction.

  Parameters:
    expr - Rational expression
    vars - List of variables

  Returns:
    True  if the term still needs Gr\[ODoubleDot]bner-based reduction
    False otherwise
*)

ClearAll[NeedsGroebnerReductionQ]

NeedsGroebnerReductionQ[args___] := Null /; !CheckArguments[NeedsGroebnerReductionQ[args], 2]

NeedsGroebnerReductionQ[expr_, vars_] := (
    Message[NeedsGroebnerReductionQ::notList, vars];
    $Failed
) /; !ListQ[vars]

NeedsGroebnerReductionQ[expr_, vars_List] := (
    Message[NeedsGroebnerReductionQ::emptyVars];
    $Failed
) /; Length[vars] === 0

NeedsGroebnerReductionQ[expr_, vars_List] := (
    Message[NeedsGroebnerReductionQ::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

NeedsGroebnerReductionQ[expr_, vars_List] :=
    CountBareDenoms[expr, vars] > Length[vars]

NeedsGroebnerReductionQ::notList = "Second argument `1` must be a list of variables.";
NeedsGroebnerReductionQ::emptyVars = "Variable list must not be empty.";
NeedsGroebnerReductionQ::varsNotSymbols = "The following variables are not Symbols: `1`.";


(*
  GroebnerApart[term, vars, iterativeGroebner, useParameterSymbolization]

  Reduces a single additive term with the Gr\[ODoubleDot]bner-based multivariate
  partial fraction method.

  The function:
    1. Checks whether the term actually needs Gr\[ODoubleDot]bner reduction.
    2. Builds the local denominator set and inverse q-variables.
    3. Optionally makes non-polynomial parameter-dependent pieces symbolic.
    4. Clears remaining parameter denominators in the denominator set.
    5. Builds the local Gr\[ODoubleDot]bner ideal and computes a Gr\[ODoubleDot]bner basis.
    6. Converts the term to q-space.
    7. Reduces either iteratively or in one shot.
    8. Substitutes back from q-space.
    9. Restores the original parameter expressions.

  Parameters:
    term                      - A single additive term.
    vars                      - List of variables.
    iterativeGroebner         - True/False, whether to use iterated q-factor reduction.
    useParameterSymbolization - True/False, whether to replace non-polynomial
                                parameter-dependent coefficient pieces by
                                temporary symbols before Gr\[ODoubleDot]bner computation.

  Returns:
    The reduced term, or $Failed on error.
*)

ClearAll[GroebnerApart]

GroebnerApart[args___] := Null /; !CheckArguments[GroebnerApart[args], 4]

GroebnerApart[term_, vars_, iterativeGroebner_, useParameterSymbolization_] := (
    Message[GroebnerApart::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

GroebnerApart[term_, vars_List, iterativeGroebner_, useParameterSymbolization_] := (
    Message[GroebnerApart::emptyVars];
    $Failed
) /; Length[vars] === 0

GroebnerApart[term_, vars_List, iterativeGroebner_, useParameterSymbolization_] := (
    Message[GroebnerApart::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

GroebnerApart[term_, vars_List, iterativeGroebner_, useParameterSymbolization_] := (
    Message[GroebnerApart::notBoolean, iterativeGroebner, 3];
    $Failed
) /; !MemberQ[{True, False}, iterativeGroebner]

GroebnerApart[term_, vars_List, iterativeGroebner_, useParameterSymbolization_] := (
    Message[GroebnerApart::notBoolean, useParameterSymbolization, 4];
    $Failed
) /; !MemberQ[{True, False}, useParameterSymbolization]

GroebnerApart[term_, vars_List, iterativeGroebner_, useParameterSymbolization_] := Module[
    {
        denoms, qVars,
        combined, newCombined,
        fwdRules, bwdRules,
        newTerm, newDenoms,
        paramDenoms, denomsCleaned,
        ordering, allVars,
        ideal, gb,
        tmp,
        num, qFactors
    },

    (* Terms already minimal are kept unchanged. *)
    If[!NeedsGroebnerReductionQ[term, vars],
        Return[term, Module]
    ];

    (* Get the local denominator set for the current term. *)
    denoms = GetBareDenoms[term, vars];

    If[denoms === $Failed,
        Message[GroebnerApart::denomExtractionFailed, term];
        Return[$Failed, Module]
    ];

    If[Length[denoms] <= Length[vars],
        Return[term, Module]
    ];

    qVars = Table[Unique["q"], {Length[denoms]}];

    (* Default: no parameter substitution. *)
    newTerm = term;
    newDenoms = denoms;
    fwdRules = {};
    bwdRules = {};

    (*
      Optional replacement of non-polynomial parameter-dependent
      sub-expressions by temporary symbols.
    *)
    If[useParameterSymbolization,
        combined = {term, denoms};
        newCombined = MakeParametersSymbolic[combined, vars];

        If[newCombined === $Failed,
            Message[GroebnerApart::parameterFailed, term];
            Return[$Failed, Module]
        ];

        {newCombined, fwdRules, bwdRules} = newCombined;
        {newTerm, newDenoms} = newCombined;
    ];

    (*
      Clear any remaining parameter denominators in the denominator set.
    *)
    paramDenoms = Denominator[Together[#]] & /@ newDenoms;
    denomsCleaned = Expand[#1 * #2] & @@@ Transpose[{newDenoms, paramDenoms}];

    ordering = GroebnerApartOrdering[denomsCleaned, vars, qVars];

    If[ordering === $Failed,
        Message[GroebnerApart::orderingFailed, newDenoms];
        Return[$Failed, Module]
    ];

    allVars = Flatten[ordering];

    ideal = If[And @@ (# === 1 & /@ paramDenoms),
        GroebnerApartIdeal[denomsCleaned, qVars],
        MapThread[#1 * #2 - #3 &, {qVars, denomsCleaned, paramDenoms}]
    ];

    If[ideal === $Failed,
        Message[GroebnerApart::idealFailed, newDenoms];
        Return[$Failed, Module]
    ];

    gb = Quiet @ Check[
        GroebnerBasis[
            ideal,
            allVars,
            MonomialOrder -> EliminationOrder,
            CoefficientDomain -> RationalFunctions
        ],
        $Failed
    ];

    If[gb === $Failed,
        Message[GroebnerApart::basisFailed, newDenoms];
        Return[$Failed, Module]
    ];

    tmp = ExprToApartForm[
        newTerm,
        newDenoms,
        qVars,
        "ReturnPieces" -> iterativeGroebner
    ];

    If[tmp === $Failed,
        Message[GroebnerApart::conversionFailed, newTerm];
        Return[$Failed, Module]
    ];

    If[iterativeGroebner,
        {num, qFactors} = tmp;
        tmp = GroebnerReduce[
            num,
            gb,
            allVars,
            "Iterated" -> iterativeGroebner,
            "QFactors" -> qFactors
        ],
        tmp = GroebnerReduce[
            tmp,
            gb,
            allVars,
            "Iterated" -> iterativeGroebner
        ]
    ];

    If[tmp === $Failed,
        Message[GroebnerApart::reductionFailed, newTerm];
        Return[$Failed, Module]
    ];

    tmp = GroebnerBackSubstitute[tmp, newDenoms, qVars];

    If[tmp === $Failed,
        Message[GroebnerApart::backSubFailed, newTerm];
        Return[$Failed, Module]
    ];

    tmp /. bwdRules
]

GroebnerApart::notList = "Argument `1` at position `2` must be a list.";
GroebnerApart::emptyVars = "Variable list must not be empty.";
GroebnerApart::varsNotSymbols = "The following variables are not Symbols: `1`.";
GroebnerApart::notBoolean = "Argument `1` at position `2` must be True or False.";
GroebnerApart::denomExtractionFailed = "Failed to extract denominator factors from term `1`.";
GroebnerApart::parameterFailed = "Failed to make parameter-dependent non-polynomial pieces symbolic in term `1`.";
GroebnerApart::orderingFailed = "Failed to build a Gr\[ODoubleDot]bner ordering for denominator set `1`.";
GroebnerApart::idealFailed = "Failed to build the Gr\[ODoubleDot]bner ideal for denominator set `1`.";
GroebnerApart::basisFailed = "Failed to compute the Gr\[ODoubleDot]bner basis for denominator set `1`.";
GroebnerApart::conversionFailed = "Failed to convert term `1` into q-space.";
GroebnerApart::reductionFailed = "Gr\[ODoubleDot]bner reduction failed for term `1`.";
GroebnerApart::backSubFailed = "Back substitution from q-space failed for term `1`.";
