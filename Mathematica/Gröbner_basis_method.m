(* ::Package:: *)

(* ::Section::Closed:: *)
(*BuildApartOrdering*)


(*
  BuildApartOrdering[denoms, vars, qVars]

  Creates a block monomial ordering for the Gr\[ODoubleDot]bner basis computation
  following the MultivariateApart strategy.

  The ordering groups denominators by which variables they depend on:
    - Denominators depending on more variables come first (are "greater")
    - Within each group, sort by total degree (higher degree = greater)
    - Finally, the original variables form the last block

  This ordering ensures:
    - Denominators with disjoint zeros are separated (Leinartas requirement i)
    - Good computational performance for GB calculation
    - Lower degrees in the output compared to lexicographic ordering

  Parameters:
    denoms - List of denominator polynomials {d\:2081, ..., d\:2098}
    vars   - List of original variables {x\:2081, ..., x\:2099}
    qVars  - List of inverse variables {q\:2081, ..., q\:2098}

  Returns:
    List of variable blocks for use with MonomialOrder option, e.g.:
    {{q\:2083, q\:2081}, {q\:2082}, {q\:2084}, {x, y}}

  Example:
    denoms = {x^2 + y, x - y, x + 1, x^2 - 3, y + 1, y};
    vars = {x, y};
    qVars = {q1, q2, q3, q4, q5, q6};
    BuildApartOrdering[denoms, vars, qVars]
    -> {{q1, q2}, {q4, q3}, {q5, q6}, {x, y}}
    
    (q1, q2 depend on {x,y}; q3, q4 depend on {x}; q5, q6 depend on {y})
*)

ClearAll[BuildApartOrdering]

BuildApartOrdering[args___] := Null /; !CheckArguments[BuildApartOrdering[args], 3]

BuildApartOrdering[denoms_, vars_, qVars_] := (
    Message[BuildApartOrdering::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

BuildApartOrdering[denoms_, vars_, qVars_] := (
    Message[BuildApartOrdering::notList, vars, 2];
    $Failed
) /; !ListQ[vars]

BuildApartOrdering[denoms_, vars_, qVars_] := (
    Message[BuildApartOrdering::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

BuildApartOrdering[denoms_, vars_, qVars_] := (
    Message[BuildApartOrdering::lengthMismatch];
    $Failed
) /; Length[denoms] =!= Length[qVars]

BuildApartOrdering[denoms_List, vars_List, qVars_List] := Module[
    {
        denomData, groups, sortedGroups, qBlocks
    },

    (*
      For each denominator, record:
        - Which variables it depends on
        - Its total degree
        - Its corresponding q variable
    *)
    denomData = MapThread[
        Function[{d, q},
            {
                Select[vars, !FreeQ[d, #] &],  (* variables it depends on *)
                Total[Exponent[d, vars]],       (* total degree *)
                q                                (* corresponding q *)
            }
        ],
        {denoms, qVars}
    ];

    (*
      Group by variable dependence (first element).
      Each group contains {varSet, degree, q} tuples with same varSet.
    *)
    groups = GatherBy[denomData, First];

    (*
      Sort groups:
        - By number of variables (more variables = greater = comes first)
        - This ensures denominators depending on more variables are reduced first
    *)
    sortedGroups = SortBy[groups, -Length[#[[1, 1]]] &];

    (*
      Within each group, sort by total degree (higher degree = greater = comes first).
      Extract just the q variables in sorted order.
    *)
    qBlocks = Map[
        Function[group,
            SortBy[group, -#[[2]] &][[All, 3]]
        ],
        sortedGroups
    ];

    (* Append the original variables as the final block *)
    Append[qBlocks, vars]
]

BuildApartOrdering::notList = "Argument `1` at position `2` must be a list.";
BuildApartOrdering::lengthMismatch = "Length of denoms must equal length of qVars.";


(* ::Section::Closed:: *)
(*BuildApartIdeal*)


(*
  BuildApartIdeal[denoms, qVars]

  Creates the ideal generators for the MultivariateApart algorithm.

  The ideal is I = \:27e8q\:2081d\:2081 - 1, ..., q\:2098d\:2098 - 1\:27e9
  
  Setting these generators to zero corresponds to the relation q\:1d62 = 1/d\:1d62.

  Parameters:
    denoms - List of denominator polynomials {d\:2081, ..., d\:2098}
    qVars  - List of inverse variables {q\:2081, ..., q\:2098}

  Returns:
    List of polynomials {q\:2081d\:2081 - 1, ..., q\:2098d\:2098 - 1}

  Example:
    BuildApartIdeal[{x + y, x - y}, {q1, q2}]
    -> {q1*(x + y) - 1, q2*(x - y) - 1}
*)

ClearAll[BuildApartIdeal]

BuildApartIdeal[args___] := Null /; !CheckArguments[BuildApartIdeal[args], 2]

BuildApartIdeal[denoms_, qVars_] := (
    Message[BuildApartIdeal::notList, denoms, 1];
    $Failed
) /; !ListQ[denoms]

BuildApartIdeal[denoms_, qVars_] := (
    Message[BuildApartIdeal::notList, qVars, 2];
    $Failed
) /; !ListQ[qVars]

BuildApartIdeal[denoms_, qVars_] := (
    Message[BuildApartIdeal::lengthMismatch];
    $Failed
) /; Length[denoms] =!= Length[qVars]

BuildApartIdeal[denoms_List, qVars_List] := 
    MapThread[#1 * #2 - 1 &, {qVars, denoms}]

BuildApartIdeal::notList = "Argument `1` at position `2` must be a list.";
BuildApartIdeal::lengthMismatch = "Length of denoms must equal length of qVars.";


(* ::Section::Closed:: *)
(*GroebnerReduce*)


(*
  GroebnerReduce[expr, groebnerBasis, varOrder]

  Reduces a polynomial expression in the q-variables and original variables
  with respect to a Gr\[ODoubleDot]bner basis using PolynomialReduce.

  This is the core reduction step of the MultivariateApart algorithm.
  The expression must already be written as a polynomial in the inverse
  denominator variables q_i (i.e., denominators replaced by q_i = 1/d_i).

  Parameters:
    expr           - A polynomial in {q_1, ..., q_m, x_1, ..., x_n}.
                     This is the rational function rewritten using q_i = 1/d_i.
    groebnerBasis  - The Gr\[ODoubleDot]bner basis of the ideal <q_1*d_1 - 1, ..., q_m*d_m - 1>,
                     computed with the given monomial ordering.
    varOrder       - The flattened variable ordering (e.g., {q3, q1, q2, x, y})
                     consistent with the block ordering used for the GB computation.

  Returns:
    The fully reduced polynomial (the unique remainder modulo the GB).

  Examples:
    Given denoms = {x - y, y, x + y}, qVars = {q1, q2, q3}:
    
    GroebnerReduce[q1 q2 q3, gb, varOrder]
      -> 2 q1 q3^2 + q2 q3^2
    
    GroebnerReduce[(2x - y) q1 q2 q3, gb, varOrder]
      -> (reduced form)

  Notes:
    - Uses PolynomialReduce with DegreeReverseLexicographic ordering.
      The block structure is encoded in the variable ordering itself.
    - The remainder from PolynomialReduce is unique for a given GB and ordering.
    - This function does NOT convert back to rational function form;
      use GroebnerBackSubstitute for that.
*)

ClearAll[GroebnerReduce]

GroebnerReduce[args___] := Null /; !CheckArguments[GroebnerReduce[args], 3]

GroebnerReduce[expr_, groebnerBasis_, varOrder_] := (
    Message[GroebnerReduce::notList, groebnerBasis, 2];
    $Failed
) /; !ListQ[groebnerBasis]

GroebnerReduce[expr_, groebnerBasis_, varOrder_] := (
    Message[GroebnerReduce::notList, varOrder, 3];
    $Failed
) /; !ListQ[varOrder]

GroebnerReduce[expr_, groebnerBasis_List, varOrder_List] :=
    Last @ PolynomialReduce[
        expr, 
        groebnerBasis, 
        varOrder, 
        MonomialOrder -> DegreeReverseLexicographic
    ]

GroebnerReduce::notList = "Argument `1` at position `2` must be a list.";


(* ::Section::Closed:: *)
(*GroebnerBackSubstitute*)


(*
  GroebnerBackSubstitute[expr, denoms, qVars]

  Converts an expression from q-space (inverse denominator variables) 
  back to standard rational function form.

  Parameters:
    expr   - A polynomial in the q-variables (output from GroebnerReduce).
    denoms - List of denominator polynomials {d_1, ..., d_m}.
    qVars  - List of inverse denominator variables {q_1, ..., q_m}.
             Must satisfy q_i = 1/d_i.

  Returns:
    The rational function with q_i replaced by 1/d_i.

  Examples:
    GroebnerBackSubstitute[2 q1 q3^2 + q2 q3^2, {x-y, y, x+y}, {q1, q2, q3}]
      -> 2/((x-y)(x+y)^2) + 1/(y(x+y)^2)

  Notes:
    - Simply applies replacement rules q_i -> 1/d_i.
    - Does not simplify or combine terms.
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

GroebnerBackSubstitute[expr_, denoms_, qVars_] := (
    Message[GroebnerBackSubstitute::lengthMismatch];
    $Failed
) /; Length[denoms] =!= Length[qVars]

GroebnerBackSubstitute[expr_, denoms_List, qVars_List] := 
    expr /. MapThread[#1 -> 1/#2 &, {qVars, denoms}]

GroebnerBackSubstitute::notList = "Argument `1` at position `2` must be a list.";
GroebnerBackSubstitute::lengthMismatch = "Length of denoms must equal length of qVars.";


(* ::Section::Closed:: *)
(*ExprToApartForm*)


(*
  ExprToApartForm[expr, denoms, qVars]

  Converts a rational expression into q-space by replacing the actual
  denominator factors d_i^n with q_i^n, while leaving the numerator
  unchanged.

  This version uses robust polynomial equivalence checks (Expand) instead
  of strict structural pattern matching, ensuring that Mathematica's
  automatic sign flips (e.g. x-1 vs -1+x) do not cause substitution failures.

  Parameters:
    expr   - Rational expression.
    denoms - List of denominator polynomials {d_1, ..., d_m}.
    qVars  - List of inverse denominator variables {q_1, ..., q_m}.

  Returns:
    Expression in q-space.

  Examples:
    ExprToApartForm[1/((x-y) y (x+y)), {x-y, y, x+y}, {q1, q2, q3}]
      -> q1 q2 q3

    ExprToApartForm[(2 x - y)/((x-y) y), {x-y, y, x+y}, {q1, q2, q3}]
      -> (2 x - y) q1 q2
*)

ClearAll[ExprToApartForm]

ExprToApartForm[args___] := Null /; !CheckArguments[ExprToApartForm[args], 3]

ExprToApartForm[expr_, denoms_, qVars_] := (
    Message[ExprToApartForm::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

ExprToApartForm[expr_, denoms_, qVars_] := (
    Message[ExprToApartForm::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

ExprToApartForm[expr_, denoms_, qVars_] := (
    Message[ExprToApartForm::lengthMismatch];
    $Failed
) /; Length[denoms] =!= Length[qVars]

ExprToApartForm[expr_, denoms_List, qVars_List] := Module[
    {
        num, den, denFactors,
        convertedFactors
    },

    num = Numerator[expr];
    den = Denominator[expr];

    If[den === 1, Return[num]];

    denFactors = If[Head[den] === Times, List @@ den, {den}];

    convertedFactors = Map[
        Function[factor,
            Module[{base, pow, posMatches, idx},
                {base, pow} = If[Head[factor] === Power,
                    {factor[[1]], factor[[2]]},
                    {factor, 1}
                ];
                
                (* Safely find matching index using Map and Position *)
                posMatches = Position[
                    Map[Expand[# - base] === 0 &, denoms], 
                    True
                ];
                
                If[posMatches =!= {},
                    idx = posMatches[[1, 1]];
                    qVars[[idx]]^pow,
                    
                    (* Try opposite sign *)
                    posMatches = Position[
                        Map[Expand[# + base] === 0 &, denoms], 
                        True
                    ];
                    If[posMatches =!= {},
                        idx = posMatches[[1, 1]];
                        (-1)^pow * qVars[[idx]]^pow,
                        
                        (* Not found *)
                        1/factor
                    ]
                ]
            ]
        ],
        denFactors
    ];

    num * Times @@ convertedFactors
]

ExprToApartForm::notList = "Argument `1` at position `2` must be a list.";
ExprToApartForm::lengthMismatch = "Length of denoms must equal length of qVars.";


(* ::Section::Closed:: *)
(*GrobnerApart*)


(*(*
  GroebnerApart[expr, vars]

  Multivariate partial fraction decomposition using the Gr\[ODoubleDot]bner-basis approach:
    1. Expand the result into additive terms.
    2. Terms with at most Length[vars] variable-dependent denominator
       factors are already minimal and are kept unchanged.
    3. Only terms with more than Length[vars] such factors are reduced
       with the Gr\[ODoubleDot]bner-basis approach, using a local denominator set.

  Parameters:
    expr - A rational expression.
    vars - List of variables.

  Returns:
    Partial fraction decomposition of expr.

  Notes:
    - This is a prototype wrapper.
    - Denominators are assumed to be already factored.
    - Gr\[ODoubleDot]bner bases are only computed for individual terms that strictly 
      need reduction (i.e. those with > Length[vars] denominators).
*)

ClearAll[NeedsGroebnerReductionQ, GroebnerApart]

NeedsGroebnerReductionQ[args___] := Null /; !CheckArguments[NeedsGroebnerReductionQ[args], 2]

NeedsGroebnerReductionQ[expr_, vars_] := (
    Message[NeedsGroebnerReductionQ::notList, vars];
    $Failed
) /; !ListQ[vars]

NeedsGroebnerReductionQ[expr_, vars_List] :=
    Length[GetAllBareDenoms[expr, vars]] > Length[vars]

NeedsGroebnerReductionQ::notList = "Second argument `1` must be a list of variables.";


GroebnerApart[args___] := Null /; !CheckArguments[GroebnerApart[args], 2]

GroebnerApart[expr_, vars_] := (
    Message[GroebnerApart::notList, vars];
    $Failed
) /; !ListQ[vars]

GroebnerApart[expr_, vars_List] := (
    Message[GroebnerApart::emptyVars];
    $Failed
) /; Length[vars] === 0

GroebnerApart[expr_, vars_List] := Module[
    {
        expandedExpr,
        terms
    },

    (* Expressions free of the variables are unchanged. *)
    If[FreeQ[expr, Alternatives @@ vars],
        Return[expr]
    ];

    (* Polynomials are unchanged. *)
    If[PolynomialQ[expr, vars],
        Return[expr]
    ];

    (* If there are not enough denominators globally, nothing to do. *)
    If[Length[GetAllBareDenoms[expr, vars]] <= Length[vars],
        Return[expr]
    ];

    (* Expand so we can inspect terms individually. *)
    expandedExpr = Expand[expr];

    terms = If[Head[expandedExpr] === Plus, List @@ expandedExpr, {expandedExpr}];

    terms = Map[
        Function[term,
            Module[
                {
                    denoms, qVars,
                    ordering, allVars,
                    ideal, gb,
                    tmp
                },

                (* Already minimal: keep unchanged. *)
                If[!NeedsGroebnerReductionQ[term, vars],
                    Return[term, Module]
                ];

                (* Local denominator set for this term only. *)
                denoms = GetAllBareDenoms[term, vars];

                (* Safety check *)
                If[Length[denoms] <= Length[vars],
                    Return[term, Module]
                ];

                qVars = Table[Unique["q"], {Length[denoms]}];

                ordering = BuildApartOrdering[denoms, vars, qVars];
                allVars = Flatten[ordering];

                ideal = BuildApartIdeal[denoms, qVars];
                gb = GroebnerBasis[
                    ideal,
                    allVars,
                    MonomialOrder -> EliminationOrder
                ];

                tmp = ExprToApartForm[term, denoms, qVars];
                tmp = GroebnerReduce[tmp, gb, allVars];
                tmp = GroebnerBackSubstitute[tmp, denoms, qVars];

                tmp
            ]
        ],
        terms
    ];

    Plus @@ terms // Expand[#, Alternatives @@ vars] &
]

GroebnerApart::notList = "Second argument `1` must be a list of variables.";
GroebnerApart::emptyVars = "Variable list must not be empty.";*)


(*
  GroebnerApart[expr, vars]

  Multivariate partial fraction decomposition using the Gr\[ODoubleDot]bner-basis approach.

  This upgraded version follows the staged / iterated reduction philosophy:
    1. Expand the expression into additive terms.
    2. Terms with at most Length[vars] variable-dependent denominator
       factors are already minimal and are kept unchanged.
    3. Terms with more than Length[vars] such factors are mapped to q-space.
    4. Instead of reducing the full q-monomial at once, we multiply in the
       denominator factors one at a time, reducing after each step.
       This often exposes reduction opportunities that a single-shot
       PolynomialReduce would miss.

  Parameters:
    expr - A rational expression.
    vars - List of variables.

  Returns:
    Partial fraction decomposition of expr.

  Notes:
    - This is still a prototype wrapper.
    - Denominators are assumed to be already factored.
    - Gr\[ODoubleDot]bner bases are computed locally for the denominator set of each term.
    - The staged reduction is the main difference to the previous version.
*)

ClearAll[
    NeedsGroebnerReductionQ,
    ExprToApartPieces,
    IteratedApartReduce,
    GroebnerApart
]

(*
  NeedsGroebnerReductionQ[expr, vars]

  Checks whether a term has strictly more variable-dependent bare
  denominators than variables, in which case a nontrivial multivariate
  decomposition is still needed.
*)

NeedsGroebnerReductionQ[args___] := Null /; !CheckArguments[NeedsGroebnerReductionQ[args], 2]

NeedsGroebnerReductionQ[expr_, vars_] := (
    Message[NeedsGroebnerReductionQ::notList, vars];
    $Failed
) /; !ListQ[vars]

NeedsGroebnerReductionQ[expr_, vars_List] :=
    Length[GetAllBareDenoms[expr, vars]] > Length[vars]

NeedsGroebnerReductionQ::notList = "Second argument `1` must be a list of variables.";


(*
  ExprToApartPieces[expr, denoms, qVars]

  Converts a rational expression into a numerator times a list of q-factors.

  Instead of immediately multiplying everything together, we return the
  numerator and the list of q-factors separately. This is needed for the
  iterated Gr\[ODoubleDot]bner reduction strategy.

  Parameters:
    expr   - Rational expression
    denoms - List of denominator polynomials
    qVars  - List of inverse denominator variables

  Returns:
    {numerator, qFactorList}

  where qFactorList is a list like {q1^2, q3, q2^4, ...}.
*)

ClearAll[ExprToApartPieces]

ExprToApartPieces[args___] := Null /; !CheckArguments[ExprToApartPieces[args], 3]

ExprToApartPieces[expr_, denoms_, qVars_] := (
    Message[ExprToApartPieces::notList, denoms, 2];
    $Failed
) /; !ListQ[denoms]

ExprToApartPieces[expr_, denoms_, qVars_] := (
    Message[ExprToApartPieces::notList, qVars, 3];
    $Failed
) /; !ListQ[qVars]

ExprToApartPieces[expr_, denoms_, qVars_] := (
    Message[ExprToApartPieces::lengthMismatch];
    $Failed
) /; Length[denoms] =!= Length[qVars]

ExprToApartPieces[expr_, denoms_List, qVars_List] := Module[
    {
        num, den, denFactors,
        convertedFactors
    },

    num = Numerator[expr];
    den = Denominator[expr];

    If[den === 1,
        Return[{num, {}}]
    ];

    denFactors = If[Head[den] === Times, List @@ den, {den}];

    convertedFactors = Map[
        Function[factor,
            Module[{base, pow, posMatches, idx},

                {base, pow} = If[Head[factor] === Power,
                    {factor[[1]], factor[[2]]},
                    {factor, 1}
                ];

                (* First try exact match *)
                posMatches = Position[
                    Map[Expand[# - base] === 0 &, denoms],
                    True
                ];

                If[posMatches =!= {},
                    idx = posMatches[[1, 1]];
                    qVars[[idx]]^pow,

                    (* Then try match up to a sign *)
                    posMatches = Position[
                        Map[Expand[# + base] === 0 &, denoms],
                        True
                    ];

                    If[posMatches =!= {},
                        idx = posMatches[[1, 1]];
                        (-1)^pow * qVars[[idx]]^pow,
                        1/factor
                    ]
                ]
            ]
        ],
        denFactors
    ];

    {num, convertedFactors}
]

ExprToApartPieces::notList = "Argument `1` at position `2` must be a list.";
ExprToApartPieces::lengthMismatch = "Length of denoms must equal length of qVars.";


(*
  IteratedApartReduce[num, qFactors, gb, allVars]

  Performs staged Gr\[ODoubleDot]bner reduction by multiplying in q-factors one at a time.

  Parameters:
    num     - Polynomial numerator part
    qFactors - List of q-factors such as {q1^2, q3, q2^4}
    gb      - Gr\[ODoubleDot]bner basis
    allVars - Variable ordering used for PolynomialReduce

  Returns:
    Reduced q-space polynomial.

  Strategy:
    - Start from the numerator.
    - Sort q-factors according to their order in allVars.
    - Multiply in one factor at a time.
    - Reduce after each multiplication.

  This mimics the staged reduction strategy discussed in the
  MultivariateApart paper.
*)

ClearAll[IteratedApartReduce]

IteratedApartReduce[args___] := Null /; !CheckArguments[IteratedApartReduce[args], 4]

IteratedApartReduce[num_, qFactors_, gb_, allVars_] := (
    Message[IteratedApartReduce::notList, qFactors, 2];
    $Failed
) /; !ListQ[qFactors]

IteratedApartReduce[num_, qFactors_, gb_, allVars_] := (
    Message[IteratedApartReduce::notList, gb, 3];
    $Failed
) /; !ListQ[gb]

IteratedApartReduce[num_, qFactors_, gb_, allVars_] := (
    Message[IteratedApartReduce::notList, allVars, 4];
    $Failed
) /; !ListQ[allVars]

IteratedApartReduce[num_, qFactors_List, gb_List, allVars_List] := Module[
    {
        orderedFactors,
        tmp
    },

    (* Sort q-factors by first occurring variable position in the ordering *)
    orderedFactors = SortBy[
        qFactors,
        Function[f,
            First @ FirstPosition[
                allVars,
                v_ /; Exponent[f, v] > 0
            ]
        ]
    ];

    tmp = num;

    Do[
        tmp = Expand[tmp * factor];
        tmp = GroebnerReduce[tmp, gb, allVars],
        {factor, orderedFactors}
    ];

    tmp
]

IteratedApartReduce::notList = "Argument `1` at position `2` must be a list.";


(*
  GroebnerApart[expr, vars]

  Upgraded multivariate Gr\[ODoubleDot]bner-based partial fraction decomposition.

  Compared to the earlier version, this one uses an iterated reduction
  strategy:
    - the denominator q-factors are introduced one at a time,
    - after each multiplication, the expression is reduced with respect
      to the Gr\[ODoubleDot]bner basis.

  This often yields a more decomposed result than reducing the full
  q-monomial in one shot.

  Parameters:
    expr - A rational expression
    vars - List of variables

  Returns:
    Partial fraction decomposition of expr.
*)

ClearAll[GroebnerApart]

ClearAll[GroebnerApart]

GroebnerApart[args___] := Null /; !CheckArguments[GroebnerApart[args], 2]

GroebnerApart[expr_, vars_] := (
    Message[GroebnerApart::notList, vars];
    $Failed
) /; !ListQ[vars]

GroebnerApart[expr_, vars_List] := (
    Message[GroebnerApart::emptyVars];
    $Failed
) /; Length[vars] === 0

(*
  PolynomializeParameters[expr, vars]
  
  Replaces all non-polynomial parameter sub-expressions (like 1/(1+b), 
  Sqrt[s t], Beta[p,q], etc.) with unique temporary symbols so that
  the expression becomes a genuine polynomial in vars and these symbols.
  
  Returns {newExpr, forwardRules, backwardRules} where:
    - newExpr has all non-polynomial parameter pieces replaced
    - forwardRules: original -> temp symbol
    - backwardRules: temp symbol -> original
*)
ClearAll[PolynomializeParameters]

PolynomializeParameters[expr_, vars_List] := Module[
    {
        allSymbols, paramSymbols, 
        collected, nonPolyAtoms,
        forwardRules, backwardRules,
        newExpr, expanded,
        subExprs, uniqueSubs
    },
    
    (* Get the fully expanded expression *)
    expanded = expr;
    
    (* 
      Strategy: Expand the expression, then find all "atomic" coefficient 
      pieces that are not polynomial in vars and not simple symbols.
      Replace each unique such piece with a fresh symbol.
    *)
    
    (* Collect all leaf-level sub-expressions that are free of vars 
       but are not simple symbols or integers *)
    subExprs = Cases[
        expanded,
        s_ /; (FreeQ[s, Alternatives @@ vars] && 
               !NumericQ[s] && 
               (* Not a simple symbol *)
               (!AtomQ[s] || (* atoms that aren't polynomial-safe *)
                Head[s] === Rational)),
        {0, Infinity},
        Heads -> False
    ];
    
    (* Also find compound parameter expressions like 1/(1+b), Sqrt[s t], Beta[p,q] *)
    (* More robust: find all maximal sub-expressions free of vars that 
       would make the expression non-polynomial *)
    subExprs = Cases[
        expanded,
        s_ /; (FreeQ[s, Alternatives @@ vars] && 
               !AtomQ[s] &&
               !MatchQ[s, _Integer | _Rational | _Symbol] &&
               (* Don't descend into things we want to replace wholesale *)
               True),
        {0, Infinity},
        Heads -> False
    ];
    
    (* Keep only maximal such sub-expressions: 
       remove any that are sub-expressions of others *)
    subExprs = DeleteDuplicates[subExprs];
    
    (* Filter to those that would cause non-polynomial behavior:
       Power with negative/fractional exponents, special functions, etc. *)
    subExprs = Select[subExprs, 
        MatchQ[#, 
            Power[_, _?Negative] | Power[_, _Rational] | 
            _Beta | _Sqrt | 
            HoldPattern[1/(_)] |
            Power[_, _?(Not@*IntegerQ)]
        ] &
    ];
    
    (* Also catch rational coefficients like a/(1+b) by looking at 
       denominators of coefficients *)
    (* Actually, let's use a cleaner approach *)
    
    newExpr = expanded;
    forwardRules = {};
    backwardRules = {};
    
    (* Replace from largest/most complex to smallest to avoid partial replacement *)
    subExprs = SortBy[subExprs, -LeafCount[#] &];
    
    Do[
        If[FreeQ[newExpr, sub], Continue[]];
        Module[{tmp = Unique["par"]},
            AppendTo[forwardRules, sub -> tmp];
            AppendTo[backwardRules, tmp -> sub];
            newExpr = newExpr /. sub -> tmp;
        ],
        {sub, subExprs}
    ];
    
    {newExpr, forwardRules, backwardRules}
]

GroebnerApart[expr_, vars_List] := Module[
    {
        expandedExpr,
        terms
    },

    If[FreeQ[expr, Alternatives @@ vars],
        Return[expr]
    ];

    If[PolynomialQ[expr, vars],
        Return[expr]
    ];

    If[Length[GetAllBareDenoms[expr, vars]] <= Length[vars],
        Return[expr]
    ];

    expandedExpr = Expand[expr];
    terms = If[Head[expandedExpr] === Plus, List @@ expandedExpr, {expandedExpr}];

    terms = Map[
        Function[term,
            Module[
                {
                    denoms, qVars,
                    ordering, allVars,
                    ideal, gb,
                    num, qFactors,
                    tmp,
                    denomsCleaned, paramDenoms,
                    polyExpr, polyDenoms, fwdRules, bwdRules,
                    allParamVars
                },

                If[!NeedsGroebnerReductionQ[term, vars],
                    Return[term, Module]
                ];

                denoms = GetAllBareDenoms[term, vars];

                If[Length[denoms] <= Length[vars],
                    Return[term, Module]
                ];

                qVars = Table[Unique["q"], {Length[denoms]}];

                (*
                  Step 1: Polynomialize everything.
                  Replace non-polynomial parameter expressions with temp symbols.
                *)
                Module[{allExprs, combined, newCombined, fwd, bwd, newTerm, newDenoms},
                    (* Combine term and denoms into one expression for consistent replacement *)
                    combined = {term, denoms};
                    
                    (* Find all sub-expressions free of vars that are non-polynomial *)
                    fwd = {};
                    bwd = {};
                    newCombined = combined;
                    
                    (* Find problematic sub-expressions across all of combined *)
                    Module[{problematic, sorted},
                        problematic = Cases[
                            combined,
                            s_ /; (FreeQ[s, Alternatives @@ vars] && 
                                   !AtomQ[s] &&
                                   MatchQ[s, 
                                       Power[_, _?Negative] | Power[_, _Rational] | 
                                       _Beta | HoldPattern[Sqrt[_]] |
                                       (* Catch things like (s+t)/(s-t) *)
                                       _?(Function[x, !PolynomialQ[x, {}] && 
                                           Head[x] =!= Symbol && Head[x] =!= Integer &&
                                           Head[x] =!= Times && Head[x] =!= Plus])
                                   ]),
                            {0, Infinity},
                            Heads -> False
                        ];
                        
                        (* Also find any sub-expression free of vars containing Power with negative exp *)
                        problematic = Join[problematic, 
                            Cases[combined,
                                Power[base_, exp_] /; (FreeQ[base, Alternatives @@ vars] && 
                                    (Negative[exp] || !IntegerQ[exp])),
                                {0, Infinity},
                                Heads -> False
                            ]
                        ];
                        
                        (* And Beta, Sqrt *)
                        problematic = Join[problematic,
                            Cases[combined,
                                (Beta | Sqrt)[___],
                                {0, Infinity},
                                Heads -> False
                            ]
                        ];
                        
                        problematic = DeleteDuplicates[problematic];
                        sorted = SortBy[problematic, -LeafCount[#] &];
                        
                        Do[
                            If[!FreeQ[newCombined, sub],
                                Module[{tmp = Unique["par"]},
                                    AppendTo[fwd, sub -> tmp];
                                    AppendTo[bwd, tmp -> sub];
                                    newCombined = newCombined /. sub -> tmp;
                                ]
                            ],
                            {sub, sorted}
                        ];
                    ];
                    
                    newTerm = newCombined[[1]];
                    newDenoms = newCombined[[2]];
                    fwdRules = fwd;
                    bwdRules = bwd;
                    
                    (* 
                      Step 2: Clear remaining rational parameter denominators.
                      After symbol substitution, denoms should mostly be polynomial,
                      but may still have rational coefficients.
                    *)
                    paramDenoms = Map[
                        Function[d, Denominator[Together[d]]],
                        newDenoms
                    ];
                    
                    denomsCleaned = MapThread[
                        Expand[#1 * #2] &,
                        {newDenoms, paramDenoms}
                    ];
                    
                    (* Verify these are now polynomial *)
                    allParamVars = Join[
                        vars, 
                        Cases[bwdRules, (s_ -> _) :> s]
                    ];
                    
                    ordering = BuildApartOrdering[denomsCleaned, vars, qVars];
                    allVars = Flatten[ordering];
                    
                    ideal = MapThread[
                        #1 * #2 - #3 &,
                        {qVars, denomsCleaned, paramDenoms}
                    ];
                    
                    gb = GroebnerBasis[
                        ideal,
                        allVars,
                        MonomialOrder -> EliminationOrder
                    ];
                    
                    (* Use original denoms for ExprToApartPieces since back-sub uses them *)
                    {num, qFactors} = ExprToApartPieces[newTerm, newDenoms, qVars];
                    
                    tmp = IteratedApartReduce[num, qFactors, gb, allVars];
                    tmp = GroebnerBackSubstitute[tmp, newDenoms, qVars];
                    
                    (* Substitute back the original parameter expressions *)
                    tmp = tmp /. bwdRules;
                    
                    tmp
                ]
            ]
        ],
        terms
    ];

    Plus @@ terms // Expand[#, Alternatives @@ vars] &
]

GroebnerApart::notList = "Second argument `1` must be a list of variables.";
GroebnerApart::emptyVars = "Variable list must not be empty.";
