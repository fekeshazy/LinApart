(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Preprocessor*)


(*
  PreProcessorLinApart - Preprocessing for Partial Fraction Decomposition

  This function prepares expressions for partial fraction decomposition by 
  handling edge cases, separating variable-dependent and independent parts, 
  normalizing denominators, and dispatching to the main algorithm.

  The function is written in stages for two main reasons:
    1. Easier debugging and readability.
    2. More flexibility in coding and future extensions.

  The stages are:
    Stage 0: Optional term-by-term factoring and/or gathering by unique 
             variable-dependent structure.

    Stage 1: Handle sums (map over terms), detect special cases where 
             decomposition is not needed, separate variable-independent 
             multiplicative terms, handle special cases in numerator 
             (complex/rational/symbolic powers).

    Stage 2: Manipulate denominator arguments (normalization, collecting),
             handle special cases in denominator, separate constants from 
             normalization, handle improper fractions.

    Stage 3: Final dispatch to mathematicaPartialFraction.

  The function handles both single-variable (var_Symbol) and multivariate 
  (vars_List) cases. The dispatch is based on pattern matching.
*)

Options[PreProcessorLinApart] = {
    "Method" -> "ExtendedLaurentSeries",
    "Factor" -> True,
    "GaussianIntegers" -> True,
    "Extension" -> {},
    "Parallel" -> {False, None, None},
    "PreCollect" -> False,
    "ApplyAfterPreCollect" -> None
};


(* ============================================== *)
(*      Stage 0: Factoring and PreCollecting      *)
(* ============================================== *)

(* Single-variable version *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 0] := 
Module[
    {tmp, varPat},

    varPat = VarPattern[var];
    
    (* Expand with respect to variable to expose structure. *)
    tmp = Expand[expr, var];

    (* Optional factoring of variable-dependent parts. *)
    tmp = If[OptionValue["Factor"],
        If[Head[tmp] === Plus,
            Map[
                Times @@ (
                    SeparateDependency[#, varPat] // 
                    MapAt[
                        Factor[#, 
                            GaussianIntegers -> OptionValue["GaussianIntegers"], 
                            Extension -> OptionValue["Extension"]
                        ] &, 
                        #, 2
                    ] &
                ) &,
                tmp
            ],
            Times @@ (
                SeparateDependency[tmp, varPat] // 
                MapAt[
                    Factor[#, 
                        GaussianIntegers -> OptionValue["GaussianIntegers"], 
                        Extension -> OptionValue["Extension"]
                    ] &, 
                    #, 2
                ] &
            )
        ],
        tmp
    ];

    (* Optional gathering by variable-dependent structure. *)
    tmp = If[OptionValue["PreCollect"],
        GatherByDependency[tmp, varPat, OptionValue["ApplyAfterPreCollect"], None],
        tmp
    ];

    (* Proceed to Stage 1. *)
    PreProcessorLinApart[tmp, var, options, 1]
]

(* Multivariate version *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 0] :=
Module[
    {tmp, varPat},

    varPat = VarPattern[vars];

    (* Expand with respect to all variables. *)
    tmp = Expand[expr, vars];

    (* Optional factoring of variable-dependent parts. *)
    tmp = If[OptionValue["Factor"],
        If[Head[tmp] === Plus,
            Map[
                Times @@ (
                    SeparateDependency[#, varPat] // 
                    MapAt[
                        Factor[#, 
                            GaussianIntegers -> OptionValue["GaussianIntegers"], 
                            Extension -> OptionValue["Extension"]
                        ] &, 
                        #, 2
                    ] &
                ) &,
                tmp
            ],
            Times @@ (
                SeparateDependency[tmp, varPat] // 
                MapAt[
                    Factor[#, 
                        GaussianIntegers -> OptionValue["GaussianIntegers"], 
                        Extension -> OptionValue["Extension"]
                    ] &, 
                    #, 2
                ] &
            )
        ],
        tmp
    ];

    (* Optional gathering by variable-dependent structure. *)
    tmp = If[OptionValue["PreCollect"],
        GatherByDependency[tmp, varPat, OptionValue["ApplyAfterPreCollect"], None],
        tmp
    ];

    (* Proceed to Stage 1. *)
    PreProcessorLinApart[tmp, vars, options, 1]
]

(* ============================================== *)
(*    Stage 1: Special Cases and Separation       *)
(* ============================================== *)

(* Single-variable rules *)

(* Map over sums. *)
PreProcessorLinApart[expr_Plus, var_Symbol, options:OptionsPattern[], 1] := 
    PreProcessorLinApart[#, var, options, 1] & /@ expr

(* Expression free of variable: nothing to decompose. *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 1] := 
    expr /; FreeQ[expr, var]

(* Expression is a polynomial: nothing to decompose. *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 1] := 
    expr /; PolynomialQ[expr, var]

(* No denominator, or denominator free of variable *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 1] := 
    expr /; (Denominator[expr] === 1 || FreeQ[Denominator[expr], var]) && 
            FreeQ[expr, Power[_, Complex[a_ /; a < 0, _]]]

(* Numerator free of variable and denominator is unfactored polynomial *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 1] := 
    expr /; FreeQ[Numerator[expr], var] && Head[Denominator[expr]] === Plus

(* Single irreducible polynomial power with var-free numerator *)
PreProcessorLinApart[expr:num_. Power[poly_, pow_], var_Symbol, options:OptionsPattern[], 1] := 
    expr /; FreeQ[num, var] && IrreduciblePolynomialQ[poly]

(* Main Stage 1 processing for single variable. *)
PreProcessorLinApart[expr_, var_Symbol, options:OptionsPattern[], 1] := Module[
    {tmp, coeff = 1, ignoreFrac, keepFrac, varPat},

    (* Separate variable-dependent and independent multiplicative parts. *)
    tmp = SeparateDependency[expr, var];
    coeff = coeff * tmp[[1]];
    tmp = tmp[[2]];

    (* Separate factors with non-integer powers (to ignore) from good factors. *)
    {ignoreFrac, keepFrac} = SeparateFrac[tmp, var];

    (* Proceed to Stage 2. *)
    PreProcessorLinApart[coeff, ignoreFrac, keepFrac, var, options, 2]
]

(* Multivariate rules *)

(* Map over sums. *)
PreProcessorLinApart[expr_Plus, vars_List, options:OptionsPattern[], 1] := 
    PreProcessorLinApart[#, vars, options, 1] & /@ expr

(* Expression free of all variables: nothing to decompose. *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 1] := 
    expr /; FreeQ[expr, Alternatives @@ vars]

(* Expression is a polynomial in all variables: nothing to decompose. *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 1] := 
    expr /; IsPolynomialInVars[expr, vars]

(* No denominator, or denominator free of all variables *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 1] := 
    expr /; (Denominator[expr] === 1 || FreeQ[Denominator[expr], Alternatives @@ vars]) && 
            FreeQ[expr, Power[_, Complex[a_ /; a < 0, _]]]

(* Numerator free of all variables and denominator is unfactored polynomial *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 1] := 
    expr /; FreeQ[Numerator[expr], Alternatives @@ vars] && Head[Denominator[expr]] === Plus

(* Main Stage 1 processing for multivariate. *)
PreProcessorLinApart[expr_, vars_List, options:OptionsPattern[], 1] := Module[
    {tmp, coeff = 1, ignoreFrac, keepFrac, varPat},

    varPat = VarPattern[vars];

    (* Separate variable-dependent and independent multiplicative parts. *)
    tmp = SeparateDependency[expr, varPat];
    coeff = coeff * tmp[[1]];
    tmp = tmp[[2]];

    (* Separate factors with non-integer powers (to ignore) from good factors. *)
    {ignoreFrac, keepFrac} = SeparateFrac[tmp, vars];

    (* Proceed to Stage 2. *)
    PreProcessorLinApart[coeff, ignoreFrac, keepFrac, vars, options, 2]
]

(* ============================================== *)
(* Stage 2: Normalization and Improper Fractions  *)
(* ============================================== *)

(* Single-variable rules *)

(* keepFrac is 1: nothing to decompose *)
PreProcessorLinApart[coeff_, ignoreFrac_, 1, var_Symbol, options:OptionsPattern[], 2] := 
    coeff * ignoreFrac

(* keepFrac is a polynomial (not 1): nothing to decompose. *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepFrac_, var_Symbol, options:OptionsPattern[], 2] := 
    coeff * ignoreFrac * keepFrac /; PolynomialQ[keepFrac, var] && keepFrac =!= 1

(* Main Stage 2 processing for single variable. *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepFrac_, var_Symbol, options:OptionsPattern[], 2] := Module[
    {
        tmp,
        tmpCoeff = coeff,
        keepForDivision = 1,
        tmpKeepFrac = keepFrac,
        tmpNormalizationCoeff,
        tmpNum, tmpDen,
        tmpNumExp, tmpDenExp,
        pow
    },

    (* Extract non-monomial numerator factors. *)
    tmp = If[Head[tmpKeepFrac] =!= Times, {tmpKeepFrac}, List @@ tmpKeepFrac];
    tmp = Times @@ Cases[
        tmp,
        Power[a_, b_] /; b > 0 && Head[a] =!= Symbol :> Power[a, b],
        1
    ];
    keepForDivision = keepForDivision * tmp;
    tmpKeepFrac = tmpKeepFrac / tmp;

    (* Collect denominators in the variable before normalization. *)
    tmpKeepFrac = tmpKeepFrac /. Power[a_, pow_] /; pow < 0 :> Power[Collect[a, var], pow];

    (* Normalize denominators so leading coefficient is 1. *)
    {tmpNormalizationCoeff, tmpKeepFrac} = NormalizeDenominators[tmpKeepFrac, var];
    tmpCoeff = tmpCoeff * tmpNormalizationCoeff;

    (* Separate any constants introduced by normalization. *)
    tmp = SeparateDependency[tmpKeepFrac, var];
    tmpCoeff = tmpCoeff * tmp[[1]];
    tmpKeepFrac = tmp[[2]];

    (* Check for improper fraction. *)
    tmpNum = Numerator[tmpKeepFrac];
    tmpDen = Denominator[tmpKeepFrac];
    tmpNumExp = Exponent[tmpNum, var];
    tmpDenExp = Exponent[tmpDen, var];

    If[tmpDenExp <= tmpNumExp,
        (* Improper fraction *)
        pow = tmpDenExp - tmpNumExp - 1;
        PreProcessorLinApart[tmpCoeff, ignoreFrac, keepForDivision * var^(-pow), var^pow * tmpKeepFrac, var, options, 3],
        (* Proper fraction *)
        PreProcessorLinApart[tmpCoeff, ignoreFrac, keepForDivision, tmpKeepFrac, var, options, 3]
    ]
]

(* Multivariate rules *)

(* keepFrac is 1: nothing to decompose. *)
PreProcessorLinApart[coeff_, ignoreFrac_, 1, vars_List, options:OptionsPattern[], 2] := 
    coeff * ignoreFrac

(* keepFrac is a polynomial in all variables: nothing to decompose. *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepFrac_, vars_List, options:OptionsPattern[], 2] := 
    coeff * ignoreFrac * keepFrac /; IsPolynomialInVars[keepFrac, vars] && keepFrac =!= 1

(* Main Stage 2 processing for multivariate. *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepFrac_, vars_List, options:OptionsPattern[], 2] := Module[
    {
        tmp,
        tmpCoeff = coeff,
        keepForDivision = 1,
        tmpKeepFrac = keepFrac,
        varPat,
        bareDenoms, nonLinearDenoms
    },

    varPat = Alternatives @@ vars;

    (* Extract non-monomial numerator factors. *)
    tmp = If[Head[tmpKeepFrac] =!= Times, {tmpKeepFrac}, List @@ tmpKeepFrac];
    tmp = Times @@ Cases[
        tmp,
        Power[a_, b_] /; b > 0 && Head[a] =!= Symbol :> Power[a, b],
        1
    ];
    keepForDivision = keepForDivision * tmp;
    tmpKeepFrac = tmpKeepFrac / tmp;

    (* Linearity check *)
    bareDenoms = If[
        Head[Denominator[tmpKeepFrac]] === Times,
        Cases[
            List @@ (1/Denominator[tmpKeepFrac]),
            Power[base_, _] | base_ :> base
        ],
        {
            If[Head[Denominator[tmpKeepFrac]] === Power,
                Denominator[tmpKeepFrac][[1]],
                Denominator[tmpKeepFrac]
            ]
        }
    ];
    
    (* Filter to only var-dependent denominators - FIXED *)
    bareDenoms = Select[bareDenoms, !FreeQ[#, varPat] &];
    
    (* Safety check - FIXED *)
    If[Length[bareDenoms] < Length[vars],
        Return[tmpCoeff * ignoreFrac * keepForDivision * tmpKeepFrac, Module]
    ];
    
    (* Check each denominator is linear *)
    nonLinearDenoms = Select[bareDenoms, Max[Exponent[#, vars]] > 1 &];
    
    If[nonLinearDenoms =!= {},
        Message[LinApart::nonLinearExpression, nonLinearDenoms];  (* FIXED *)
        Return[tmpCoeff * ignoreFrac * keepForDivision * tmpKeepFrac, Module]  (* FIXED *)
    ];

    (* Separate any variable-free constants. *)
    tmp = SeparateDependency[tmpKeepFrac, varPat];
    tmpCoeff = tmpCoeff * tmp[[1]];
    tmpKeepFrac = tmp[[2]];

    PreProcessorLinApart[tmpCoeff, ignoreFrac, keepForDivision, tmpKeepFrac, vars, options, 3]
]

(* ============================================== *)
(*      Stage 3: Dispatch to Main Algorithm       *)
(* ============================================== *)

(* Single-variable rules *)

(* No denominator left *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepForDivision_, keepFrac_, var_Symbol, options:OptionsPattern[], 3] := 
    coeff * ignoreFrac * keepForDivision * keepFrac /; Denominator[keepFrac] === 1

(* Main dispatch *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepForDivision_, keepFrac_, var_Symbol, options:OptionsPattern[], 3] := 
    mathematicaPartialFraction[coeff, ignoreFrac, keepForDivision, keepFrac, var, options]

(* Multivariate rules *)

(* No denominator left *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepForDivision_, keepFrac_, vars_List, options:OptionsPattern[], 3] := 
    coeff * ignoreFrac * keepForDivision * keepFrac /; Denominator[keepFrac] === 1

(* Main dispatch *)
PreProcessorLinApart[coeff_, ignoreFrac_, keepForDivision_, keepFrac_, vars_List, options:OptionsPattern[], 3] := 
    mathematicaPartialFraction[coeff, ignoreFrac, keepForDivision, keepFrac, vars, options]
