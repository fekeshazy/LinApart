(* ::Package:: *)

BeginPackage["LinApart`"]


(* ::Subsection:: *)
(*Load internal functions*)


(*  
  The package is organized into multiple files for maintainability.
  Files must be loaded in this order due to dependencies.
  
  File structure:
  
  1. tools_general.m
     General utility functions used by both single-variable and multivariate.
     Contains:
       - GetExponent: extracts base and exponent from power expressions
       - SeparateDependency, Dependent: separates variable-dependent parts
       - GatherByDependency: gathers terms by unique variable-dependent structure
       - GatherByDenominator: gathers fractions by common denominators
       - SeparateFrac: separates non-integer power factors
       - NormalizeDenominators: normalizes leading coefficients to 1
  
  2. tools_parallel.m
     Parallelization infrastructure.
     Contains:
       - ReportTime: timing utility for debugging
       - ComputeParallel: parallel evaluation with file-based communication
  
  3. tools_univariate_nonlinear.m
     Functions for handling non-linear (irreducible) denominators in 
     single-variable partial fractions (LinApart2 functionality).
     Contains:
       - MakeCoefficientsSymbolic: replaces coefficients with symbols
       - ReducePolynomialForResidue: reduces expressions modulo a polynomial
       - NewtonsIdentity: computes power sums via Newton's identities
       - LinApartU: the U/S function for non-linear residue computation
       - DistributeAll: fully distributes products over sums
       - CheckNumericallyIfZero: probabilistic zero-testing
  
  4. tools_multivariate_linear.m
     Functions for multivariate partial fractions with linear denominators 
     (LinApart3 functionality).
     Contains:
       - GetDenoms, GetBareDenoms, GetDenomData: denominator extraction
       - ExtendedCoefficientVector, ExtendedCoefficientMatrix: coefficient matrices
       - FindSafeNullRelations, SafeNullRelations, NullSupportSize: null relation finding
       - GetBestDenominatorToReplace: priority ordering for elimination
       - EliminateNullRelations: recursive null relation elimination
       - FindBases: finds all n-element bases (maximal cuts)
  
  5. preprocessor.m
     The preprocessing pipeline that handles both single-variable and 
     multivariate cases through a unified staging structure.
     Contains:
       - PreProcessorLinApart: stages 0, 1, 2, 3
         Stage 0: optional factoring and precollecting
         Stage 1: handle sums, special cases, separate dependencies
         Stage 2: normalize denominators, check linearity
         Stage 3: dispatch to mathematicaPartialFraction
  
  6. partial_fraction_algorithms.m
     The core partial fraction algorithms.
     Contains:
       - mathematicaPartialFraction: main algorithm dispatcher
         * Single-variable "ExtendedLaurentSeries": residue-based method
         * Single-variable "Euclidean": GCD-based reduction
         * Multivariate: null relation elimination + basis residues
       - ResidueForLaurentSeries: computes residues at poles (single-variable)
       - ResidueForBasis: computes residues at basis intersections (multivariate)
       - GetDataForResidue: extracts data for multivariate residue computation
       - CalculateResidueInDenominatorSpace: multivariate residue formula
*)

(*Is is soo fucking bullshit, that I must define these here...*)
ClearAll[LinApart]

		(* ============================================== *)
		(*                 Options                        *)
		(* ============================================== *)

$LinApartOptions=Options[LinApart] = {
    "Method" -> "Automatic",
    "Factor" -> True,
    "GaussianIntegers" -> True,
    "Extension" -> {},
    "Parallel" -> {False, None, None},
    "PreCollect" -> False,
    "ApplyAfterPreCollect" -> None
};

PrependTo[$Path, If[$Notebooks, NotebookDirectory[], Directory[]]];

Get["tools_general.m"]
Get["tools_parallel.m"]
Get["tools_univariate_nonlinear.m"]
Get["tools_multivariate_linear.m"]
Get["tools_residue_method.m"]
Get["tools_Leinartas_algorithm.m"]
Get["preprocessor.m"]
Get["partial_fraction_algorithms.m"]

Begin["Private`"]


(* ::Subsection:: *)
(*LinApart wrapper*)


LinApart function
(*
  LinApart - Partial Fraction Decomposition

  This file contains the public entry point of the LinApart package.

  LinApart handles two cases:
    1. Single-variable:
         LinApart[expr, var, options]
       Available methods:
         - "ExtendedLaurentSeries"
         - "Euclidean"
         - "EquationSystem"

    2. Multivariate:
         LinApart[expr, {var1, var2, ...}, options]
       Available methods:
         - "MultivariateResidue"
         - "Leinartas"

  The dispatch is automatic based on whether the second argument is a
  Symbol or a List.

  Method defaults:
    - Single-variable  -> "ExtendedLaurentSeries"
    - Multivariate     -> "MultivariateResidue"
*)

(*
  Architecture overview:

  LinApart[expr, var_Symbol, options]
  LinApart[expr, vars_List, options]
      |
      v
  PreProcessorLinApart[expr, var/vars, options, 0]
      |  - Factor (if option set)
      |  - PreCollect (if option set)
      v
  PreProcessorLinApart[..., 1]
      |  - Map over sums
      |  - Check FreeQ, PolynomialQ
      |  - SeparateDependency
      |  - SeparateFrac
      v
  PreProcessorLinApart[..., 2]
      |  - NormalizeDenominators (single-variable only)
      |  - Multivariate method-dependent preprocessing
      |  - Handle edge cases
      v
  PreProcessorLinApart[..., 3]
      |
      +--- var_Symbol ---> mathematicaPartialFraction[..., var, options]
      |                        |
      |                        +- "ExtendedLaurentSeries"
      |                        +- "Euclidean"
      |                        +- "EquationSystem" -> Apart
      |
      +--- vars_List ---> mathematicaPartialFraction[..., vars, options]
                               |
                               +- "MultivariateResidue"
                               +- "Leinartas"
*)

$LinApartUnivariateMethodCases = {
    "ExtendedLaurentSeries",
    "Euclidean",
    "EquationSystem"
};

$LinApartMultivariateMethodCases = {
    "MultivariateResidue",
    "Leinartas"
};

$LinApartBooleanCases = {True, False};

		(* ============================================== *)
		(*           Argument count check                 *)
		(* ============================================== *)

LinApart[arg___] := $Failed /; !CheckArguments[LinApart[arg], 2]

		(* ============================================== *)
		(*         Automatic method normalization         *)
		(* ============================================== *)

(* Single-variable default method *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] := Module[
    {newOptions},

    newOptions = FilterRules[{options}, Except["Method"]];

    LinApart[
        expr,
        var,
        "Method" -> "ExtendedLaurentSeries",
        Sequence @@ newOptions
    ]
] /; OptionValue["Method"] === Automatic

(* Multivariate default method *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := Module[
    {newOptions},

    newOptions = FilterRules[{options}, Except["Method"]];

    LinApart[
        expr,
        vars,
        "Method" -> "MultivariateResidue",
        Sequence @@ newOptions
    ]
] /; OptionValue["Method"] === "Automatic"

		(* ============================================== *)
		(*           Check for input correctness          *)
		(* ============================================== *)

(* Expression is a polynomial in var - nothing to decompose *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    expr /; PolynomialQ[expr, var]

(* Expression is a polynomial in all vars - nothing to decompose *)
LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    expr /; PolynomialQ[expr, vars]

(* Variable must be a Symbol or List *)
LinApart[expr_, var_, options:OptionsPattern[]] := (
    Message[LinApart::varNotSymbol, var];
    $Failed
) /; Head[var] =!= Symbol && Head[var] =!= List

(* Single-element list: delegate to single-variable version *)
LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    LinApart[expr, First[vars], options] /; Length[vars] === 1

(* Validation: vars must not be empty *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := (
    Message[LinApart::emptyVars];
    $Failed
) /; Length[vars] === 0

(* Validation: all vars must be Symbols *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := (
    Message[LinApart::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

(* Remove duplicates and re-dispatch *)
LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    LinApart[expr, DeleteDuplicates[vars], options] /;
        Length[vars] =!= Length[DeleteDuplicates[vars]]

(* Filter out unused variables and retry *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := Module[
    {usedVars, unusedVars},

    usedVars = Intersection[vars, Variables[Denominator[Together[expr]]]];
    unusedVars = Complement[vars, usedVars];

    If[unusedVars =!= {},
        Message[LinApart::droppingVars, unusedVars];
    ];

    Which[
        Length[usedVars] === 0,
            expr,
        Length[usedVars] === 1,
            LinApart[expr, First[usedVars], options],
        True,
            LinApart[expr, usedVars, options]
    ]
] /; Length[Complement[vars, Variables[Denominator[expr]]]] > 0

		(* ============================================== *)
		(*             Option validation                  *)
		(* ============================================== *)

(* Option validation: Method (single-variable) *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "Method"];
    $Failed
) /; !MemberQ[Join[$LinApartUnivariateMethodCases, {Automatic}], OptionValue["Method"]]

(* Option validation: Method (multivariate) *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "Method"];
    $Failed
) /; !MemberQ[Join[$LinApartMultivariateMethodCases, {Automatic}], OptionValue["Method"]]

(* Option validation: Factor *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "Factor"];
    $Failed
) /; !MemberQ[$LinApartBooleanCases, OptionValue["Factor"]]

(* Option validation: GaussianIntegers *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "GaussianIntegers"];
    $Failed
) /; !MemberQ[$LinApartBooleanCases, OptionValue["GaussianIntegers"]]

(* Option validation: GaussianIntegers requires Factor *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::factorIsFalse, "GaussianIntegers"];
    $Failed
) /; OptionValue["GaussianIntegers"] && !OptionValue["Factor"]

(* Option validation: Extension requires Factor *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::factorIsFalse, "Extension"];
    $Failed
) /; OptionValue["Extension"] =!= {} && !OptionValue["Factor"]

(* Option validation: Extension must be a list of numbers *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "Extension"];
    $Failed
) /; Head[OptionValue["Extension"]] =!= List ||
     (OptionValue["Extension"] =!= {} &&
      !And @@ Map[NumberQ, N[OptionValue["Extension"]]])

(* Option validation: Parallel format {bool, int, string} *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "Parallel"];
    $Failed
) /; !MatchQ[OptionValue["Parallel"], {True | False, _Integer | None, _String | None}]

(* Option validation: PreCollect *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "PreCollect"];
    $Failed
) /; !MemberQ[$LinApartBooleanCases, OptionValue["PreCollect"]]

(* Option validation: ApplyAfterPreCollect requires PreCollect *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "ApplyAfterPreCollect"];
    $Failed
) /; !OptionValue["PreCollect"] && OptionValue["ApplyAfterPreCollect"] =!= None

		(* ============================================== *)
		(*              Parallel handling                 *)
		(* ============================================== *)

(* Parallel requested but no kernels available: single-variable *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] := Module[
    {newOptions},

    Message[LinApart::noParallelKernels];

    newOptions = FilterRules[{options}, Except["Parallel"]];

    LinApart[
        expr,
        var,
        "Parallel" -> {False, OptionValue["Parallel"][[2]], OptionValue["Parallel"][[3]]},
        Sequence @@ newOptions
    ]
] /; OptionValue["Parallel"][[1]] && Length[Kernels[]] === 0

(* Parallel requested but no kernels available: multivariate *)
LinApart[expr_, vars_List, options:OptionsPattern[]] := Module[
    {newOptions},

    Message[LinApart::noParallelKernels];

    newOptions = FilterRules[{options}, Except["Parallel"]];

    LinApart[
        expr,
        vars,
        "Parallel" -> {False, OptionValue["Parallel"][[2]], OptionValue["Parallel"][[3]]},
        Sequence @@ newOptions
    ]
] /; OptionValue["Parallel"][[1]] && Length[Kernels[]] === 0

(* Parallel only works with ExtendedLaurentSeries in the single-variable case *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] := Module[
    {newOptions},

    Message[LinApart::ParallelComputationError];

    newOptions = FilterRules[{options}, Except["Parallel"]];

    LinApart[
        expr,
        var,
        "Parallel" -> {False, OptionValue["Parallel"][[2]], OptionValue["Parallel"][[3]]},
        Sequence @@ newOptions
    ]
] /; OptionValue["Parallel"][[1]] && OptionValue["Method"] =!= "ExtendedLaurentSeries"

		(* ============================================== *)
		(*      Entry point for further computation       *)
		(* ============================================== *)

(* EquationSystem method: delegate to Apart *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    Apart[expr, var] /; OptionValue["Method"] === "EquationSystem"

(* Main single-variable entry point: delegate to preprocessor *)
LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    PreProcessorLinApart[expr, var, options, 0]

(* Main multivariate entry point: delegate to preprocessor *)
LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    PreProcessorLinApart[expr, vars, options, 0]


(* ::Subsection::Closed:: *)
(*Text of messages*)


End[];

LinApart::usage =
"LinApart[expression, variable_Symbol]
LinApart[expression, variable_Symbol, Options]
LinApart[expression, {var1, var2, ...}]
LinApart[expression, {var1, var2, ...}, Options]

The function gives a partial fraction decomposition with respect to the chosen variable(s).

For a single variable, available methods are:
  \"ExtendedLaurentSeries\" (default),
  \"Euclidean\",
  \"EquationSystem\".

For multiple variables, available methods are:
  \"MultivariateResidue\" (default),
  \"Leinartas\".

Options:
  - Method -> method name or Automatic
  - Factor -> True/False
  - GaussianIntegers -> True/False
  - Extension -> {a[1], a[2], ...}
  - Parallel -> {True/False, NumberOfCores, TemporaryPath}
  - PreCollect -> True/False
  - ApplyAfterPreCollect -> pure function (e.g. Factor)

Notes:
  - Automatic selects \"ExtendedLaurentSeries\" for one variable and
    \"MultivariateResidue\" for multiple variables.
  - Parallel computation is only supported for the
    \"ExtendedLaurentSeries\" method in the univariate case and for
    multivariate residue/basis computations in the multivariate case.
";

(* Message definitions *)
LinApart::noParallelKernels = "There are no parallel kernels available, proceeding with sequential evaluation.";
LinApart::ParallelComputationError = "Parallel computation is not possible for this method, proceeding with sequential evaluation.";
LinApart::varNotSymbol = "The variable `1` is not a symbol or list.";
LinApart::wrongOption = "Problem with option `1`, OptionName or OptionValue not recognized.";
LinApart::nonLinearExpression = "The expression contains non-linear denominators `1`.";
LinApart::nonLinearDenomFactored = "Warning: Non-linear denominators `1` were factored out and not decomposed.";
LinApart::factorIsFalse = "`1` is an option for Factor but Factor was set to False.";
LinApart::emptyVars = "Variable list must not be empty.";
LinApart::unusedVars = "Variables `1` do not appear in any denominator. All specified variables must be used.";
LinApart::varsNotSymbols = "The following variables are not Symbols: `1`.";
LinApart::droppingVars = "Warning: Variables `1` do not appear in denominators and will be ignored.";

EndPackage[];
