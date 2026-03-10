(* ::Package:: *)

BeginPackage["LinApart`"]


(* ::Subsection:: *)
(*Load internal functions*)


(*  
  The package is organized into multiple files for maintainability.
  Files must be loaded in this order due to dependencies.
  
  File structure:
  
  1. tools_general.m
     General utility functions used by both single-variable and multivariate code.
     Contains:
       - GetExponent: extracts base and exponent from power expressions
       - SeparateDependency, Dependent: separates variable-dependent parts
       - GatherByDependency: gathers terms by unique variable-dependent structure
       - GatherByDenominator: gathers fractions by common denominators
       - SeparateFrac: separates non-integer or unsupported power factors
       - VarPattern, IsPolynomialInVars: variable-pattern and polynomial checks
       - NormalizeDenominators: normalizes leading coefficients to 1
  
  2. tools_parallel.m
     Parallelization infrastructure.
     Contains:
       - ReportTime: timing utility for debugging
       - ComputeParallel: parallel evaluation with file-based communication
  
  3. tools_univariate_nonlinear.m
     Functions for handling non-linear (irreducible) denominators in
     single-variable partial fractions.
     Contains:
       - MakeCoefficientsSymbolic: replaces coefficients with symbols
       - ReducePolynomialForResidue: reduces expressions modulo a polynomial
       - NewtonsIdentity: computes power sums via Newton's identities
       - LinApartU: the U/S function for non-linear residue computation
       - DistributeAll: fully distributes products over sums
       - CheckNumericallyIfZero: probabilistic zero-testing
  
  4. tools_multivariate_linear.m
     Functions for multivariate partial fractions with linear denominators.
     Contains:
       - GetDenoms, GetBareDenoms, CountBareDenoms, GetDenomData:
         denominator extraction
       - ExtendedCoefficientVector, ExtendedCoefficientMatrix:
         coefficient matrices
       - FindSafeNullRelations, SafeNullRelations, NullSupportSize:
         null relation finding
       - GetBestDenominatorToReplace: priority ordering for elimination
       - EliminateNullRelations: recursive null relation elimination
       - ExpandNumeratorInDenomSpace: expands numerators in denominator space
       - FindBases: finds all n-element bases (maximal cuts)
  
  5. tools_Leinartas_method.
     Functions for multivariate polynomial decomposition via a
     Leinartas-style algorithm.
     Contains:
       - FindSyzygies: computes polynomial relations among denominator factors
       - ReconstructRelation: reconstructs a relation from coefficient arrays
       - SeparateSyzygiesByType: splits syzygies into homogeneous and
         inhomogeneous classes
       - FilterSyzygiesToCurrentDenoms: restricts syzygies to the current
         denominator set
       - DenomAppearsInSyzygy: checks whether a denominator participates
         in a syzygy
       - DeleteNonPresentDenomFromOrderedDenoms: updates elimination order
       - EliminateInhomogeneousSyzygies: recursive inhomogeneous elimination
       - EliminateHomogeneousSyzygies: recursive homogeneous elimination
  
  6. tools_Gr\[ODoubleDot]bner_method.
     Functions for multivariate Gr\[ODoubleDot]bner-basis partial fraction decomposition.
     Contains:
       - GroebnerApartOrdering: constructs the block variable ordering
       - GroebnerApartIdeal: builds the Gr\[ODoubleDot]bner-apart ideal
       - GroebnerReduceOrderFactors: sorts q-factors for iterated reduction
       - GroebnerReduce: one-shot or iterated Gr\[ODoubleDot]bner reduction in q-space
       - GroebnerBackSubstitute: substitutes q_i -> 1/d_i
       - GroebnerApartConvertFactor: converts a denominator factor into q-space
       - ExprToApartForm: converts an expression into q-space
       - MakeParametersSymbolic: makes non-polynomial parameter pieces symbolic
       - NeedsGroebnerReductionQ: checks whether a term still needs reduction
       - GroebnerApart: reduces a single additive term with the Gr\[ODoubleDot]bner method
  
  7. preprocessor.m
     The preprocessing pipeline that handles both single-variable and
     multivariate cases through a unified staging structure.
     Contains:
       - PreProcessorLinApart: stages 0, 1, 2, 3
         Stage 0: optional factoring and precollecting
         Stage 1: handle sums, special cases, separate dependencies
         Stage 2: normalize denominators or perform method-dependent
                  multivariate preprocessing
         Stage 3: dispatch to mathematicaPartialFraction
  
  8. partial_fraction_algorithms.m
     The core partial fraction algorithms.
     Contains:
       - mathematicaPartialFraction: main algorithm dispatcher
         * Single-variable "ExtendedLaurentSeries": residue-based method
         * Single-variable "Euclidean": GCD-based reduction
         * Multivariate "MultivariateResidue": null relation elimination
           plus basis residues
         * Multivariate "Leinartas": syzygy-based decomposition
         * Multivariate "Groebner": Gr\[ODoubleDot]bner-basis q-space decomposition
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

$LinApartOptions = Options[LinApart] = {
    "Method" -> Automatic,
    "Factor" -> True,
    "GaussianIntegers" -> True,
    "Extension" -> {},
    "Parallel" -> {False, None, None},
    "PreCollect" -> False,
    "ApplyAfterPreCollect" -> None,
    "IterativeGroebner" -> True,
    "GroebnerParameterSymbolization" -> False
};

PrependTo[$Path, If[$Notebooks, NotebookDirectory[], Directory[]]];

Get["tools_general.m"]
Get["tools_parallel.m"]

Get["tools_univariate_nonlinear.m"]
Get["tools_multivariate_linear.m"]
Get["tools_residue_method.m"]
Get["tools_Leinartas_method.m"]
Get["tools_Gr\[ODoubleDot]bner_method.m"]

Get["preprocessor.m"]
Get["partial_fraction_algorithms.m"]

Begin["Private`"]


(* ::Subsection:: *)
(*LinApart wrapper*)


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
         - "Groebner"

  The dispatch is automatic based on whether the second argument is a
  Symbol or a List.

  Method defaults:
    - Single-variable  -> "ExtendedLaurentSeries"
    - Multivariate     -> "MultivariateResidue"

  Additional method-specific options:  
  
  - IterativeGroebner -> True/False:
      controls whether the Gr\[ODoubleDot]bner method reduces q-factors iteratively
      or in one shot; the default value is True.
  - GroebnerParameterSymbolization -> True/False:
      controls whether non-polynomial parameter-dependent coefficient
      pieces are replaced by temporary symbols before Gr\[ODoubleDot]bner-basis
      computations; the default value is True.
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
                               +- "Groebner"
*)

$LinApartUnivariateMethodCases = {
    "ExtendedLaurentSeries",
    "Euclidean",
    "EquationSystem"
};

$LinApartMultivariateMethodCases = {
    "MultivariateResidue",
    "Leinartas",
    "Groebner"
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
] /; OptionValue["Method"] === Automatic

		(* ============================================== *)
		(*           Check for input correctness          *)
		(* ============================================== *)

LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    expr /; PolynomialQ[expr, var]

LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    expr /; PolynomialQ[expr, vars]

LinApart[expr_, var_, options:OptionsPattern[]] := (
    Message[LinApart::varNotSymbol, var];
    $Failed
) /; Head[var] =!= Symbol && Head[var] =!= List

LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    LinApart[expr, First[vars], options] /; Length[vars] === 1

LinApart[expr_, vars_List, options:OptionsPattern[]] := (
    Message[LinApart::emptyVars];
    $Failed
) /; Length[vars] === 0

LinApart[expr_, vars_List, options:OptionsPattern[]] := (
    Message[LinApart::varsNotSymbols, Select[vars, Head[#] =!= Symbol &]];
    $Failed
) /; !And @@ (Head[#] === Symbol & /@ vars)

LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    LinApart[expr, DeleteDuplicates[vars], options] /;
        Length[vars] =!= Length[DeleteDuplicates[vars]]

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

(* Option validation: IterativeGroebner *)
LinApart[expr_, varOrVars_, options:OptionsPattern[]] := (
    Message[LinApart::wrongOption, "IterativeGroebner"];
    $Failed
) /; !MemberQ[$LinApartBooleanCases, OptionValue["IterativeGroebner"]]

		(* ============================================== *)
		(*              Parallel handling                 *)
		(* ============================================== *)

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

LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    Apart[expr, var] /; OptionValue["Method"] === "EquationSystem"

LinApart[expr_, var_Symbol, options:OptionsPattern[]] :=
    PreProcessorLinApart[expr, var, options, 0]

LinApart[expr_, vars_List, options:OptionsPattern[]] :=
    PreProcessorLinApart[expr, vars, options, 0]


(* ::Subsection:: *)
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
  \"Leinartas\",
  \"Groebner\".

Options:
  - Method -> method name or Automatic
      Chooses the decomposition algorithm.
      Automatic selects the default method for the chosen input type.

  - Factor -> True/False
      Factors each additive term before decomposition.
      Default: True.

  - GaussianIntegers -> True/False
      Controls whether factorization is performed over the Gaussian integers.
      Only relevant when Factor -> True.
      Default: True.

  - Extension -> {a[1], a[2], ...}
      Passes an algebraic extension to Mathematica's factorization routines.
      Only relevant when Factor -> True.
      Default: {}.

  - Parallel -> {True/False, NumberOfCores, TemporaryPath}
      Enables parallel evaluation where supported.
      In the single-variable case this is available only for
      \"ExtendedLaurentSeries\".
      In the multivariate case it is used by the residue-based method.
      Default: {False, None, None}.

  - PreCollect -> True/False
      Groups terms by common variable-dependent structure before decomposition.
      Default: False.

  - ApplyAfterPreCollect -> pure function (e.g. Factor)
      Applies the given function to the variable-independent coefficient
      during precollection.
      Only relevant when PreCollect -> True.
      Default: None.

  - IterativeGroebner -> True/False
      Used only with Method -> \"Groebner\".
      True  means that q-factors are introduced one at a time and the
      expression is reduced after each step.
      False means that the full q-space expression is reduced in one shot.
      Default: True.

Notes:
  - Automatic selects \"ExtendedLaurentSeries\" for one variable and
    \"MultivariateResidue\" for multiple variables.
  - MultivariateResidue is intended for linear denominator factors.
  - Leinartas and Groebner are multivariate polynomial methods.
  - Parallel computation is not available for every method.";

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
