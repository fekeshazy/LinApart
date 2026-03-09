# LinApart Mathematica package

This directory contains the Wolfram Mathematica implementation of `LinApart`.

The package is organized into several source files. The code is split by responsibility so that the public interface, preprocessing, core algorithms, and helper layers stay separate.

## Package structure

The files are loaded in dependency order by the main package file.

### `LinApart.m`

This is the main package entry point.

It defines:

- the public symbol `LinApart`
- package options
- method dispatch
- input validation
- usage messages
- top-level option handling for univariate and multivariate calls

It also loads the internal source files in the correct order.

---

### `tools_general.m`

General helper functions used across the whole package.

This file contains utilities for:

- extracting bases and exponents from powers,
- separating variable-dependent and variable-independent factors,
- grouping terms by common structure,
- grouping terms by denominator,
- separating factors with unsupported powers,
- checking polynomial dependence on variables,
- normalizing univariate denominators.

Main functions in this file include:

- `GetExponent`
- `SeparateDependency`
- `Dependent`
- `GatherByDependency`
- `GatherByDenominator`
- `SeparateFrac`
- `VarPattern`
- `IsPolynomialInVars`
- `NormalizeDenominators`

---

### `tools_parallel.m`

Parallelization and timing utilities.

This file contains:

- `ReportTime`
- `ComputeParallel`

`ComputeParallel` uses file-based communication between Mathematica kernels in order to avoid the large overhead that often appears when passing big symbolic expressions directly.

---

### `tools_univariate_nonlinear.m`

Helper functions for the univariate residue-based method, especially for irreducible non-linear denominator factors.

This file contains functionality for:

- temporarily replacing complicated coefficients by symbols,
- reducing expressions modulo irreducible polynomials,
- Newton identities for power sums,
- auxiliary functions used in the residue formulas,
- complete or selective distribution,
- probabilistic zero testing.

Main functions include:

- `MakeCoefficientsSymbolic`
- `ReducePolynomialForResidue`
- `LinApartU`
- `NewtonsIdentity`
- `DistributeAll`
- `CheckNumericallyIfZero`

---

### `tools_multivariate_linear.m`

Helper layer for the multivariate linear-denominator residue method.

This file contains routines for:

- extracting denominator factors and multiplicities,
- building coefficient vectors and coefficient matrices for linear forms,
- finding affine/null relations,
- selecting elimination priorities,
- recursively eliminating null relations,
- expanding numerators in denominator space,
- finding valid denominator bases.

Main functions include:

- `GetDenoms`
- `GetBareDenoms`
- `CountBareDenoms`
- `GetDenomData`
- `ExtendedCoefficientVector`
- `ExtendedCoefficientMatrix`
- `FindSafeNullRelations`
- `SafeNullRelations`
- `NullSupportSize`
- `GetBestDenominatorToReplace`
- `EliminateNullRelations`
- `ExpandNumeratorInDenomSpace`
- `FindBases`

---

### `tools_multivariate_leinartas.m`

Helper layer for the multivariate `"Leinartas"` method.

This file contains routines for:

- computing polynomial relations among denominator factors,
- reconstructing relations from coefficient-array format,
- separating syzygies into homogeneous and inhomogeneous types,
- filtering syzygies to the currently present denominator set,
- checking whether a given denominator participates in a syzygy,
- recursively eliminating inhomogeneous syzygies,
- recursively eliminating homogeneous syzygies.

Main functions include:

- `FindSyzygies`
- `ReconstructRelation`
- `SeparateSyzygiesByType`
- `FilterSyzygiesToCurrentDenoms`
- `DenomAppearsInSyzygy`
- `DeleteNonPresentDenomFromOrderedDenoms`
- `EliminateInhomogeneousSyzygies`
- `EliminateHomogeneousSyzygies`

If this file does not yet exist as a separate source file, these functions should still be documented as the Leinartas helper layer.

---

### `preprocessor.m`

The staged preprocessing pipeline used by both univariate and multivariate methods.

This file contains `PreProcessorLinApart`, which runs in several stages.

#### Stage 0
Optional factoring and optional precollection.

#### Stage 1
Maps over sums, handles trivial cases, separates variable-independent factors, and removes unsupported powers.

#### Stage 2
Method-specific denominator preprocessing.

For one variable:
- denominator normalization,
- improper-fraction handling,
- preparation for residue or Euclidean decomposition.

For several variables:
- `"MultivariateResidue"` keeps the linear-denominator pipeline,
- `"Leinartas"` prepares polynomial denominators for syzygy-based decomposition.

#### Stage 3
Dispatches to `mathematicaPartialFraction`.

This file is one of the key structural pieces of the package.

---

### `partial_fraction_algorithms.m`

This file contains the core decomposition algorithms.

The main dispatcher is:

- `mathematicaPartialFraction`

Depending on the selected method, it handles:

#### Single-variable
- `"ExtendedLaurentSeries"`
- `"Euclidean"`

#### Multivariate
- `"MultivariateResidue"`
- `"Leinartas"`

This file also contains the residue computation machinery.

Main functions include:

- `mathematicaPartialFraction`
- `ResidueForLaurentSeries`
- `GetDataForResidue`
- `CalculateResidueInDenominatorSpace`
- `ResidueForBasis`

---

## Method overview

The Mathematica package currently supports four main decomposition strategies.

### Univariate `"ExtendedLaurentSeries"`

Residue-based decomposition in one variable.

### Univariate `"Euclidean"`

Extended-GCD/Bézout-based decomposition in one variable.

### Multivariate `"MultivariateResidue"`

Linear multivariate decomposition by null-relation elimination and basis residues.

### Multivariate `"Leinartas"`

Polynomial multivariate decomposition by recursive syzygy elimination.

---

## Data flow

A typical call

```mathematica
LinApart[expr, vars, options]
```

flows through the package as follows:

1. `LinApart.m`
   - validates input and options,
   - normalizes method selection,
   - dispatches into preprocessing.

2. `preprocessor.m`
   - factors or collects if requested,
   - separates supported and unsupported factors,
   - prepares numerator and denominator structure.

3. `partial_fraction_algorithms.m`
   - selects the requested method,
   - computes the decomposition.

4. helper files
   - supply the low-level algebraic and combinatorial operations needed by the chosen method.

---

## Important design ideas

### Separation of concerns

The package is intentionally split into:
- public interface,
- preprocessing,
- algorithms,
- method-specific helpers,
- infrastructure helpers.

### Stage-based preprocessing

The preprocessor is written in stages to make debugging easier and to isolate edge-case handling from the core algorithms.

### Method-specific multivariate logic

The multivariate code now has two distinct mathematical paths:

- a linear-denominator residue method,
- a Leinartas-style polynomial method.

They share the same public interface but use different helper layers.

---

## Notes for developers

When adding a new method, the usual places that need changes are:

- `LinApart.m`
  - add the new method name to validation and dispatch logic

- `preprocessor.m`
  - add or adapt method-specific Stage 2 handling if needed

- `partial_fraction_algorithms.m`
  - add a new `mathematicaPartialFraction` definition guarded by the method name

- a helper file
  - place the method-specific internal functions there

If a helper file depends on another one, make sure the load order in `LinApart.m` is correct.

---

## Testing advice

For correctness, a standard check is:

```mathematica
Together[expr - LinApart[expr, vars, "Method" -> method]] === 0
```

For large expressions, `Together` can be expensive, so numerical checks may also be useful during development.

Regression tests should include:

- simple examples,
- high powers,
- symbolic coefficients,
- numerator-sensitive cases,
- single-denominator edge cases,
- stress tests,
- method-specific non-linear examples for `"Leinartas"`.

---

## File loading order

The main package file should load the source files in dependency order. A typical order is:

1. `tools_general.m`
2. `tools_parallel.m`
3. `tools_univariate_nonlinear.m`
4. `tools_multivariate_linear.m`
5. `tools_multivariate_leinartas.m`
6. `preprocessor.m`
7. `partial_fraction_algorithms.m`

If the Leinartas helper functions are currently stored inside another file, this README still reflects the conceptual separation that the code now has.

