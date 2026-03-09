# LinApart

`LinApart` is a package for partial fraction decomposition in **Wolfram Mathematica**, together with a **C implementation** of the original univariate algorithm.

The project currently supports:

- **univariate** partial fraction decomposition,
- **multivariate** decomposition for **linear** denominators,
- **multivariate** decomposition for **general polynomial** denominators via a **Leinartas-style** method.

The Mathematica implementation is the main development branch of the project. It contains several complementary algorithms under one interface.

The package is based on the following papers:

- [arXiv:2405.20130](https://arxiv.org/abs/2405.20130)
- [arXiv:2511.15735](https://arxiv.org/pdf/2511.15735)

If `LinApart` is useful in your research, please consider citing the relevant paper(s).

## Features

### Mathematica package

The Mathematica package provides a single entry point:

```mathematica
LinApart[expr, var]
LinApart[expr, var, options]

LinApart[expr, {var1, var2, ...}]
LinApart[expr, {var1, var2, ...}, options]
```

It supports the following methods.

#### Single-variable methods

- `"ExtendedLaurentSeries"`  
  Residue-based decomposition. This is the main univariate method and also supports irreducible non-linear denominator factors.

- `"Euclidean"`  
  Uses repeated polynomial extended-GCD identities.

- `"EquationSystem"`  
  Delegates to Mathematica's built-in `Apart`.

#### Multivariate methods

- `"MultivariateResidue"`  
  Designed for **linear** denominator factors. Uses null-relation elimination and basis residues.

- `"Leinartas"`  
  A multivariate decomposition method for **general polynomial** denominator factors.

## Repository layout

A typical repository layout is:

- `Mathematica/` — Mathematica package source
- `C/` — C library and standalone executable
- `Examples/` — Mathematica examples and tests

The Mathematica package consists of several `.m` files which are loaded together.  
Keep them in the same directory.

## Installation

### Clone the repository

```bash
git clone https://github.com/fekeshazy/LinApart.git
cd LinApart
```

## Mathematica installation

Copy the **entire Mathematica package directory** into a location where Mathematica can find it.

For example, copy the contents of `Mathematica/` into one of your Mathematica application directories.

Then load the package with:

```mathematica
Needs["LinApart`"]
```

If the package is not installed in a standard Mathematica path, you can also load it from its directory with `Get`.

## C installation

### Prerequisites

- a C compiler such as `gcc`
- `make`
- [GMP](https://gmplib.org/)

### Build

```bash
cd C
make
```

This builds:

- a standalone executable
- a library that can be linked from other software

The library is suitable for external use, for example from `FORM`.

## Mathematica usage

### Basic univariate example

```mathematica
expr = 1/((x - 1) (x + 1)^2);
LinApart[expr, x]
```

### Choose a univariate method

```mathematica
LinApart[expr, x, "Method" -> "Euclidean"]
```

### Basic multivariate linear example

```mathematica
expr = 1/(x y (x + y - 1));
LinApart[expr, {x, y}, "Method" -> "MultivariateResidue"]
```

### Basic multivariate polynomial example

```mathematica
expr = 1/((x^2 + y^2) (x^2 + y^2 + 1));
LinApart[expr, {x, y}, "Method" -> "Leinartas"]
```

## Mathematica options

`LinApart` supports the following options.

### `"Method"`

Chooses the decomposition algorithm.

For one variable:

- `"ExtendedLaurentSeries"`
- `"Euclidean"`
- `"EquationSystem"`

For several variables:

- `"MultivariateResidue"`
- `"Leinartas"`

If automatic method selection is enabled in your version of the package, the defaults are:

- univariate: `"ExtendedLaurentSeries"`
- multivariate: `"MultivariateResidue"`

### `"Factor"`

```mathematica
"Factor" -> True | False
```

Factors each additive term before decomposition.

### `"GaussianIntegers"`

```mathematica
"GaussianIntegers" -> True | False
```

Controls whether factorization is performed over the Gaussian integers.

### `"Extension"`

```mathematica
"Extension" -> {a1, a2, ...}
```

Passes an algebraic extension to Mathematica's factorization routines.

### `"Parallel"`

```mathematica
"Parallel" -> {useParallelQ, numberOfCores, temporaryPath}
```

Enables parallel evaluation where supported.

Example:

```mathematica
"Parallel" -> {True, 4, "/tmp/"}
```

### `"PreCollect"`

```mathematica
"PreCollect" -> True | False
```

Groups terms by common variable-dependent structure before decomposition.

### `"ApplyAfterPreCollect"`

```mathematica
"ApplyAfterPreCollect" -> f
```

Applies a function such as `Factor` to the variable-independent coefficient during precollection.

## Notes on the methods

### Univariate

The univariate implementation is intended for large symbolic expressions and supports higher powers of irreducible denominator factors.

### Multivariate residue method

The `"MultivariateResidue"` method is for **linear** denominator factors. It is fast and effective in that setting, especially for physics-style denominator structures.

### Leinartas method

The `"Leinartas"` method is intended for **general polynomial denominators**. It is the appropriate choice when non-linear denominator factors should be decomposed rather than factored away or ignored.

Multivariate decompositions are generally **not unique**.

## C standalone executable

The standalone executable performs the decomposition of functions of the form

\[
x^l \prod_{k=1}^{n} \frac{1}{(x-a_k)^{m_k}} .
\]

Run it as:

```bash
./LinApart <exponents> <roots>
```

where

- `<exponents>` is a comma-separated list of `l,m1,...,mn`
- `<roots>` is a comma-separated list of `a1,...,an`

### Example

To decompose

\[
\frac{x^3}{(x-a_1)^5 (x-a_2)^7 (x-a_3)^{11}},
\]

run

```bash
./LinApart "3,5,7,11" "a1,a2,a3"
```

## Examples and tests

Example notebooks, scripts, and regression tests are provided in the repository.

These include:

- univariate examples,
- multivariate linear-denominator examples,
- multivariate polynomial-denominator examples,
- stress tests with many denominator factors and high powers.

For large expressions, a useful correctness check is:

```mathematica
Together[expr - LinApart[expr, vars, "Method" -> "..."]] === 0
```

## Contributing

Contributions are welcome.

If you find a bug, have a feature request, or want to improve the implementation, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License.
