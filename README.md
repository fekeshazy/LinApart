# LinApart

The LinApart routine can be used for efficiently performing the univariate partial fraction decomposition of large symbolic expressions. The routine is described in the following paper: [arXiv:2405.20130](https://arxiv.org/abs/2405.20130) and https://arxiv.org/pdf/2511.15735 .

Wolfram Mathematica and C language implementations of the routine are provided. The latter includes both a standalone version and a library suitable for linking with other software (e.g. FORM).

Example files for the Mathematica routine are also provided.

If you find this routine useful in your research please consider citing the article.

## Installation

1. Clone the repository:
```
git clone https://github.com/your-username/LinApart.git
```
2. Navigate to the project directory:
```
cd LinApart
```

### Mathematica

1. Copy the `LinApart.m` file from the `LinApart/Mathematica` directory to your Mathematica package directory.

2. The routine can be loaded with:
    ~~~mathematica
    Needs["LinApart`"]
    ~~~
    
### C

#### Prerequisites

- C compiler (e.g., GCC)
- GNU Make
- [GMP library](https://gmplib.org/)

#### Building the Executable and Library

1. Navigate to the C directory:
```
cd LinApart/C
```
2. Compile the library and standalone executable:
```
make
```

## Usage

### Mathematica

LinApart[expression, variable_Symbol, Options] 

The function gives the partrial fraction decomposition of fractions with linear denominators in the choosen variable; the variable must be a symbol. 

Options: 
	-Factor->True/False: factor each additive term in the expression; the default value is False.
	-GaussianInteger->True/False: factorization of the input expression is performed over the Gaussian integer; the default value is False.
	-Extension->{a[1], a[2], ...}: option for Factor; factors a polynomial allowing coefficients that are rational combinations of the algebraic numbers a[i].
	-Parallel->{True/False, NumberOfCores, TemporaryPath}: calculate the residues on multiple cores during the extended Laurent-series method.
	-PreCollect->True/False: gather by every unique structure in the expression; the default value is False.
	-ApplyAfterPreCollect -> pure function (e.g. Factor): applies the given function on the variable independent part of each term; the default value is None.


For more detailed usage instructions and examples, refer to the paper and the `LinApart/Examples` directory.

### C Standalone Executable
The standalone executable performs the partial fraction decomposition of the function
$x^{l} {\LARGE\Pi}^{n}_{k=1} \frac{1}{(x-a_k)^{m_k}}$

1. Run the standalone executable from the command line:
    ```
    ./LinApart <exponents> <roots>
    ```
    * &lt;exponents&gt;: Comma-separated list of $(n+1)$ exponents $l,m_1,\ldots, m_n$ (e.g., "1,2,3,4")
    * &lt;roots&gt;: Comma-separated list of $n$ roots $a_1,\ldots,a_n$ (e.g., "a,b,c")

    Example: to perform the partial fraction decomposition of $\frac{x^3}{(x-a1)^5(x-a2)^7(x-a3)^{11}}$, use
    ```
    ./LinApart "3,5,7,11" "a1,a2,a3"
    ```

## Contributing
Contributions are welcome! If you find any issues or have suggestions for
improvement, please open an issue or submit a pull request on the GitHub
repository.

## License
This project is licensed under the MIT License.
