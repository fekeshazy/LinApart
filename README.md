# LinApart

The LinApart routine can be used for efficiently performing the univariate partial fraction decomposition of large symbolic expressions, when the denominators are fully factorized. The routine is described in the following paper: [arXiv:2405.20130](https://arxiv.org).

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

The command LinApart[expr, var] returns the partial fraction decomposition of expr with respect to the variable var. The head of var must be Symbol. 

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
