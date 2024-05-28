# LinApart

This is the repository of the LinApart package, which can be used for efficiently performing the univariate partial fraction decomposition of large symbolic expressions, when the denominators are linear. Our method is based on the residue method and was presented in the following paper ... .

We provide a Wolfram Mathematica implementation and a C version. The former can be used out of the box and mimics the usage of Mathematica's native function Apart. While for the latter we include both a standalone executable as well as a library suitable for linking with other software (e.g. FORM).

Benchmarks and detailed description of both implementation can be found in the article mentioned above. Furthermore, we provide a file containing simple (including every example form the article) and more complicated real life examples.

If you found this package useful for your research please cite the article:)

Best wishes, The Authors!
