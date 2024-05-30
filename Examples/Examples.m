(* ::Package:: *)

(* ::Section:: *)
(*Load*)


SetDirectory[NotebookDirectory[]]


Import["../Mathematica/LinApart.m"];


(* ::Section:: *)
(*Examples*)


(*
The command LinApart[expr, var] returns the partial fraction decomposition of expr 
with respect to the variable var. The Head of var must be Symbol.
*)


LinApart[1/((1 + x)(2 + x)(3 + x)),x]


(*
Non-linear denominators, denominators with purely symbolic exponents, as well as 
non-rational functions of x are by default ignored during decomposition.
*)


LinApart[x^p Log[x]/((1 + x)(2 + x)(3 + x)(1 + x^2)),x]


(*
When the option Factor is set to True, the input expression is factorized term-by-term 
(over the integers) before the partial fraction decomposition. In order to avoid any 
unnecessary computation, the factorization only affects the variable-dependent part.
*)


LinApart[1/((1 - a^2)(1 - x^2)),x,"Factor"->True]


(*
Factorization can be extended to allow for constants that are Gaussian integers.
*)


LinApart[1/((1 + x)(1 + x^2)),x,"Factor"->True,"GaussianIntegers"->True]
%//ComplexExpand


(*
Rational numbers in exponents are manipulated such that any explicit rational exponent 
in the output is smaller than one. This prescription is adopted in order to reproduce 
the behaviour of the Apart command on such expressions.
*)


LinApart[1/((1 + x)(2 + x)^(1/2) (3 + x)^(1/3+p)),x]


(*
The same rational function can appear many times in the input with different 
coefficients. In such cases it can be more advantageous to first gather every 
unique x-dependent structure. The option PreCollect, when set to True, does 
exactly this.
*)


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));

LinApart[expr, x]//AbsoluteTiming
LinApart[expr, x, "PreCollect" -> True]//AbsoluteTiming

LinApart[expr, x]//MaxMemoryUsed
LinApart[expr, x, "PreCollect" -> True]//MaxMemoryUsed


(*
When the PreCollect option is set to True, one may further specify the option
ApplyAfterPreCollect, which takes a pure function (e.g., Factor, Simplify, Toghether) 
and applies it to the x-independent coefficients.
*)


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));
  
LinApart[expr, x, "PreCollect" -> True,
"ApplyAfterPreCollect" -> Factor]


(*
If the coefficients are large, performing the factorization/simplification/etc. can 
become expensive in terms of runtime. In order to mitigate this, the function 
GatherByDenominator is provided, which however may not lead to full simplification.
*)


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));
  
GatherByDenominator[expr]


(*
GatherByDenominator can be passed as a value to the option ApplyAfterPreCollect
*)


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));
  
LinApart[expr, x, "PreCollect" -> True,
"ApplyAfterPreCollect" -> GatherByDenominator]


(*
This example emerges during the analytic computation of phase space integrals 
relevant for setting up a local subtraction scheme beyond next-to-leading order. 
We estimate that the time required for Apart to finish the decomposition is more 
than 10^9 seconds.
*)


MaxAllowedTime=10;

expr=1/((-4 + y)*(1 - y + xb*y)*(2 - y + xb*y)*(4 - y + xb*y)*(1 - xa - y + xb*y)^3*
  (-1 + xa - y + xb*y)*(-4 - 4*xb - y + xb*y)*(-4*xb - y + xb*y)*
  (-4*xa - 4*xb - y + xb*y)*(4*xa - 4*xb - y + xb*y)*(2 + 2*xb - y + xb*y)^3*
  (6 + 2*xb - y + xb*y)*(2 - 4*xa + 2*xb - y + xb*y)*(2 + 4*xa + 2*xb - y + xb*y)*
  (-1 + xa - xa*y + xa*xb*y)*(1 + xa - xa*y + xa*xb*y)*(-2 + 2*xa - xa*y + xa*xb*y)*
  (2 + 2*xa - xa*y + xa*xb*y)*(-xb + xa*xb - xa*y + xa*xb*y)^3*
  (-4 + 2*xa + 2*xa*xb - xa*y + xa*xb*y)*(4 + 2*xa + 2*xa*xb - xa*y + xa*xb*y)*
  (1 - 2*xa + xa^2 - y - xa*y + xb*y + xa*xb*y)*
  (2*xb - 2*xa*xb + xa*y - xb*y - xa*xb*y + xb^2*y)^3);

LinApart[expr,y];//AbsoluteTiming
TimeConstrained[Apart[expr,y],MaxAllowedTime]



