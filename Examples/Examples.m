(* ::Package:: *)

(* ::Section::Closed:: *)
(*Load*)


SetDirectory[NotebookDirectory[]]


Import["..\\Mathematica\\LinApart.m"];


exampleSimple=Import["exampleSimple.mx"];


structures=Import["PF_structures"]//ToExpression;


(* ::Section:: *)
(*Test*)


(* ::Subsection::Closed:: *)
(*Examples from the article*)


LinApart[1/((1 + x)(2 + x)(3 + x)),x]


LinApart[x^p Log[x]/((1 + x)(2 + x)(3 + x)(1 + x^2)),x]


LinApart[1/((1 - a^2)(1 - x^2)),x,"Factor"->True]


LinApart[1/((1 + x)(1 + x^2)),x,"Factor"->True,"GaussianIntegers"->True]
%//ComplexExpand


LinApart[1/((1 + x)(2 + x)^(1/2) (3 + x)^(1/3+p)),x]


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));

LinApart[expr, x, "PreCollect" -> True]


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));
  
LinApart[expr, x, "PreCollect" -> True,
"ApplyAfterPreCollect" -> Factor]


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));
  
LinApart[expr, x, "PreCollect" -> True,
"ApplyAfterPreCollect" -> GatherByDenominator]


expr=1/((\[Minus]4 + y)(1 \[Minus] y + xb y)(2 \[Minus] y + xb y)(4 \[Minus] y + xb y)(1 \[Minus] xa \[Minus] y + xb y)^3
(\[Minus]1 + xa \[Minus] y + xb y)(\[Minus]4 \[Minus] 4xb \[Minus] y + xb y)(\[Minus]4xb \[Minus] y + xb y)(\[Minus]4xa \[Minus] 4xb \[Minus] y + xb y)
(4xa \[Minus] 4xb \[Minus] y + xb y)(2 + 2xb \[Minus] y + xb y)^3 (6 + 2xb \[Minus] y + xb y)
(2 \[Minus] 4xa + 2xb \[Minus] y + xb y)
(2 + 4xa + 2xb \[Minus] y + xb y)(\[Minus]1 + xa \[Minus] xay + xa xb y)(1 + xa \[Minus] xay + xa xb y)
(\[Minus]2 + 2xa \[Minus] xa y + xa xb y)(2 + 2xa \[Minus] xa y + xa xb y)(\[Minus]xb + xa xb \[Minus] xa y + xa xb y)^3
(\[Minus]4 + 2xa + 2xa xb \[Minus] xay + xa xb y)(4 + 2xa + 2xa xb \[Minus] xa y + xa xb y)
(1 \[Minus] 2xa + x2 a \[Minus] y \[Minus] xa y + xb y + xa xb y)
(2xb \[Minus] 2xa xb + xa y \[Minus] xb y \[Minus] xa xb y + x2 by)^3);

res=LinApart[expr,y];//AbsoluteTiming


(* ::Subsection::Closed:: *)
(*More basic examples*)


(*
Most basic use of the function.
*)


LinApart[1/((1 + x)(2 - x)),x]


(*
Non-linear denominators and, denominators with purely symbolic exponents, as well
as non-rational functions of the variable are by default ignored.
*)


LinApart[1/((1 + x)(2 - x)(1 - x^2)), x]
LinApart[1/((1 + x)(2 - x)(3 + x)^(a[1])), x]
LinApart[1/((1 + x)(2 - x)(3 + x)^(3+I)), x]


(*
In case of fractional powers it factors out the appropriate order, 
does the decomposition then multiplies it back. This behaviour is consistent with
Apart.

For example: LinApart[1/(1+x)/(2+x)^(1/2),x] -> Sqrt[2+x] LinApart[1/(1+x)/(2+x),x]
*)


Apart[1/((1 + x)(2 + x)^1/2), x]
LinApart[1/((1 + x)(2 + x)^1/2), x]
LinApart[1/((1 + x)(2 + x)^0.5), x]


(*
When the option Factor set to True, the input expression is factorized term-
by-term before the partial fraction decomposition; by default only over the integers.
Though can be extended to allow for constants that are Gaussian integers.

In order to avoid any unnecessary computation, the factorization only affects
the variable dependent part.
*)


LinApart[1/((1 + x)(2 - x)(1 - x^2)), x, "Factor"->True]
LinApart[(a^2-b^2)/((1 + x)(2 - x)(1 - x^2)), x, "Factor"->True]

Apart[1/((1 + x)(2 - x)(1 + x^2)), x]
LinApart[1/((1 + x)(2 - x)(1 + x^2)), x, "Factor"->True]
LinApart[1/((1 + x)(2 - x)(1 + x^2)), x, "Factor"->True, "GaussianIntegers"->True]//ComplexExpand


(*
When the PreCollect option is set to True, one may further specify the option
ApplyAfterPreCollect, which takes a pure function and applies it to the x-independent
coefficients. This can lead to cancellation further reducing the number 
of calculation.

The choice of the appropriate ApplyAfterPreCollect set-ting is highly 
context-dependent. Obvious choices would be: Factor, Simplify or Together.
*)


expr=C1 f[x]/((1 + x)(2 - x)(3 + x))+
		C2 f[x]/((1 + x)(2 - x)(3 + x))+
			C3/((1 + x)(2 - x)(3 + x))+
				C4/((1 + x)(2 - x)(3 + x));

expr//LinApart[#,x]&//Length//AbsoluteTiming
expr//LinApart[#,x,"PreCollect"->True]&//Length//AbsoluteTiming


(* ::Subsection::Closed:: *)
(*Simple example*)


(*
This expression also arose during our calculations.
*)


exampleSimple//Length
vars=exampleSimple//Variables


(*
Numerical values for checks.
*)


numRules=Table[vars[[i]]->1/i/10,{i,Length[vars]}]


(*
Here one can see the timings with different partial fraction decomposition 
strategies.
*)


(* ::Input::Initialization:: *)
tmpApart=exampleSimple//Apart[#,x1]&;//MaxMemoryUsed//AbsoluteTiming


tmpLinApart=exampleSimple//LinApart[#,x1,"PreCollect"->True,"ApplyAfterPreCollect"->Factor]&;//MaxMemoryUsed//AbsoluteTiming


numOriginal=exampleSimple/.numRules//N;

tmpApart/numOriginal/.numRules//N
tmpLinApart/numOriginal/.numRules//N


(* ::Subsection::Closed:: *)
(*Fractions*)


(*
These are some fraction which surfaced during our calculations.

One can increase the TimeConstrained's maximum time and see 
the difference between Apart's and LinApart's runtime for seemingly simple examples.
*)


structures//Length


Monitor[
	timingStructuresLinApart=Table[
		structures[[counter]]//LinApart[#,x2]&//AbsoluteTiming,
		{counter,1,Length[structures]}
	];,
{counter,Length[structures],structures[[counter]]}]


maximumTime=1;

Monitor[
	timingStructuresApart=Table[
			TimeConstrained[
				Apart[structures[[counter]],x2]//AbsoluteTiming,
				maximumTime,
				{Overtime,Overtime}
			],
		{counter,1,Length[structures]}
	];,
{counter,Length[structures],structures[[counter]]}]


timingStructuresLinApart[[All,1]]
timingStructuresApart[[All,1]]
