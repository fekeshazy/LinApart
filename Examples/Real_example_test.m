(* ::Package:: *)

(* ::Section::Closed:: *)
(*Load*)


SetDirectory[NotebookDirectory[]]


Import["LinApart.m"];


exampleSimple=Import["exampleSimple.mx"];


exampleComplicated=Import["exampleComplicated.mx"];


structures=Import["PF_structures"]//ToExpression;


(* ::Section:: *)
(*Test*)


(* ::Subsection::Closed:: *)
(*Examples from the article*)


(*
The usage of the LinApart command mimics that of the standard Apart command.

LinApart[expr, var] returns the partial fraction decomposition of expr with 
respect to the variable var.
*)


LinApart[1/((1 + x)(2 + x)(3 + x)),x]


(*
Non-linear denominators and, denominators with purely symbolic exponents, as well
as non-rational functions of the variable are by default ignored.
*)


LinApart[x^p Log[x]/((1 + x)(2 + x)(3 + x)(1 + x^2)),x]


(*
When the option Factor set to True, the input expression is factorized term-
by-term before the partial fraction decomposition; by default only over the integers.

In order to avoid any unnecessary computation, the factorization only affects
the variable dependent part.
*)


LinApart[1/((1 - a^2)(1 - x^2)),x,"Factor"->True]


(*
Factorization can be extended to allow for constants that are Gaussian integers.
*)


LinApart[1/((1 + x)(1 + x^2)),x,"Factor"->True,"GaussianIntegers"->True]
%//ComplexExpand


(*
In case of fractional powers it factors out the appropriate order, 
does the decomposition then multiplies it back.

For example: LinApart[1/(1+x)/(2+x)^(1/2),x] -> Sqrt[2+x] LinApart[1/(1+x)/(2+x),x]
*)


LinApart[1/((1 + x)(2 + x)^(1/2) (3 + x)^(1/3+p)),x]


(*
The same rational function structure can appear many times in the input with different
coefficients. In such cases, rather than applying the partial decomposition routine on
individual terms, one ought to gather every unique x-dependent structure, then apply 
the partial fraction decomposition, thus reducing the number of computation.

The option PreCollect does exactly this.
*)


expr = 2/((1 + x)(2 + x)) + 1/((1 - a)(1 + x)(2 + x))+
 a/((1 + x)(2 + x)) - a/((1 - a)(1 + x)(2 + x))-
  b/((1 + a)(1 + x)(2 + x)) - (a b)/((1 + a)(1 + x)(2 + x));

LinApart[expr, x, "PreCollect" -> True]


(*
When the PreCollect option is set to True, one may further specify the option
ApplyAfterPreCollect, which takes a pure function and applies it to the x-independent
coefficients. This can lead to cancellation further reducing the number 
of calculation.

The choice of the appropriate ApplyAfterPreCollect set-ting is highly 
context-dependent. Obvious choices would be: Factor, Simplify or Together.

Sometimes these can take too much time and some simplier gathering by denominators
would be also sufficient. For this we defined the GatherByDenominator function.
*)


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


(*
An example which emerged during our phase space integrals, when we tried to 
sett up a local subtraction scheme beyond next-to-leading order.

Based on our benchmarks we estimated to decompose this integral with Apart
in about 31 years (10^9s). Our algorithm does it under a second.
*)


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

Sometimes these can take too much time and some simplier gathering by denominators
would be also sufficient. For this we defined the GatherByDenominator function.
*)


expr=C1 f[x]/((1 + x)(2 - x)(3 + x))+
		C2 f[x]/((1 + x)(2 - x)(3 + x))+
			C3/((1 + x)(2 - x)(3 + x))+
				C4/((1 + x)(2 - x)(3 + x));

expr//LinApart[#,x]&//Length//AbsoluteTiming
expr//LinApart[#,x,"PreCollect"->True]&//Length//AbsoluteTiming


(* ::Subsection::Closed:: *)
(*Simple*)


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
The gathering is done by a function called GatherByDependency. It can be called
with 4 arguments:

GatherByDependency[
	expression,
	symbol or head or any expression which FreeQ can take,
	function to be applied to the coefficients,
	function to be applied to the dependent parts
	].
	
The last two argument has None as default value. For example if the users wants to
apply a function on the depedent part:

GatherByDependency[expr, x, None, Apart[#,x]& ]

or

GatherByDependency[expr, ep, None, Series[#,{ep,0,-2}]& ].
*)


(*
One can see that different simplifing attempts have different result. 
Factor does takes longer completly simplifies the coefficients leading to less
structure, while GatherByDenominator is faster but the number of variable dependent
terms is greater.

One important note, the function defined in the third argument will act on 
every coefficient. The fully variable indepent terms together build up the 
coefficient of the trivial structure (1), thus the function will be applied on that
as well.
*)


(* ::Input::Initialization:: *)
tmpGather=exampleSimple//GatherByDependency[#,x1]&;//AbsoluteTiming
tmpGatherFactor=exampleSimple//GatherByDependency[#,x1,Factor]&;//AbsoluteTiming
tmpGatherGatherByDenominator=exampleSimple//GatherByDependency[#,x1,GatherByDenominator]&;//AbsoluteTiming

numOriginal=exampleSimple/.numRules//N;

tmpGather/numOriginal/.numRules//N
tmpGatherFactor/numOriginal/.numRules//N
tmpGatherGatherByDenominator/numOriginal/.numRules//N


tmpGather//Select[#,FreeQ[#,x1]&]&//Length
tmpGatherFactor//Select[#,FreeQ[#,x1]&]&//Length
tmpGatherGatherByDenominator//Select[#,FreeQ[#,x1]&]&//Length


(*
Here one can see the timings with different partial fraction decomposition 
strategies.

One can decduce that colelction is essential in every case, even with a fast algorithm
like LinApart.
*)


(* ::Input::Initialization:: *)
tmpApart=exampleSimple//Apart[#,x1]&;//MaxMemoryUsed//AbsoluteTiming
tmpApartGather=exampleSimple//GatherByDependency[#,x1,Factor, Apart[#,x1]&]&;//MaxMemoryUsed//AbsoluteTiming


tmpLinApart=exampleSimple//LinApart[#,x1]&;//MaxMemoryUsed//AbsoluteTiming

tmpLinApartGather=exampleSimple//LinApart[#,x1,"PreCollect"->True]&;//MaxMemoryUsed//AbsoluteTiming
tmpLinApartGatherFactor=exampleSimple//LinApart[#,x1,"PreCollect"->True,"ApplyAfterPreCollect"->Factor]&;//MaxMemoryUsed//AbsoluteTiming
tmpLinApartGatherGatherByDenominator=exampleSimple//LinApart[#,x1,"PreCollect"->True,"ApplyAfterPreCollect"->GatherByDenominator]&;//MaxMemoryUsed//AbsoluteTiming


numOriginal=exampleSimple/.numRules//N;

tmpApart/numOriginal/.numRules//N
tmpApartGather/numOriginal/.numRules//N
tmpLinApart/numOriginal/.numRules//N
tmpLinApartGather/numOriginal/.numRules//N
tmpLinApartGatherFactor/numOriginal/.numRules//N
tmpLinApartGatherGatherByDenominator/numOriginal/.numRules//N


(* ::Subsection::Closed:: *)
(*Complicated*)


(*
This example also surfaced during our calculation.

In the first subsection we separeted the structures for demostration purposes.
In the second we provide the whole expression unfiltered.
*)


(* ::Subsubsection::Closed:: *)
(*Structures*)


(*
Here we can really see the power of LinApart by just running this subsection.
One can increase the TimeConstrained maximum time and see for themself 
the difference even in seemingly simple examples.
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


(* ::Subsubsection::Closed:: *)
(*Expression*)


(*
This subsection requires a great amount of time and RAM.
*)


exampleComplicated//Length
vars=exampleComplicated//Variables


numRules=Table[vars[[i]]->1/i/10,{i,Length[vars]}]


(* ::Input::Initialization:: *)
tmpGather=exampleComplicated//GatherByDependency[#,x2]&;//AbsoluteTiming
tmpGatherGatherByDenominator=exampleComplicated//GatherByDependency[#,x2,GatherByDenominator]&;//AbsoluteTiming

numOriginal=exampleComplicated/.numRules//N;

tmpGather/numOriginal/.numRules//N
tmpGatherFactor/numOriginal/.numRules//N
tmpGatherGatherByDenominator/numOriginal/.numRules//N


tmpLinApartGatherFactor=exampleSimple//LinApart[#,x1,"PreCollect"->True,"ApplyAfterPreCollect"->Factor]&;//MaxMemoryUsed//AbsoluteTiming
tmpLinApartGatherGatherByDenominator=exampleSimple//LinApart[#,x1,"PreCollect"->True,"ApplyAfterPreCollect"->GatherByDenominator]&;//MaxMemoryUsed//AbsoluteTiming


numOriginal=exampleSimple/.numRules//N;

tmpLinApartGatherFactor/numOriginal/.numRules//N
tmpLinApartGatherGatherByDenominator/numOriginal/.numRules//N
