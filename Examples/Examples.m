(* ::Package:: *)

(* ::Section:: *)
(*Load*)


SetDirectory[NotebookDirectory[]]


Import["..//Mathematica//LinApart.m"];


exampleSimple=Import["exampleSimple.mx"];


structures=Import["PF_structures"]//ToExpression;


(* ::Section:: *)
(*Tests*)


(* ::Subsection::Closed:: *)
(*Examples from the first article*)


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
(*More basic linear examples*)


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
(*Examples from the first article*)


(*
The example done by hand in the article.
*)


expr=x^10/(x^2+x+1)^2/(x^2-x+1)^2;

tmpApart=expr//LinApart[#,x,"Method"->"EquationSystem"]&;//AbsoluteTiming
tmpEuclidean=expr//LinApart[#,x,"Method"->"Euclidean"]&;//AbsoluteTiming
tmpLinApart=expr//LinApart[#,x,"Method"->"ExtendedLaurentSeries"]&;//AbsoluteTiming

expr-tmpApart//Together
expr-tmpEuclidean//Together
expr-tmpLinApart//Together


(*
The equation-based method has an advantage when only a single symbol is 
present, but as soon as additional symbols enter the coefficients this 
advantage diminishes.
*)


expr=x^2/Product[
				Sum[
						RandomInteger[{1,10^5}] x^j,
					{j,0,3}
					]^2,
			{i,1,5}
			];
			
expr//LinApart[#,x]&;//AbsoluteTiming
expr//Apart[#,x]&;//AbsoluteTiming


expr=x^2/Product[
				Sum[
					(RandomInteger[{1,10^5}] y +
					RandomInteger[{1,10^5}] a) x^j,
				{j,0,3}
				]^2,
			{i,1,5}
			];
			
expr//LinApart[#,x]&;//AbsoluteTiming
expr//Apart[#,x]&;//AbsoluteTiming


(* ::Subsection::Closed:: *)
(*More basic non-linear examples*)


expr=1/(-3 x-a1)/(5x^2+b01 x -b00)

tmpApart=expr//LinApart[#,x,"Method"->"EquationSystem"]&;//AbsoluteTiming
tmpEuclidean=expr//LinApart[#,x,"Method"->"Euclidean"]&;//AbsoluteTiming
tmpLinApart=expr//LinApart[#,x,"Method"->"ExtendedLaurentSeries"]&;//AbsoluteTiming

expr-tmpApart//CheckNumericallyIfZero
expr-tmpEuclidean//CheckNumericallyIfZero
expr-tmpLinApart//CheckNumericallyIfZero


expr=1/(x-a1)/(x^2-2/3 y x+ 1)^2/(x^3+ x^2-b21 x+ b20)^2

tmpApart=expr//LinApart[#,x,"Method"->"EquationSystem"]&;//AbsoluteTiming
tmpEuclidean=expr//LinApart[#,x,"Method"->"Euclidean"]&;//AbsoluteTiming
tmpLinApart=expr//LinApart[#,x,"Method"->"ExtendedLaurentSeries"]&;//AbsoluteTiming

expr-tmpApart//CheckNumericallyIfZero
expr-tmpEuclidean//CheckNumericallyIfZero
expr-tmpLinApart//CheckNumericallyIfZero


(* ::Input::Initialization:: *)
expr=1/(x-a1)/(x^2-2 x+ y+10)^2/(x^2-Exp[y] x+ 12/3)

tmpApart=expr//LinApart[#,x,"Method"->"EquationSystem"]&;//AbsoluteTiming
tmpEuclidean=expr//LinApart[#,x,"Method"->"Euclidean"]&;//AbsoluteTiming
tmpLinApart=expr//LinApart[#,x,"Method"->"ExtendedLaurentSeries"]&;//AbsoluteTiming

expr-tmpApart//Together
expr-tmpEuclidean//Together
expr-tmpLinApart//Together


expr=x^2/Product[ Sum[b[i,j] x^j, {j,0,2}]^2, {i,1,4}]

tmpEuclidean=expr//LinApart[#,x,"Method"->"Euclidean"]&;//AbsoluteTiming
tmpLinApart=expr//LinApart[#,x,"Method"->"ExtendedLaurentSeries"]&;//AbsoluteTiming


(* ::Subsection::Closed:: *)
(*Examples for parallelization from the article*)


CloseKernels[];
LaunchKernels[4];
DistributeDefinitions[LinApart]


(*
For many denominators with simple poles, parallelization 
in Mathematica provides essentially no speed-up.
*)


expr=x^2/Product[ Sum[b[i,j] x^j, {j,0,1}], {i,1,30}];

expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


(*
However, in the high-multiplicity limit, runtime can indeed be reduced.
*)


expr=x^2/Product[(x-b[i,1])^20, {i,1,15}];

expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


(*
In the general case, the situation is similar.
*)


expr=x^2/Product[Sum[b[i,j] x^j, {j,0,2}]^2, {i,1,8}];

expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


expr=x^2/Product[Sum[b[i,j] x^j, {j,0,2}]^4, {i,1,5}];
expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


(*
The poorest performance occurs when the degrees of the denominators 
are increased.
*)


expr=x^2/Product[Sum[b[i,j] x^j, {j,0,2}]^2, {i,1,7}];
expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


expr=x^2/Product[Sum[b[i,j] x^j, {j,0,3}]^2, {i,1,5}];
expr//LinApart[#,x]&;//AbsoluteTiming
expr//LinApart[#,x,"Parallel"->{True,4,NotebookDirectory[]}]&;//
		AbsoluteTiming


(* ::Subsection::Closed:: *)
(*Simple linear example*)


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
(*Linear fractions*)


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
