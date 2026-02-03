(* ::Package:: *)

(*
In this notebook I collected all of the test cases, which I could think of. 
Some are from dr. Sam von Thurenhaut and some are from dr. G\[AAcute]bor Somogyi as well.
*)


(* ::Section:: *)
(*Load*)


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]]
Import["../FastApart.m"];


(* ::Input::Initialization:: *)
?FastApart


(* ::Section::Closed:: *)
(*fn0:: normalization check; *)


expr=1/(x+y)/(a x+2)^2;

Apart[expr,x]
FastApart[expr,x]//GatherByDependency[#,x]&

expr-%%//Factor//If[#!=0, Interrupt[], #]&
expr-%%//Factor//If[#!=0, Interrupt[], #]&


expr=f x^4/(a x+b)/(a x+c);

Apart[expr,x]
FastApart[expr,x]//GatherByDependency[#,x]&

expr-%%//Factor//If[#!=0, Interrupt[], #]&
expr-%%//Factor//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn1:: Non-integer power in denominator; *)


expr=1/(x+1)/(x^2+2)^(5/2);

Apart[expr,x]
FastApart[expr,x]//GatherByDependency[#,x]&

expr-%%//Factor//If[#!=0, Interrupt[], #]&
expr-%%//Factor//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
fn1=(x^2+a)^(-3/2)/(x+b)/(x+c);

pf1=FastApart[fn1,x]//Expand
pf2=Apart[fn1,x]//Expand

fn1-%%//Factor//If[#!=0, Interrupt[], #]&
fn1-%%//Factor//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
fn1=(x^3+a x -b)^(-3/2)/(x+b)/(x+c);

pf1=FastApart[fn1,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn1,x]//GatherByDependency[#,x,Together]&

fn1-%%//Factor//If[#!=0, Interrupt[], #]&
fn1-%%//Factor//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn2:: Non-integer power in numerator;*)


(*
This weird thing is caused by the Factoring
*)


(* ::Input::Initialization:: *)
fn2=x^(5/7)/(x+1)/(x^2+4x+2)/(x+3)

pf1=FastApart[fn2,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn2,x]//GatherByDependency[#,x,Together]&

fn2-%%//Together//If[#!=0, Interrupt[], #]&
fn2-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn3:: Sum;*)


(* ::Input::Initialization:: *)
fn3=x/(x+5)^2+x^2/(x^3+x^2+1)/(x+2)

pf1=FastApart[fn3,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn3,x]//GatherByDependency[#,x,Together]&

fn3-pf1//Together//If[#!=0, Interrupt[], #]&
fn3-pf2//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn4:: 1/Log[x];*)


(* ::Input::Initialization:: *)
fn4=1/(x+1)/(x^6+I x^3+2y)/Log[x];

pf1=FastApart[fn4,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn4,x]//GatherByDependency[#,x,Together]&

fn4-pf1//Together//If[#!=0, Interrupt[], #]&
fn4-pf2//Together//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
fn4=1/(x+1)/(x+2)/Log[Sqrt[x]];

pf1=FastApart[fn4,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn4,x]//GatherByDependency[#,x,Together]&

fn4-pf1//Together//If[#!=0, Interrupt[], #]&
fn4-pf2//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn5:: x in exponent;*)


(* ::Text:: *)
(*It was an interesting one in the sense, that the bug was in fact not in my code, but in Exponent, which I use in stage 1, to determine the polynomial power of the numerator.*)
(**)
(*It turns out if we give only one term to Exponent, where the power is not independent of the variable, then it does nothing with it. But as soon as we multiply it with anything, no matter what it gives the right answer 0. See examples below.*)


(*Exponent[x^2,x]
Exponent[1+x+x^2,x]

Exponent[Log[x],x]
Exponent[1+x^2 Log[x],x]

Exponent[Exp[x],x]
Exponent[2^x,x]

Exponent[10 Exp[x],x]
Exponent[10 2^x,x]

Exponent[x Exp[x],x]
Exponent[x 2^x,x]*)


(* ::Input::Initialization:: *)
fn5=Exp[x]/(x^3+1)/(x^2+2)^2

pf1=FastApart[fn5,x]//GatherByDependency[#,x,Together]&
pf2=Apart[fn5,x]//GatherByDependency[#,x,Together]&

fn5-pf1//Together//If[#!=0, Interrupt[], #]&
fn5-pf2//Together//If[#!=0, Interrupt[], #]&


expr=2^x/(x^3+1)/(x^2+2)^2;

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(x^3+1)/(x^2+2)^2;

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn6:: Independent function;*)


(* ::Input::Initialization:: *)
expr=1/(x^4+1)/(x+2);
fn6=FastApart[expr,y]

fn6-expr//Factor//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn7:: Constant a[0];*)


fn7=1/(x^3-a[0])/(x-a[1])

FastApart[fn7,x]//GatherByDependency[#,x,Together]&
Apart[fn7,x]//GatherByDependency[#,x,Together]&

fn7-%%//Together//If[#!=0, Interrupt[], #]&
fn7-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn8:: Non symbol variable;*)


(* ::Text:: *)
(*The error was caused because sam[1] is not a symbol. There is two way to fix this:*)
(**)
(*1. The first is to change the variable to a symbol. We must do it, because most of the functions operate this way eg. D or Exponent. So they require symbols. Of course in the end we must change it back, which can cost significant amount of time.*)
(**)
(*2. Throw an error, when the variable is not a Symbol.*)
(**)
(*I chose the former.*)


fn8=1/(sam[1]^4+x^3)/(sam[1]+2)

FastApart[fn8,sam[1]]
Apart[fn8,sam[1]]

fn8-%%//Together//If[#!=0, Interrupt[], #]&
fn8-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn9:: Non-linearity in denominator; behavior differs from Apart*)


expr=1/(x^3+Log[x])/(x+1)

expr//Apart[#,x]&
expr//FastApart[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(*The routine does not recoginze denominators, whcih are not polynomials of x.*)
expr=1/(x+Sqrt[x])/(x+1)

expr//Apart[#,x]&
expr//FastApart[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&

(*****************************************************************************)

expr=1/(x+Sqrt[x])/(x+1)

expr//Apart[#,x]&
expr//FastApart[#,x, "Factor"->True, "GaussianIntegers"->True]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/(x+x^7/2)/(x+1)

expr//Apart[#,x]&
expr//FastApart[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn10:: Sign of x;*)


expr=1/(-x^3+10 x^2+1)/(x+2);

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn11:: Polynomial division;*)


(* ::Input::Initialization:: *)
fn=x^5/(x^2+1)+x^3/(x^3-1)/(x+1);

FastApart[fn,x]//GatherByDependency[#,x,Together]&
Apart[fn,x]//GatherByDependency[#,x,Together]&

fn-%%//Together//If[#!=0, Interrupt[], #]&
fn-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn12:: Non-linearity in denominator; behavior differs from Apart*)


(* ::Input::Initialization:: *)
fn2=(x^2+3x/5+1)/(x^2+2)^5/(x^3+1)/(Sqrt[x]+Sqrt[y]+2)/x^7/Log[x]+x/(y+1)/(y+2)

FastApart[fn2,x]//GatherByDependency[#,x,Together]&
Apart[fn2,x]//GatherByDependency[#,x,Together]&

fn2-%%//Together//If[#!=0, Interrupt[], #]&
fn2-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn13:: Non-linearity in denominator;*)


(* ::Input::Initialization:: *)
fn3=1/Log[1/(x^3+3)/(x+2)]/(x^2+x+1)/(x^5+2)

FastApart[fn3,x]//GatherByDependency[#,x,Together]&
Apart[fn3,x]//GatherByDependency[#,x,Together]&

fn3-%%//Together//If[#!=0, Interrupt[], #]&
fn3-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn15:: Complex number denominator power;*)


expr=I/(x-1.2)^1.2/(x^3+x-b)^(1/2)/(x^2-c)^I;

expr//FastApart[#,x]&//Expand
expr//Apart[#,x]&

expr-%%//Rationalize//Together//If[#!=0, Interrupt[], #]&
expr-%%//Rationalize//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn16:: Denominator Exponent is a real number;*)


expr=(x^(3/2) Sqrt[-b+x] Sqrt[-c+x])/((a-b)^2 (a-c)^2 (-a+x^2+b x))

expr//FastApart[#,x]&//Expand
expr//Apart[#,x]&

expr-%%//Factor//If[#!=0, Interrupt[], #]&
expr-%%//Factor//If[#!=0, Interrupt[], #]&


expr=x^2/(-c+x^3)^2;

FastApart[expr,x]//Expand
Apart[expr,x]//Expand

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^1.5/(x-a)^2/(x^3-b)^(1/2)/(x^2-c)^1.5;

expr//FastApart[#,x]&//Expand
expr//Apart[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn17:: Not number denominator power;*)


expr=1/(x^2+x-a)^2/(x^4-b)^(1/2)/(x^3-c)^pow/(x^2-d)^(a+b)

expr//FastApart[#,x]&//Expand
expr//Apart[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn18:: Non-number exponent in numerator; *)


(* ::Input::Initialization:: *)
expr=x^b/((1+x^3)(2-x));

FastApart[expr,x]//GatherByDependency[#,x]&

expr-%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn18:: Complicated non-number exponent in denominator;*)


expr=1/((1+x)(2-x-x^2)(3+x^3)^(f[x]^x))

FastApart[expr,x]//GatherByDependency[#,x]&
%-expr//Simplify//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn19:: Denominator exponent has a form Plus[a_Integer, b_Complex || b_Non-number];*)


(* ::Input::Initialization:: *)
expr=1/((1+x^3)(2-x-x^2)(6+2x)^(2+a[2]));

FastApart[expr,x]//GatherByDependency[#,x]&
FastApart[expr,x,"Factor"->True]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
expr=1/((1-x^3)(2+x-x^2)(6+2x)^(2+I));

FastApart[expr,x]//GatherByDependency[#,x]&
FastApart[expr,x,"Factor"->True]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn20:: Appearance of I in case of rational denominator exponent;*)


(* ::Input::Initialization:: *)
expr=1/((1-x^3)(2-x^2)(3-x^2)^(1/2))

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
expr=x^15/((1-x^3)(2-x^2)(3-x^2)^(1/2))

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn21:: Real exponent of numerator if it is not a monomial;*)


(* ::Input::Initialization:: *)
expr=x^x/((1-x^2)(2+x^3)^-0.5)//Expand

FastApart[expr,x]

expr-%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn22:: G function in expression; *)


(* ::Input::Initialization:: *)
expr=G[0,x/(x^2+1)]/(x^3+1)/(x^2+x+2)

FastApart[expr,x]//GatherByDependency[#,x]&

expr-%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn25:: Issue with rational powers in the numerator;*)


expr=1/((1-x^2) (2+x-x^3) (3-x)^(-1/2))

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/((1+x^3) (2+x)^(-1/2))

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn26:: numerator is a higher order polynomial on some power;*)


(* ::Text:: *)
(*This case should be absent in every calculation, but it looks like the function can handle it.*)


(* ::Input::Initialization:: *)
expr=1/((1-x^3) (2+x-x^2) (3-x^2)^(-2))

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn27:: Issue with rational + symbolic/complex powers in the numerator; *)


(* ::Input::Initialization:: *)
expr1=1/((1-x) (2+x-x^5) (3-x^2)^(a-3/2))
expr2=1/( (2+x-x^5) (3-x^2)^(I-3/2))


(* ::Input::Initialization:: *)
(* In this case, the 3/2 power is not decomposed *)
FastApart[expr1,x]//GatherByDependency[#,x]&
Apart[expr1,x]//GatherByDependency[#,x]&

expr1-%%//Together//If[#!=0, Interrupt[], #]&
expr1-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
(* In this case, the 3/2 power is not decomposed *)
FastApart[expr2,x]//GatherByDependency[#,x]&
Apart[expr2,x]//GatherByDependency[#,x]&

expr2-%%//Together//If[#!=0, Interrupt[], #]&
expr2-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn28:: Issue with Log[x] and G[...,x] mixed:: *)


(* ::Input::Initialization:: *)
expr=Log[x](1/((1+x-x^5)(2-x^3))+G[0,1,x]/Log[x])

FastApart[expr,x]//GatherByDependency[#,x]&
Apart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn30:: Non-number Power is an expression; *)


(* Note the power is rational > 1 in the numerator and there is a symbolic or complex part as well *)


(* ::Input::Initialization:: *)
(* In this case, the 3/2 power is not decomposed *)
expr=x/((1-x^2) (2-x^3-a^2)^(3/2) (3-x)^(b^2+2(a+1/2)+(b+3)^3))

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Simplify//If[#!=0, Interrupt[], #]&
expr-%%//Simplify//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn31:: Improper fraction with rational power; *)


expr=Log[x](b-x)^(4/3) x^11/2/(1-x^2)/(2 a-x^3)/(3-x)^(3/2)/(4-x)^(3/2+a)/(5-x)^(5/2+I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn32:: Improper fraction with rational power and non-number/complex part;*)


expr=Log[x] x^11/2+b/(1-x^2)/(2-x^4+a x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=Log[x] x^11/2+I/(1-x^2)/(2-x^4+a x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn33:: Test for different kinds of numerators; *)


expr=(f-x)^(1/2+4a[1]) (d-x^3)^(2+3I) (c-x^2)^1.2 (b-x)^(1/3) x^(11/2) Log[x]Sin[x]/(a-x^2)/(b-a x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn34:: A systematic check without non-linearity*)


(* ::Subsection:: *)
(*Denominators with integer powers*)


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator number powers*)


expr=1/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^3/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator complex powers*)


expr=x^I/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3+I)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3/2+I)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+I)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 I)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 I)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+I))/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator non-number powers*)


expr=x^a/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1+a)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2+a)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+a)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 a)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 a)/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+a))/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+a)+Log[x])/(a-x^2)/(c+x-x^3)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsection:: *)
(*Denominators with different number powers*)


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator number powers*)


expr=1/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^3/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator complex powers*)


expr=x^I/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3+I)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3/2+I)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+I)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 I)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 I)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+I))/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator non-number powers*)


expr=x^a/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1+a)/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2+a)/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+a)/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 a)/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 a)/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+a))/x^(1/4)/(a-x^2)^2/(c+x-x^3)^(1/2)/(3-x)^(5/2)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsection:: *)
(*Denominators with different complex powers*)


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator number powers*)


expr=1/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^3/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator complex powers*)


expr=x^I/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)
expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3+I)/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(3/2+I)/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+I)/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 I)/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 I)/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+I))/x^(1/4+I)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Subsubsection::Closed:: *)
(*Fractions with different numerator non-number powers*)


expr=x^a/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1+a)/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(1/2+a)/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+a)/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3 a)/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 a)/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^(5/2+3/2 (2/3+a))/x^(1/4+b)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)/(3-x^3)^(5/2+3I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn35:: Test for different kinds of non-linearity; *)


expr=1/(Log[x]+x)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/(1+Log[x]x)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/(Sin[x]+x)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=a/Log[x]/x^2/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/(a+x^3)^(1/3)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/(1+x^2)/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=G[0,1,x]/(1-x^2)^2/(2-a x-x^3)^(1/2 I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=a G[0,1,x](1+x^4)Cos[(1-x)^(1/2)]Sin[x]/Log[x]/(1+x^2)/(1+x^3)^(1/3)/(Log[x]+x)/(1-x)/(2-x)+G[0,1,x]

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn35:: combining different special cases;*)


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(1/2+I+a)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(1.2+a)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(1.2+I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(1.2+2.1a+3.1I)

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(2+a+2(b+1.2))

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(2+I+2(I+1.2))

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=1/Log[x]/(1-x)/(a-x^2)^2/(2-a x-x^3)^(2+I+2b(I+1.2 a)+I(2+b))

expr//FastApart[#,x]&//GatherByDependency[#,x,Together]&
expr//Apart[#,x]&//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn38:: Polynomial with one complex denominator;*)


expr=x/(1-x^2+a x)^(1-I)

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(1-x^2+a x)^(1-a);

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/(1-x^2+a x)^(1-"a");

FastApart[expr,x]//GatherByDependency[#,x,Together]&
Apart[expr,x]//GatherByDependency[#,x,Together]&

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section:: *)
(*fn39:: Factored polynomial under a common power;*)


expr=1/((1-x+a x)(2-x))^(2-I)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/((1-x)(2-x))^(1-I)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Simplify//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x Log[x]/((1-x)(2-x))^(1-I)/(Log[x]-x+2a)^(2+I)/(Log[x]x+2a)^(2+I)/(x+2a x+1+nb)/(x^2+1)/(3+x)^(2/3)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x/((1-x)(2-x))^(2-a)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^2/((1-x)(2-x))^(1-I)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


expr=x^5/((1-x)(2-x))^(1-I)

FastApart[expr,x]
Apart[expr,x]

expr-%%//Together//If[#!=0, Interrupt[], #]&
expr-%%//Together//If[#!=0, Interrupt[], #]&


(* ::Section::Closed:: *)
(*fn41:: Non-Factored denominators with the same root;*)


expr=1/((1+x)(2+x-x^2)(3+x^3)^(f[x]^x))

FastApart[expr,x]
%-expr//Simplify//If[#!=0, Interrupt[], #]&


(* ::Input::Initialization:: *)
expr=1/((1+x^3)(2+x-x^2)(6+2x)^(2+I));

FastApart[expr,x]//GatherByDependency[#,x]&

expr-%%//Together//If[#!=0, Interrupt[], #]&



