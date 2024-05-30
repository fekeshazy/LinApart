(* ::Package:: *)

(* ::Section:: *)
(*LinApart*)


(*
This file contains the LinApart package, based on the article ....... .

It has four main sections.
	1. Helper functions: it contains all of smaller functions used in the main routine.
	2. LinApart front: it contains the definition fo the front function and input error handling.
	3. PreProcessor: it conaint the function called PreProcessor, which manipulates the input so 
	the formula (eq ....) presented in the article can be applied. 
	4. Text of messages: it contains the definition of the error/warning messages and the 
	short useage summary.
*)


(* ::Subsection::Closed:: *)
(*Welcome message*)


Print[

	"(****** LinApart 1.0 ******)"
	
];


(* ::Subsection::Closed:: *)
(*Helper functions*)


												(*Helper functions*)
		
		(*
		The Exponent function only gives the highest order of an expression. 
		I needed the exponent of each multiplicative term to determine the multiplicites of the denominators.
		*)
		
		(*
		-If the expression is free of the variable give back the expression itself.
		-If we have the desired structure give the expression and its power.
		
		The constant before the structure is not needed.
		*)
		
ClearAll[GetExponent]

GetExponent[a_. expr_^n_.,var_]:={expr^n,1}/;FreeQ[expr,var]
GetExponent[a_. expr_^n_.,var_]:={expr,n}/;!FreeQ[expr,var]


		(*
		The Collect function is very useful, but when one collects by head like expr//Collect[#, _den]& 
		it also factors the terms as well. Which means if we have an expression like: 
			C1*den[a1]C2 den[a2]*den[a3]+C1*den[a1]*den[a2]*den[a4],
		it will give
			den[a1] den[a2] (C1 den[a3]+ C1 den[a4)).
		It also ignores functions which depend on x. Example:
			na+nb x + x G[x]+ 2 na nb x + na^2x G[x]+x^2+x^2G[x]//Collect[#,x]&.
		
		However in most cases we need a result, where all of the terms are made out of two terms:
			1. the first should be free of the variable, be it a Symbol or Head;
			2. the second should be the unique dependent part.
			
		For this purpose I've written the GatherByDependency function, which has two separate helper function 
		SeparateDependency and Dependent.
		*)
			
		(*
		This function separates the dependent part of an expression. This only works on single expression!
		
		The first 4 rules are for the special cases, when Select cannot be used. Like when:
			1. the expression has only one mulitplicative term (e.g. a, a[1] etc);
			2. the expression is a fraction, with one term in the numerator and one in the denominator. 
		*)

ClearAll[SeparateDependency]

SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===0&&FreeQ[expr,var]
SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===0&&!FreeQ[expr,var]
SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===2&&Head[expr]===Power&&FreeQ[expr,var]
SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===2&&Head[expr]===Power&&!FreeQ[expr,var]
SeparateDependency[expr_,var_]:=expr//{#//Select[#,FreeQ[#,var]&]&,#//Select[#,!FreeQ[#,var]&]&}&

		(*
		This function takes the expression, checks whether is it free of the variable or not. If it is not, makes
		a list out of it. Then applies the SeparateDependency term by term. Can be used as a standalone function.
		*)
		
ClearAll[Dependent]

Dependent[expr_,var_]:=expr/;FreeQ[expr,var]
Dependent[expr_,var_]:=Block[
{
tmp
},

	tmp=expr;
	tmp=tmp//Expand[#,var]&;
	tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];
	tmp=SeparateDependency[#,var]&/@tmp

]

		(*
		This function takes any expression and gathers the terms with the same unique structure, 
		which depends on the given variable. The variable can be anything what is regonized by FreeQ.
		
		The first argument is the expression.
		The second argument is the variable.
		The optional third argument is a function, which is going to be applied on the independent part. 
		The optional fourth argument is a function, which is going to be applied on the dependent part. 
		The default is None for both.
		*)
		
		(*
		If the expression is free of the variable it gives back the expression. If not, then it
			1. expands it only in the varibale(!);
			2. separates the dependent and independent part of each additive term;
			3. gathers by the dependent part;
			4. adds together all of the independent parts for each structure.
			5. applies give function on the terms.
			
		The bootleneck might be Expand, because it is a really slow funtion in Wolfram Mathematica.
		*)
		
ClearAll[GatherByDependency]

GatherByDependency[expr_,var_]:=expr/;FreeQ[expr,var]
GatherByDependency[expr_,var_,__]:=expr/;FreeQ[expr,var]

GatherByDependency[expr_,var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=Block[
{
tmp=expr
},

	tmp=tmp//Expand[#,var]&;
	tmp=tmp//Dependent[#,var]&;
	tmp=tmp//GatherBy[#,Last]&;
	
	Switch[
		{
		ApplyFunctionOnIndependent,
		ApplyFunctionOnDependent
		},
		{None,None}, tmp=Flatten[{#[[All,1]]//Total,#[[1,2]]}]&/@tmp;,
		{_,None},    tmp=Flatten[{#[[All,1]]//Total//ApplyFunctionOnIndependent,#[[1,2]]}]&/@tmp;,
		{None,_},    tmp=Flatten[{#[[All,1]]//Total,#[[1,2]]//ApplyFunctionOnDependent}]&/@tmp;,
		{_,_},       tmp=Flatten[{#[[All,1]]//Total//ApplyFunctionOnIndependent,#[[1,2]]//ApplyFunctionOnDependent}]&/@tmp;
	];
	Plus@@Times@@@tmp

]


(* ::Input::Initialization:: *)
		(*
		When working with fractions Mathematica cannot immediately recognize simplifications. Take for example:

		na/(1-x)-(na+nb)/(1-x)

		Thus one has to apply a function, which helps Mathematica regonize the same additive terms.
	  This can be anything, like Expand, Factor, Together, Simplify, etc. 
	  Although these can be time consuming depending on the expression.

		GatherByDenominator gathers terms by their denominator and expands their numerator, thus canceling
		some of the terms; though it provides only a patrial cancelation.
		*)

ClearAll[GatherByDenominator];

GatherByDenominator[expr_Plus]:=
Block[
	{tmp=expr},
	
	(*tmp=List@@Expand[expr];*)
   tmp=List@@expr;
	tmp={Numerator[#],1/Denominator[#] }&/@tmp;
	tmp=GatherBy[tmp,Last];
	tmp={Expand[Plus@@#[[All,1]]],#[[1,2]]}&/@tmp;
	tmp=Plus@@Times@@@tmp
]

GatherByDenominator[expr_]:=expr


		(*
		This function is basically the implementation of Eq. 15 from the article. It does the polynomial division 
		in the case for fractions of the type x^m/((x-r)^n), when m and n are integers.
		*)

ClearAll[ReplaceRemainedStructure]

		(*Rule needed to be able to use Fold in the mathematicaPartialFraction function.*)
ReplaceRemainedStructure[var_][expr1_,expr2_]:=ReplaceRemainedStructure[var,expr1*expr2]

		(*
		The 0th stage is needed for the expand, otherwise we would need a complicated
		if to avoid an infinite iteration.
		*)
ReplaceRemainedStructure[var_,expr_]:=ReplaceRemainedStructure[var,expr,0]
ReplaceRemainedStructure[var_,expr_,0]:=ReplaceRemainedStructure[var,Expand[expr,var],1]
ReplaceRemainedStructure[var_,expr_Plus,1]:=ReplaceRemainedStructure[var,#,1]&/@expr

ReplaceRemainedStructure[
	var_,
	c_. var_^n_. (a_+var_)^m_/;Head[n]===Integer&&Head[m]===Integer&&(m<0)&&n>=m,
	1]:=
	Block[
	{
	},
	ReplaceRemainedStructure[var,
		Sum[
			c*
			(-1)^Mod[i-(-m-n),2]*
			Binomial[n,i-(-m-n)]*
			a^(i-(-m-n))/(a+var)^i,
			{i,1,-m}
		]+
		Sum[
			c*
			(-1)^(i+Mod[n+m,2])*
			Binomial[(n-1-i),
			-m-1]a^(n+m-i) var^i,
			{i,0,n+m}
		]
	]
	]

ReplaceRemainedStructure[var_,expr_,1]:=expr


(* ::Input::Initialization:: *)
		(*
		There can be cases, where some denominators have non-number, complex or rational power. We want to
		factor these out, since the powers in the formula are restricted to integers.

		Unfortunatelly, these powers can be added to a rational or integer power, which can further
		complicate things. The most problematic case is the complex powers. They are hard to separate and
		even the Denominator function does not recognize them; Mathematica treats them as numerators.
		*)

		(*
		This is a dummy function used instead of Power, thus the only rule if the second argument is 0,
		then it should return 1.
		*)
Clear[TmpPower]

TmpPower[expr_,0]:=1

		(*
		This function is used during the separating of the heads to pick out only the rational and
		integer powers.
		*)
Clear[GetIntOrRatPower]

GetIntOrRatPower[expr_]:=Block[
{
tmp,
dummy,
a,b,tmpI
},

tmp=dummy+expr/.Complex[a_,b_]->a+b tmpI;
tmp=Plus@@Cases[tmp, n_Rational|n_Integer+_.:>n,1]

]

		(*
		This is the main function, which separates the unwanted multiplicate variable-dependent terms.

	    First it gets rid of the unwanted powers like complex, rational or symbolic. Then factors out the
		appropriate power from the rationals and stores them in the dummy power function TmpPower.

	    Then, it picks out the terms linear in the variable and divide the original expression by it, to
		aquire the ignored part.
		*)
Clear[SeparateFrac]

SeparateFrac[expr_,var_]:=Block[
{
tmp,
keepFrac,
ignoreFrac,
LinApartOne,
a,b
},

	tmp=ReplaceAll[
			expr,
			Power[A_,n_]/;!FreeQ[A,var]:>
				A^Floor[GetIntOrRatPower[n]]TmpPower[A,n-Floor[GetIntOrRatPower[n]]]
			];

	keepFrac=Cases[
				LinApartOne[0]*tmp,
				A_^n_./;PolynomialQ[A,var]&&Exponent[A,var]===1,
				1];
	
	keepFrac=Times@@keepFrac;
	
	ignoreFrac=tmp/keepFrac/.LinApartOne[n_]->1/.TmpPower[a_,b_]->a^b;
	
	{ignoreFrac,keepFrac}
]


(* ::Subsection::Closed:: *)
(*LinApart function*)


												(*Front function*)
												
		(*
		This is basically just the front of the function, all it does it checks weather the input has
		no crucial mistakes like the number of argument, options and their values.
		*)

ClearAll[LinApart]

		(*
		Setting the default OptionValues.
		*)
Options[LinApart]=
	{"Factor"->False,
	 "GaussianIntegers"->False,
	 "PreCollect"->False, 
	 "ApplyAfterPreCollect"->None,
	 "Language"->"Mathematica"
	 };
	 
OptionFactorCases={True,False};
OptionGaussianIntegersCases={True,False};
OptionPreCollectCases={True,False};
OptionLanguageCases={"Mathematica","C"};

		(*Setting the properties*)
SetAttributes[LinApart,Listable]

		(*
		The first argument will be the expression, 
		the second the variable, 
		the other must be options.
		*)
	
		(*
		Since we are using the derivation function of Mathematica at the end the variable must be a symbol. 
		One could this easily circumvent by renaming the desired expression by a variable.
		*)
		
		(*
		In principle Mathematica can handel wrong Option input, but cannot handel wrong OptionValue input by itself.
		Thus we must check manually.
		*)
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[LinApartError::wrongOption,
								"Factor"
							];
	expr
	)/;!MemberQ[OptionFactorCases,OptionValue["Factor"]]

LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"GaussianIntegers"
	];
	expr
	)/;!MemberQ[OptionGaussianIntegersCases,OptionValue["GaussianIntegers"]]
	

LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"PreCollect"
	];
	expr
	)/;!MemberQ[OptionPreCollectCases,OptionValue["PreCollect"]]
	
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"ApplyAfterPreCollect"
	];
	expr
	)/;!OptionValue["PreCollect"]&&!OptionValue["ApplyAfterPreCollect"]===None
	
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"Language"
	];
	expr
	)/;!MemberQ[OptionLanguageCases,OptionValue["Language"]]
	
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[LinApartError::varNotSymbol,var];
	expr
	)/;Head[var]=!=Symbol
	
		(*If every option value is good proceed to Preprocessing.*)
LinApart[expr_, var_, options : OptionsPattern[]]:=PreProccesorLinApart[expr, var, options, 0]/;Head[var]===Symbol

		(*This is a new function might cause an error stating it reached the limit of recursion.*)
LinApart[arg___]:=Null/;CheckArguments[LinApart[arg],2]


(* ::Subsection::Closed:: *)
(*Preprocessor*)


												(*Main function*)
												
	(*
	The function is written in stages fot two main reasons:
		1. easier debugging and readability.
		2. this give more flexibility in coding.
		
	The stages are the following:
		0. stage:  
			-optional term-by-term factoring and or gatering by unique variable dependent structure.
			
		1. stage: 
			-if the expression is a sum applies itself on every term
			-handels some special cases, when decomposition is not needed
			-separates non-variable dependent multiplicative terms
			-handels special cases in the numerator, e.g. complex/real/non-number powers
			and non-linear terms
		
		2. stage: 
			-manipulate the denominator arguments (normalization, collecting in the variable)
			-handels special cases in the denominator, e.g. no root/complex/real/rational/non-number powers
			and non-linear terms
			-separates non-variable dependent multiplicative terms coming from the normalization
		
		(Obsolute but left here for future possible extensions.) 
		3. stage:
			-language choose
	*)
	
ClearAll[PreProccesorLinApart]

	(*
	Setting the default OptionValues; which are inherited from LinApart.
	*)
Options[PreProccesorLinApart]=Options[LinApart];


	(*Setting the properties*)
SetAttributes[PreProccesorLinApart,Listable]

	(*
	During the 0. stage:
		-the optional factorization and or gathering is done;
		-applies the next stage.
	*)
PreProccesorLinApart[expr_, var_, options : OptionsPattern[], 0]:=Block[
{
tmp=Expand[expr, var],tmp1,tmp2,
nums,denoms,quotients,reminders,
a,b,n,pow
},
	
	tmp=If[OptionValue["Factor"],
			If[Head[tmp]===Plus,
				Map[Times@@(#//SeparateDependency[#,var]&//MapAt[Factor[#,GaussianIntegers->OptionValue["GaussianIntegers"]]&,#,2]&)&,tmp],
				Times@@(tmp//SeparateDependency[#,var]&//MapAt[Factor[#,GaussianIntegers->OptionValue["GaussianIntegers"]]&,#,2]&)
				],
			tmp
		];

	tmp=If[OptionValue["PreCollect"],
			GatherByDependency[tmp,var,OptionValue["ApplyAfterPreCollect"],None],
			tmp
		];
	
	tmp=PreProccesorLinApart[tmp,var,options,1]
]
	
	(*
	During the 1. stage: 
		-applies itself on every term in a sum, if any of the terms
			o are indepedent of the variable
			o are ploynomials
			o has no denominator or the denominator is independent of the variable
		 it gives back the expression;
		-removes non-variable dependent parts
		-selects the multiplicative terms with complex/real/non-number powers
		-factors out the appropiate power from terms with rational powers
	*)
PreProccesorLinApart[expr_Plus,var_,options : OptionsPattern[],1]:=PreProccesorLinApart[#,var,options,1]&/@expr
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;FreeQ[expr,var]
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;PolynomialQ[expr,var]
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;(Denominator[expr]===1||FreeQ[Denominator[expr],var])&&FreeQ[expr,Power[_,Complex[a_/;a<0,_]]]
PreProccesorLinApart[expr_, var_, options : OptionsPattern[], 1]:=Block[
{
tmp,
coeff=1,
tmpNum,tmpDen,
keepForDivision=1,
keepNum,keepDen,keepFrac,
ignoreNum,ignoreDen,ignoreFrac,

a,b,tmpList1,tmpList2
},

	(*Separating variable dependent and independent parts.*)
	tmp=SeparateDependency[expr,var];
	coeff=coeff*tmp[[1]];
	tmp=tmp[[2]];
		
	(*Separating variable dependent part to ingnore.*)
	{ignoreFrac,keepFrac}=SeparateFrac[tmp,var];

		PreProccesorLinApart[coeff,ignoreFrac,keepFrac,var,options,2]
]

	(*
	During the 2. stage:
		-if the expression in non-factorizeable or just a polynomial it will show up in this step, first 2 rules;
		-if one a denominator has no root, it will give a dummy one;
		-removes every numerator, which is not a monomial in the variable;
		-does the normalization of the denominators;
		-removes constants coming from the renormalization;
		-it checks whether the numerator's order is greater than the denominator's order-1,
			o if no, nothing happens
			o if yes, it separates the expression into a proper fraction and a multiplicative term,
			with which we will deal in the mathematicaPartialFraction function with the help of the
			ReplaceRemainedStructure function.
	*)
PreProccesorLinApart[coeff_,ignoreFrac_,1, var_, options : OptionsPattern[], 2]:=((*Message[LinApartError::nonLinearExpression, coeff*ignoreFrac];*) coeff*ignoreFrac)
PreProccesorLinApart[coeff_,ignoreFrac_,keepFrac_, var_, options : OptionsPattern[], 2]:=coeff*ignoreFrac*keepFrac/;PolynomialQ[keepFrac,var]&&keepFrac=!=1
PreProccesorLinApart[coeff_,ignoreFrac_,keepFrac_,var_, options : OptionsPattern[], 2]:=Block[
{
tmp,
tmpCoeff=coeff,
tmpNum,tmpDen,
keepForDivision=1,
tmpNumExp,tmpDenExp,
tmpKeepFrac=keepFrac,

a,b,pow
},	
	
	(*If there is a denominator with root 0, it gaves a dummy root.*)
	tmpKeepFrac=keepFrac/.Power[var,pow_.]/;pow<0->Power[var-LinAparta[0],pow];
	
	(*
	It gets out the non-monomial numerators. These are a remanence from when we factored out the not needed powers.
	*)
	tmp=If[Head[tmpKeepFrac]=!=Times, {tmpKeepFrac}, List@@tmpKeepFrac];
	tmp=Times@@(tmp//Cases[
								#,
								Power[a_,b_]/;b>0&&Head[a]=!=Symbol->
								Power[a,b],
								1
								]&);
	keepForDivision=keepForDivision*tmp;
	tmpKeepFrac=tmpKeepFrac/tmp;
	
	(*Normalization.*)
	tmpKeepFrac=tmpKeepFrac/.Power[a_,pow_]/;pow<0:>Power[Collect[a,var],pow];
	tmpKeepFrac=tmpKeepFrac/.(a_.+b_. var)^n_./;(n<0):>b^n (a/b+var)^n;
	
	(*Separeting constants, which come from the normalization.*)
	tmp=SeparateDependency[tmpKeepFrac,var];
	tmpCoeff=tmpCoeff*tmp[[1]];
	tmpKeepFrac=tmp[[2]];
	
	tmpNum=Numerator[tmpKeepFrac];
	tmpDen=Denominator[tmpKeepFrac];
	
	tmpNumExp=Exponent[tmpNum,var];
	tmpDenExp=Exponent[tmpDen,var];
	
	If[tmpDenExp<=tmpNumExp,
		pow=tmpDenExp-tmpNumExp-1;
		PreProccesorLinApart[tmpCoeff,ignoreFrac,keepForDivision*var^-pow, var^pow tmpKeepFrac, var, options, 3],
		PreProccesorLinApart[tmpCoeff,ignoreFrac,keepForDivision, tmpKeepFrac, var, options, 3]
	]
]

PreProccesorLinApart[coeff_,ignoreFrac_,keepForDivision_,keepFrac_,var_,options : OptionsPattern[],3]:=coeff*ignoreFrac*keepForDivision*keepFrac/;Denominator[keepFrac]===1
PreProccesorLinApart[coeff_,ignoreFrac_,keepForDivision_,keepFrac_,var_,options : OptionsPattern[],3]:=
Block[
{
},

	mathematicaPartialFraction[coeff,ignoreFrac,keepForDivision,keepFrac,var]
	
]


(* ::Subsection::Closed:: *)
(*Partial fraction functions*)


(*Implementation of the partial fracction algorithm based on the residue method.*)

ClearAll[mathematicaPartialFraction]

mathematicaPartialFraction[coeff_,ignoreFrac_,keepForDivision_,keepFrac_,var_]:=Block[
{
tmp=keepFrac,
tmp1a,tmp1a1,tmp1a2,
tmp1b,tmp1b1,tmp1b2,
m,den,const,a,pow
},
	
	tmp1a=Numerator[tmp];
	tmp1b=1/Denominator[tmp];
	tmp1b=If[Head[tmp1b]===Power,{tmp1b},List@@tmp1b];
	
	
	tmp=GetExponent[#,var]&/@List@@tmp1b;
	den=tmp[[All,1]];
	const=den/.var->0;
	m=-tmp[[All,2]];
	
	tmp=Sum[
		Sum[
		
			(*
			The Expand here is a serious bottleneck if applied to the whole formula!
			Thus, I opted for Distribute, which do not gives us a fully expanded form but is something.
			*)
				If[FreeQ[keepForDivision,var],
					
					Distribute[	
						coeff*ignoreFrac*keepForDivision*
						(D[tmp1a*(Times@@Delete[tmp1b,i]),{var,m[[i]]-j}]/.var->-const[[i]])/(m[[i]]-j)!/(den[[i]]^j),
						Plus,Times
					],
					
					
					Distribute[	
						coeff*
						ignoreFrac*
						Expand[
							If[Head[keepForDivision]===Times,
								Fold[
									ReplaceRemainedStructure[var],1,
									Reverse[List@@(keepForDivision/(den[[i]]^j))]
								],
								ReplaceRemainedStructure[var,keepForDivision/(den[[i]]^j)]
							],
							var
						]*
						(D[tmp1a*(Times@@Delete[tmp1b,i]),{var,m[[i]]-j}]/.var->-const[[i]])/(m[[i]]-j)!,
						Plus,Times
					]
				],
				
		{j,1,m[[i]]}],
	{i,1,Length[tmp1b]}];	
	
	(*
	This might be a bootleneck if the expression is huge. Can be rewritten with set.
	*)
	tmp/.LinAparta[0]->0
]


(* ::Subsection::Closed:: *)
(*Text of messages*)


LinApart::usage = 
"LinApart[expression, variable_Symbol] 
LinApart[expression, variable_Symbol, Options] 
The function gives the partrial fraction decomposition of fractions with linear denominators in the choosen variable; the variable must be a symbol. 
Options: 
	-Factor->True/False: factor each additive term in the expression; the default value is False.
	-GaussianInteger->True/False: factorization of the input expression is performed over the Gaussian integer; the default value is False.
	-PreCollect->True/False: gather by every unique structure in the expression; the default value is False.
	-ApplyAfterPreCollect -> pure function (e.g. Factor): applies the given function on the variable independent part of each term; the default value is None.
";

LinApartError::varNotSymbol="The variable `1` is not a symbol.";
LinApartError::wrongOption="Problem with option `1`, OptionName or OptionValue not recognized.";
LinApartError::nonLinearExpression="The expression is non-linear `1`.";
