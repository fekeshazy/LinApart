(* ::Package:: *)

(* ::Section:: *)
(*FastApart*)


(*
This file contains the FastApart package, based on the articles arXiv.2405.20130 and ... .

It has four main sections.
	1. Helper functions: it contains all of smaller functions used in the main routine.
	2. LinApart front: it contains the definition fo the front function and input error handling.
	3. PreProcessor: it conaint the function called PreProcessor, which manipulates the input so 
	the formula (eq 11) presented in the article can be applied. 
	4. Text of messages: it contains the definition of the error/warning messages and the 
	short useage summary.
*)


(* ::Subsection::Closed:: *)
(*Welcome message*)


Print[

	"(****** LinApart 2.0 ******)"
	
];


(* ::Subsection::Closed:: *)
(*Helper functions for LinApart*)


(* ::Subsubsection::Closed:: *)
(*GetExponent*)


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

GetExponent[list_List,var_Symbol]:=GetExponent[#,var]&/@list
GetExponent[a_. expr_^n_.,var_]:={expr^n,1}/;FreeQ[expr,var]
GetExponent[a_. expr_^n_.,var_]:={expr,n}/;!FreeQ[expr,var]


(* ::Subsubsection::Closed:: *)
(*GatherByDependency*)


(*
        The Collect function is very useful, but when one collects by head like expr//Collect[#, _den]& it also factors
        the terms as well. Which means if we have an expression like:
                C1*den[a1]C2 den[a2]*den[a3]+C1*den[a1]*den[a2]*den[a4],
        it will give
                den[a1] den[a2] (C1 den[a3]+ C1 den[a4).
        It also ignores functions which depend on x. Example:
                na+nb x + x G[x]+ 2 na nb x + na^2x G[x]+x^2+x^2G[x]//Collect[#,x]&.

        However in most cases we need a result, where all of the terms are made out of two terms:
                1. the first should be free of the variable, be it a Symbol or Head;
                2. the second should be the unique dependent part.

        For this purpose I've written the GatherByDependency function, which has two separate helper function
        SeparateDependency and Dependent.
        *)

                (*
                This function separates the dependent part of an expression. This only works on single expression! Thus
                expression without any addition in the numberator and expression, which head is not Plus!

                The first 4 rules are for the special cases, when Select cannot be used. Like when:
                        1. the expression is only term;
                        2. the expression is a fraction, with one term in the numerator and one in the denominator.
                *)
ClearAll[SeparateDependency]

                  (*The expression is a special case and is free of the variable.*)
SeparateDependency[expr_,var_]:={expr,1}/;Head[expr]=!=Times&&FreeQ[expr,var]
SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===0&&FreeQ[expr,var]
SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===2&&Head[expr]===Power&&FreeQ[expr,var]

                  (*The expression is a special case and is not free of the variable.*)
SeparateDependency[expr_,var_]:={1,expr}/;Head[expr]=!=Times&&!FreeQ[expr,var]
SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===0&&!FreeQ[expr,var]
SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===2&&Head[expr]===Power&&!FreeQ[expr,var]

                  (*The expression is a multiplication.*)
SeparateDependency[expr_,var_]:=expr//{#//Select[#,FreeQ[#,var]&]&,#//Select[#,!FreeQ[#,var]&]&}&



                (*
                This function takes the expression, checks whether is it free of the variable or not. If it is not, makes
                a list out of it. Then applies the SeparateDependency term by term. Can be used as a standalone function.
                *)
ClearAll[Dependent]

	(*If the expression is free of the variable give back the expression.*)
Dependent[expr_,var_]:=expr/;FreeQ[expr,var]

	(*
		-expanding of expression,
		-making it a list, adgecase, when we only have a multiplication not a sum
		-separate the variable dependent terms.
	*)
Dependent[expr_,var_]:=Module[
	{
	tmp
	},

        tmp=expr;
        tmp=tmp//Expand[#,var]&;
        tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];
        tmp=SeparateDependency[#,var]&/@tmp

]


               (*
                This function takes any expression and gathers the terms with the same unique variable dependent structure.
                The variable can be anything, which FreeQ recognizes.

                  1. The first argument is the expression.
                  2. The second argument is the variable, which must have the head Symbol, And, Or, Alternatives or Pattern.
                  3. The optional third argument is a function, which is going to be applied on the independent terms. The default
                is None.
                  4. The optional forth argument is a function, which is going to be applied on the dependent terms. The default
                is None.
                *)

                (*
                If the expression is free of the variable it gives back the expression. If not, then it
                        1. expands;
                        2. separates the dependent and independent part of each additive and/or multiplicative term,
                        3. gathers by the dependent part,
                        4. applies the appropiate function(s) on the appropiate term(s),
                        5. adds together all of the independent parts for each structure.

                The bootleneck might be Expand, cuz' it is a really slow funtion in Wolfram Mathematica.
                *)


ClearAll[GatherByDependency]

GatherByDependency[expr_, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=
        If[ApplyFunctionOnIndependent===None,

                expr,
                expr//ApplyFunctionOnIndependent
        ]/;FreeQ[expr,var]

GatherByDependency[expr_Plus|expr_Times, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=Module[
{
tmp=expr,
tmpFreeOfVar,
tmpNotFreeOfVar
},

        tmp=tmp//Expand[#,var]&;

        If[tmp===0, Return[tmp,Module] ];

        If[Head[tmp]=!=Plus,
				
		        If[Length[tmp]===0,
			
				tmpFreeOfVar=If[FreeQ[tmp,var], tmp, 1];
				tmpNotFreeOfVar=If[!FreeQ[tmp,var], tmp, 1];,
				
		        tmpFreeOfVar=tmp//Select[#, FreeQ[#,var]& ]&;
		        tmpNotFreeOfVar=tmp//Select[#, !FreeQ[#,var]& ]&;
		        
		        ]
		

        Switch[
                        {
                        ApplyFunctionOnIndependent,
                        ApplyFunctionOnDependent
                        },
                        {None,None}, tmp=tmpFreeOfVar*tmpNotFreeOfVar;,
                        {_,None},    tmp=(tmpFreeOfVar//ApplyFunctionOnIndependent)*tmpNotFreeOfVar;,
                        {None,_},    tmp=tmpFreeOfVar*(tmpNotFreeOfVar//ApplyFunctionOnDependent);,
                        {_,_},       tmp=(tmpFreeOfVar//ApplyFunctionOnIndependent)*(tmpNotFreeOfVar//ApplyFunctionOnDependent);
                ];

        tmp,

        tmp=tmp//Dependent[#,var]&;

        If[Head[tmp]=!=List,

                tmp,


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

        ]

]

GatherByDependency[expr_, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=
        If[ApplyFunctionOnDependent===None,

                expr,
                expr//ApplyFunctionOnDependent
        ]


(* ::Subsubsection::Closed:: *)
(*GatherByDenominator*)


(* ::Input::Initialization:: *)
		(*
		When working with fractions Mathematica cannot immediately recognize simplifications. Take for example:

		na/(1-x)-(na+nb)/(1-x)

		Thus one has to apply a function, which helps Mathematica recognize the same additive terms.
	  This can be anything, like Expand, Factor, Together, Simplify, etc. 
	  Although these can be time consuming depending on the expression.

		GatherByDenominator gathers terms by their denominator and expands their numerator, thus canceling
		some of the terms; though it provides only a partial cancelation.
		*)

ClearAll[GatherByDenominator];

GatherByDenominator[expr_Plus]:=
Module[
	{tmp=expr},
	
	(*tmp=List@@Expand[expr];*)
   tmp=List@@expr;
	tmp={Numerator[#],1/Denominator[#] }&/@tmp;
	tmp=GatherBy[tmp,Last];
	tmp={Expand[Plus@@#[[All,1]]],#[[1,2]]}&/@tmp;
	tmp=Plus@@Times@@@tmp
]

GatherByDenominator[expr_]:=expr


(* ::Subsubsection::Closed:: *)
(*SeparateFrac*)


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

GetIntOrRatPower[expr_]:=Module[
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

SeparateFrac[expr_,var_]:=Module[
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
				A_^n_./;(PolynomialQ[A,var])&&FreeQ[n,var],
				1];
	
	keepFrac=Times@@keepFrac;
	
	ignoreFrac=tmp/keepFrac;
	
	{ignoreFrac,keepFrac}/.LinApartOne[0]->1/.TmpPower[a_,b_]->a^b
]


(* ::Subsubsection::Closed:: *)
(*NormalizeDenominators*)


ClearAll[NormalizeDenominators]

NormalizeDenominators[denominator_, var_]:=
	Module[
	{
	tmp1a,tmp1b,tmp,tmpNum,
	den,pows,const,prefac
	},
	
	tmp=denominator;
	tmp1a=Numerator[tmp];
	tmp1b=1/Denominator[tmp];
	tmp1b=If[Head[tmp1b]===Power,{tmp1b},List@@tmp1b];
	
	
	tmp=GetExponent[#,var]&/@List@@tmp1b;
	den=tmp[[All,1]];
	pows=tmp[[All,2]];
	
	const=den//CoefficientList[#,var]&;
	prefac=Times@@(const[[All,-1]]^pows);
	
	const=#/Last[#]&/@const;
	const={Length[#]-1,#}&/@const;
	
	{prefac,tmp1a Product[Sum[var^j const[[k,2,j+1]],{j,0,const[[k,1]]}]^pows[[k]] ,{k,Length[const]}]}
	
	]


(* ::Subsection::Closed:: *)
(*Helper functions for LinApart2*)


(* ::Subsubsection::Closed:: *)
(*ReportTime*)


(*
This is just a simple function to measure the time, I mainly use it for debugging.
*)

ClearAll[ReportTime]

ReportTime[label_String, lastTime_?NumericQ] :=
 Module[
	 {
	 currentTime, 
	 elapsed
	 },

	currentTime = AbsoluteTime[];
	elapsed = currentTime - lastTime;
	Print[label, ": ", NumberForm[elapsed, {5, 4}], " seconds"];
	  
	currentTime
]


(* ::Subsubsection::Closed:: *)
(*ComputeParallel *)


(* ::Input::Initialization:: *)
ClearAll[ComputeParallel]

ComputeParallel[expr_List, function_, numberOfCores_Integer, $PATHTMP_String]:=
Module[
     {
	results, tmp, tmpTiming,
        list=expr,step,
        
        lengthList=Length[expr],
        tmpFolderName,tmpJobNumber,
        submit,

		startTime,tmpTime
        },


				(*Writing the expression to file.*)

		(*Generate random folder name.*)
	tmpFolderName=$PATHTMP <> "tmp"<>ToString[RandomInteger[{1,10^10}]]<>"/";
    Print["Writing to file starts. Length of the list: "<> ToString[lengthList] <>"."];


		(*If the directory exists overwrite it.*)
        If[DirectoryQ[tmpFolderName],

                DeleteDirectory[tmpFolderName, DeleteContents -> True]

        ];
	CreateDirectory[tmpFolderName];

		(*Start of actual exporting.*)
	startTime=AbsoluteTime[];
        Do[

                Print["Writing to file " <> ToString[i] <> "/" <> ToString[lengthList] <> "."];
                tmp = list[[i]];
                DumpSave[tmpFolderName <> "tmp" <> ToString[i] <> ".mx", tmp];,

        {i, 1, lengthList}
        ];
   tmpTime=ReportTime["Exporting is done in" ,startTime];



					(*Distributing tasks to the subkernels*)

		(*Clear temporary variables and share job counting variable across kernels.*)
	Clear[tmp,tmpJobNumber];
	tmpJobNumber=lengthList;
	SetSharedVariable[tmpJobNumber];


				(*Distributing the task to the subkernels.*)
	ParallelDo[

                Print[ToString[$KernelID] <>": Calculation started."];

                Import[tmpFolderName<> "/tmp" <> ToString[i] <> ".mx"];
                {tmpTiming,tmp}=tmp//function//AbsoluteTiming;

                Print[
                        ToString[$KernelID] <> ": Calculation finished in: "<>ToString[ tmpTiming ]<>
                        "; remaining jobs: " <> ToString[tmpJobNumber--] <> "."
                ];

                DumpSave[tmpFolderName <> "/tmp" <> ToString[i] <> ".mx", tmp];,


		{i,1,lengthList},
		Method -> "FinestGrained",
		ProgressReporting->False
        ];

		(*Clear temporary variable just to be safe.*)
        Clear[tmp];
        startTime=AbsoluteTime[];
        
        (*Importing back the results with the main kernel.*)
       results=Table[

			 Import[tmpFolderName <> "tmp" <> ToString[i] <> ".mx"];
		Print[ToString[i] <> "/" <> ToString[lengthList]];
                        
                        tmp,

        {i, 1, lengthList}
        ];

	tmpTime=ReportTime["Importing is done in" ,startTime];

		(*Deleting temporary directory.*)
	DeleteDirectory[tmpFolderName, DeleteContents -> True];

	tmpTime=ReportTime["Deleting directory is done in" ,tmpTime];
	
	(*Return results.*)
	results

]


(* ::Subsubsection::Closed:: *)
(*Polynomial reduction modulo a polynomial*)


		(*

        A lot of functions are really slow if they take large expressions. We can somethimes circumvent this problem by
        doing the operation with symbolic coefficient and then substituting.

        *)

ClearAll[MakeCoefficientsSymbolic]

MakeCoefficientsSymbolic[
                expr_,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:= {dummyFunction[1], {dummyFunction[1]->expr}}/;FreeQ[expr,var]
                                                
MakeCoefficientsSymbolic[
                c_. var_Symbol^pow_.,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:= {dummyFunction[1] var^pow, {dummyFunction[1]->c}}
                                                
MakeCoefficientsSymbolic[
                expr_,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:=
        Module[
              	{
                tmp=expr,
                rules
                },

                tmp=tmp//GatherByDependency[#,var,dummyFunction]&;
                
                If[tmp//FreeQ[#,var]&, Return[{dummyFunction[1], {dummyFunction[1]->(tmp/.dummyFunction[a_]:>a)}}, Module] ];
                
                tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];

                rules=Table[tmp[[i]]/.c_. dummyFunction[a_]:>Rule[dummyFunction[i],a],{i,1,Length[tmp]}];
                tmp=Table[tmp[[i]]/.c_. dummyFunction[a_]:>c dummyFunction[i],{i,1,Length[tmp]}];

                {Plus@@tmp,rules}
        ]


(*
This is a weird helper function for the reduction. It gives the maximal order, which will have to be reduced during the reduciton.


	- if we encounter a monomial it gives back the order of the monomial,
	
	- if we have a polynomial in var on some power, it gives back the reduced polynomial's maximum order, which is the (highest order-1)
	and puts it on the appropiate order. This have two usescases:
		o one when we have something on a positive power,
		o one when we have something on some negative power, aka a denominator.
	In both cases the ReducePolynomialForResidue behaves the same, first it reduces the inner polynomial and only then expands and 
	reduces again.
	
	-if we have some multiplicative terms. Here we dismantle the expression and look at the numerator and denominator separately. 

*)



ClearAll[GetMaxOrderForReduction]
	
GetMaxOrderForReduction[Power[var_Symbol,power_Integer], polynomial_Plus, var_Symbol]:=
	Module[
		{
		},
		
			power
			
	]/;PolynomialQ[polynomial,var]
	
GetMaxOrderForReduction[Power[expr_,power_], polynomial_Plus, var_Symbol]:=
	Module[
		{
		maxOrderOfExpression,
		maxOrderOfPolyomial
		},
		
			maxOrderOfPolyomial=polynomial//Exponent[#,var]&;
			
			(maxOrderOfPolyomial-1)*Abs[power]
			
	]/;PolynomialQ[expr,var]&&PolynomialQ[polynomial,var]
	
GetMaxOrderForReduction[expr_Times, polynomial_Plus, var_Symbol]:=
	Module[
		{
		tmpNumerator,
		tmpDenominator,
		
		maxOrderOfNumerator,
		maxOrderOfDenominator,
		maxOrderOfPolyomial
		},
		
			tmpNumerator=expr//Numerator;
			tmpDenominator=expr//Denominator;
			maxOrderOfPolyomial=polynomial//Exponent[#,var]&;
			
			maxOrderOfNumerator=tmpNumerator//Exponent[#,var]&;
			maxOrderOfDenominator=(maxOrderOfPolyomial-1)*(tmpDenominator//Length);
			
			maxOrderOfNumerator+maxOrderOfDenominator
	]/;PolynomialQ[polynomial,var]
	
GetMaxOrderForReduction[expr_Plus, polynomial_Plus, var_Symbol]:=
	Module[
		{
		},
		
			expr//Exponent[#,var]&
			
			
	]/;PolynomialQ[expr,var]&&PolynomialQ[polynomial,var]


(*
This function generates the reduction rule. It takes two arguments:
	-the normalized(!) polynomial,
	-the variable.
*)

ClearAll[MakeReductionRule]

MakeReductionRule[polynomial_, var_Symbol]:=
	Module[
		{
		tmp,
		order,tmpRuleReduction,ruleReduction,
		ruleCoeffs,dummyF
		},
			
			(*Get the highest order of the polynomial*)
			order=Exponent[polynomial,var];
			
			(*"Solve" for the leading monomial.*)
			tmpRuleReduction=-polynomial+var^order;
			
			(*
			Making of the rule. Notes:
				-we need there Expand so the monomials merge and if needed we can apply the rule again.
				-since SetDelayed rules has a HoldAll attribuite at the right hand side, we must use the usual trick with With
				to insert exact values.
			*)
									
			ruleReduction=With[
				{
				i=tmpRuleReduction,
				j=order
				},
				
				coeff_. var^pow_. :> 
										(
										coeff var^Mod[pow,j]
										i^((pow-Mod[pow,j])/j)//
											Collect[#,var]&
										)
				
				]
		
		]/;PolynomialQ[polynomial,var]
		
MakeReductionRule[polynomial_, var_Symbol, maxOrder_Integer]:=
	Module[
		{
		tmp,
		
		order,tmpRuleReduction,
		
		redcutionorder,tableOfRules
		},
		
			(*Get the highest order of the polynomial*)
			order=Exponent[polynomial,var];
			
			(*"Solve" for the leading monomial.*)
			tmpRuleReduction=With[
								{
								leftHandSide=var^order, 
								rightHandSide=-polynomial+var^order
								},
								
									leftHandSide -> rightHandSide
									
								];
		
			tmp=var^(order-1);
			redcutionorder=maxOrder-order;
			tableOfRules=Table[
			
				tmp=tmp*var//Collect[#,var]&;
				tmp=tmp/.tmpRuleReduction//Collect[#,var,Together]&;
				{var^(i+order),tmp},
				
				{i,0,redcutionorder}			
			];
		
			Table[
			
					With[
						{
						leftHandSide=tableOfRules[[i,1]],
						rightHandSide=tableOfRules[[i,2]]
						},
					
						leftHandSide -> rightHandSide
					
					],
					
			{i,1,Length[tableOfRules]}			
			]//Dispatch
	]


ClearAll[ReducePolynomialForResidue]

	
ReducePolynomialForResidue[expr_List, polynomial_, var_Symbol]:=
	ReducePolynomialForResidue[#, polynomial, var]&/@expr
	

ReducePolynomialForResidue[expr_,polynomial_,var_Symbol]:=
	expr/;FreeQ[expr,var]
	
ReducePolynomialForResidue[expr_Plus, polynomial_, var_Symbol]:=
	ReducePolynomialForResidue[#, polynomial, var]&/@expr/;!PolynomialQ[expr,var]
	
	
ReducePolynomialForResidue[expr_, polynomial_, var_Symbol]:=
	expr/;PolynomialQ[expr,var]&&(Exponent[expr,var]<=Exponent[polynomial,var]-1)
	

ReducePolynomialForResidue[expr_Times, polynomial_, var_Symbol]:=
	Module[
		{
		tmp,dummyVar,
		maxOrder,
		ruleReduction,
		ruleCoeffsReductionRule,ruleCoeffsPolynomialReduction,
		tmpSybmolically,rulesCoeff
		},
			
			maxOrder=GetMaxOrderForReduction[expr,polynomial,var];
			ruleReduction=MakeReductionRule[polynomial,var,maxOrder];
			
			tmp=List@@expr;
			tmp=ReducePolynomialForResidue[#, polynomial, var]&/@tmp;

			tmp=Table[MakeCoefficientsSymbolic[tmp[[i]],var, Unique[dummyVar]], {i,1,Length[tmp]}];
			
			{tmpSybmolically,rulesCoeff}={tmp[[All,1]],tmp[[All,2]]//Flatten//Dispatch};
		
			tmp=Times@@tmpSybmolically;	
			tmp=tmp//Collect[#,var]&;
			
			tmp=tmp/.ruleReduction//Collect[#,var]&;
			
			tmp=tmp/.rulesCoeff;
			
			tmp
		]	
	
ReducePolynomialForResidue[expr_Plus|expr:Power[var_Symbol,power_Integer], polynomial_, var_Symbol]:=
	Module[
		{
		tmp,
		maxOrder,
		ruleReduction,
		ruleCoeffsReductionRule
		},
			
			maxOrder=GetMaxOrderForReduction[expr,polynomial,var];
			ruleReduction=MakeReductionRule[polynomial,var,maxOrder];
			tmp=expr/.ruleReduction;
			
			tmp
			
		
		]/;PolynomialQ[expr,var]&&((Exponent[expr,var]>=Exponent[polynomial,var])||power>=Exponent[polynomial,var])
		

	
ReducePolynomialForResidue[Power[expr_,-1], polynomial_, var_Symbol]:=
	PolynomialExtendedGCD[polynomial,expr,var][[2,2]]
		
ReducePolynomialForResidue[Power[expr_,power_Integer], polynomial_, var_Symbol]:=
	Module[
		{
		tmp,
		maxOrder,
		ruleReduction,
		ruleCoeffsReductionRule,ruleCoeffsPolynomialReduction
		},
			
			maxOrder=GetMaxOrderForReduction[expr^power,polynomial,var];
			ruleReduction=MakeReductionRule[polynomial,var,maxOrder];
						
			tmp=ReducePolynomialForResidue[#, polynomial, var]&/@expr;
			tmp=Collect[tmp,var]^power//Collect[#,var]&;
			tmp=tmp/.ruleReduction;
			
			tmp
			
		
		]/;PolynomialQ[expr,var]/;power>0
	
		
ReducePolynomialForResidue[Power[expr_,power_Integer], polynomial_, var_Symbol]:=
	Module[
		{
		tmp,
		maxOrder,
		ruleReduction,
		ruleCoeffsReductionRule,ruleCoeffsPolynomialReduction
		},
			
			maxOrder=GetMaxOrderForReduction[expr^power,polynomial,var];
			ruleReduction=MakeReductionRule[polynomial,var,maxOrder];
						
			tmp=PolynomialExtendedGCD[polynomial,expr,var][[2,2]];
			
			(*Major bootleneck!!*)
			tmp=Collect[tmp,var]^-power//Collect[#,var]&;
			tmp=tmp/.ruleReduction;
			
			
			tmp
		
		]/;PolynomialQ[expr,var]/;power<-1


(* ::Subsubsection::Closed:: *)
(*LinApartU*)


(*
Basically this is the S function from the article.
*)

ClearAll[LinApartU]
LinApartU[0, var_, polynomial_, {orderOfPolynomial_,listOfConstans_List}]:=orderOfPolynomial
LinApartU[n_., var_, polynomial_, {orderOfPolynomial_,listOfConstans_List}]:=(-1)^(-n+1)/(-n-1)! D[Log[polynomial],{var,-n}]/;n<0
LinApartU[n_., var_, polynomial_, {orderOfPolynomial_,listOfConstans_List}]:=
	Module[
		{
		tmp
		},
		
				tmp=Join[{orderOfPolynomial},NewtonsIdentity[n,listOfConstans]];
				Sum[Binomial[n,i] var^i (*(-1)^(n-i)*) tmp[[n-i+1]] ,{i,0,n}]
				
	]/;n>0


(* ::Subsubsection::Closed:: *)
(*NewtonsIdentity*)


(*
It is the implementation of Newton's Identites from 

	https://en.wikipedia.org/wiki/Newton%27s_identities .
	
The function is based on the last two equaiton of the section
Mathematical statement/Forumlation in terms of symmetric polynomials.

These indentites give the power sum ( p_k=Sum[x_i^k, {i,1,n}]) of the roots (x_i-s) of a polynomial 
in terms of the constants (b_i) of said polynomial.
*)
(*
Note: the constants are elementary symmetric polynomials of the roots.
*)

(*Clear function name.*)
ClearAll[NewtonsIdentity]

(*The function takes the maximum desired degree of the power sum and the constants.*)
NewtonsIdentity[maxDegree_Integer,constants_List]:=
	Module[
		{
		(*Temporary variable.*)
		powerSum,
		(*The length of the constants gives the order of the polynomial.*)
		orderOfPolynomial=Length[constants]
		},

		Table[
			
			(*Branching according to the formulas.*)
			If[orderOfPolynomial>=k,

			powerSum[k]=(-1)^(k-1) k constants[[k]]+
				Sum[(-1)^(k-1+i) constants[[k-i]] powerSum[i],{i,1,k-1}],

			powerSum[k]=
				Sum[(-1)^(k-1+i)constants[[k-i]] powerSum[i],{i,k-orderOfPolynomial,k-1}]
			],

			{k,1,maxDegree}
		]

	]/;Length[constants]>=1&&maxDegree>=1 (*Check for nonsensical input.*)
	
(*If any of the Heads are wrong, it should return Null and give an error message.*)
NewtonsIdentity[maxDegree_,constants_List]:=
	(
	Message[NewtonsIdentity::mayDegreeIsNotInteger,maxDegree];
	Null
	)/;Head[maxDegree]=!=Integer
	
NewtonsIdentity[maxDegree_,constants_]:=
	(
	Message[NewtonsIdentity::constantsIsNotAList,constants];
	Null
	)/;Head[constants]=!=List
	
(*If the maxDegree is less than 1 number, it should return Null and an error message.*)
NewtonsIdentity[maxDegree_Integer,constants_List]:=
	(
	Message[NewtonsIdentity::mayDegreeLessThan1];
	Null
	)/;maxDegree<=0
	
(*If the constants is an empty list, it should return Null and an error message.*)
NewtonsIdentity[maxDegree_,constants_List]:=
	(
	Message[NewtonsIdentity::constantsEmptyList];
	Null
	)/;Length[constants]==0

(*If there was no match for any rule, it should notify the user and return NUll.*)
NewtonsIdentity[maxDegree_,constants_]:=
	(
	Message[NewtonsIdentity::noRuleMatch,maxDegree,constants];
	Null
	)

(*
If the number of arguments is not three, it should notify the user and return NUll.
The CheckArguments has the error message so we don't have to define one.
*)
NewtonsIdentity[arg___]:=Null/;CheckArguments[NewtonsIdentity[arg],2]


(*Usage*)
NewtonsIdentity::usage=
"
NewtonsIdentity[maxDegree_Integer, constants_List]

This function gives the power sum ( p_k=Sum[x_i^k, {i,1,n}]) of the roots (x_i-s) of a polynomial in terms of the constants (b_i) of said polynomial.

The first argument is the order of the highest power sum, while the second is the list of constants. It returns all of the power sums up the the given maximum degree.
";

(*Error messages*)
NewtonsIdentity::mayDegreeIsNotInteger="The given degree (`1`) is not an Integer.";
NewtonsIdentity::constantsIsNotAList="The second argument (`1`) expects a list.";
NewtonsIdentity::mayDegreeLessThan1="The first argument must be greater or equal 1.";
NewtonsIdentity::constantsEmptyList="The second argument must not be an empty list.";
NewtonsIdentity::noRuleMatch="There were no rules matching the gives input. The first argument (`1`) should be the degree of the highest desired power sum, while the second (`2`) the list of constants.";


(* ::Subsubsection::Closed:: *)
(*DistributeAll*)


ClearAll[DistributeAll]

DistributeAll[expr_List]:=DistributeAll[#]&/@expr
DistributeAll[expr_Plus]:=DistributeAll[#]&/@expr
DistributeAll[expr_]:=expr//.Times[a_,b__]:>Distribute[Times[a,b],Plus,Times]
DistributeAll[expr_,var_]:=expr//.Times[a_,b_,c__]/;!FreeQ[a,var]&&!FreeQ[b,var]:>
										Times[Distribute[Times[a,b],Plus,Times],c]


(* ::Subsubsection::Closed:: *)
(*CheckNumericallyIfZero*)


ClearAll[CheckNumericallyIfZero]

CheckNumericallyIfZero[expr_]:=
	Module[
	{
	tmp,vars,ruleVars
	},
	
		vars=expr//Variables;
		ruleVars=Table[vars[[i]]->RandomPrime[10^6],{i,1,Length[vars]}]//Dispatch;
		
		tmp=expr/.ruleVars;
		
		If[tmp===0,
		
			0,
			expr
		]
	
	]


(* ::Subsubsection::Closed:: *)
(*ResidueForLaurentSeries*)


ClearAll[ResidueForLaurentSeries]

	(*
	Notes:
		-I had to put the condition inside the rule, cuz' somehow it did not work at the end.
		-I have to add everything up here otherwise I must use Flatten and that takes a lot of time, thus
		we lose the advantage of parallel computation.
	*)
ResidueForLaurentSeries[coeff_, list_List/;ArrayDepth[list]>1]:=(list//Map[(coeff*ResidueForLaurentSeries@@#)&,#]&)
ResidueForLaurentSeries[coeff_, list_List/;ArrayDepth[list]==1]:=coeff*ResidueForLaurentSeries@@list

ResidueForLaurentSeries[
	restOfFunction_, pole_,
	multiplicity_Integer,
	poleOrder_Integer,
	{1,constant_List}, var_
		]:=
	Module[
		{
		barePole=If[True, pole/.Power[expr_,power_Integer]:>expr,pole]
		},
		
			1/barePole^poleOrder (1/(multiplicity-poleOrder)!*
				D[restOfFunction,{var,multiplicity-poleOrder}]/.var->-constant[[1]])
				
	]
	
ResidueForLaurentSeries[
	restOfFunction_, pole_,
	multiplicity_Integer,
	poleOrder_Integer,
	{orderOfPolynomial_Integer,listOfConstans_List}, var_
		]:=
	Module[
		{
		tmp,tmpTiming,tmpRules,tmpR,tmpS,power,
		dens,simplifiedDens,ruleDens,
		order,tmpRuleReduction,ruleReduction,
		barePole=If[Head[pole]===Power, pole/.Power[expr_,power_Integer]:>expr,pole],
		
		tmpNum,tmpNumRules,tmpDen
		},
		
			
		(*
		Final Sum for the residue, we must go through every order starting form 1 to the pole's multiplicity.
		
			1. We derivate the R[x]/S[x] rational polynomial. R[x] is the rest of the function, S[x] is the denominator
			without the pole (x-a[i]).
			
			2. We simplfy the expression with the PolynomialGCD method.
			
			3. We express the remaning expressions in terms of the original coefficients of the 
			bare root.
			
		*)
					
			tmp=1/(multiplicity-poleOrder)! *
					D[tmpR[var]/tmpS[var]^multiplicity,{var,multiplicity-poleOrder}];
			
			tmp=tmp/.tmpR[var]:>restOfFunction;
			tmp=tmp/.D[tmpR[var],{var,power_}]:>D[restOfFunction,{var,power}];
			
			tmp=tmp/.tmpS[var]:>D[barePole,{var,1}];
			tmp=tmp/.D[tmpS[var],{var,power_}]:>D[barePole,{var,power+1}]/(power+1); (*S can be expressed as the derivative of the bare pole.*)
			
			
			tmp=tmp//Together;
			tmp=Collect[Numerator[tmp],var]/Denominator[tmp];
			
				(*Major bottleneck!!*)
			tmp=tmp//ReducePolynomialForResidue[#,barePole,var]&;
			tmp=tmp//CoefficientList[#,var]&;
			
			
			Sum[
					tmp[[powL+1]]
					Sum[
						Binomial[powL, i]*
							(
								var^i (-1)^(powL-i)*
								LinApartU[-poleOrder+powL-i, 
											var,
											barePole,
											{orderOfPolynomial, Reverse[listOfConstans]}
									]
							)//Apart[#,x]&
							
						,{i,0,powL}
					]
					
				,{powL,0,Length[tmp]-1}
			]
		
	]


(* ::Subsection::Closed:: *)
(*LinApart function*)


												(*Front function*)
												
		(*
		This is basically just the front of the function, all it does it checks wheather the input has
		no crucial mistakes like the number of arguments, options and their values.
		*)

ClearAll[LinApart]

		(*
		Setting the default OptionValues.
		*)
Options[LinApart]=
	{"Method"->"ExtendedLaurentSeries",
	
	 "Factor"->True,
	 "GaussianIntegers"->True,
	 "Extension"->{},
	 
	 "Parallel"->{False,None,None},
	 
	 "PreCollect"->False, 
	 "ApplyAfterPreCollect"->None,
	 "Language"->"Mathematica" (*Obsolute cuz' we removed the C version; it is now standalone.*)
	 };

OptionMethodCases={"ExtendedLaurentSeries","Euclidean","EquationSystem"};	 
	 
OptionFactorCases={True,False};
OptionGaussianIntegersCases={True,False};
OptionExtensionCases={True,False};

OptionParallelCases={True,False};

OptionPreCollectCases={True,False};
OptionLanguageCases={"Mathematica","C"}; (*Obsolute cuz' we removed the C version; it is now standalone.*)

		(*Setting the properties*)
SetAttributes[LinApart,Listable] (*This can lead to isses, if applied on SeriesData structures. MUST FIX!!

									I thought about this and I leave it here, cuz' this is
									Mathematica's fault and even the buit-in functions can behave weridly
									with SeriesData structures.
									*)
		(*
		The first argument will be the expression, 
		the second the variable, 
		the others must be options.
		*)
	
		(*
		Since we are using the derivation function of Mathematica at the end, the variable must be a symbol.
		*)
		
		(*
		In principle Mathematica can handel wrong Option input, but cannot handel wrong OptionValue input by itself.
		Thus we must check manually.
		*)
		
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[LinApartError::wrongOption,
								"Method"
							];
	expr
	)/;!MemberQ[OptionMethodCases,OptionValue["Method"]]

	
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
		LinApartError::factorIsFalse,
		"GaussianIntegers"
	];
	expr
	)/;(OptionValue["GaussianIntegers"]&&!OptionValue["Factor"])
	
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::factorIsFalse,
		"Extension"
	];
	expr
	)/;(OptionValue["Extension"]=!={}&&!OptionValue["Factor"])
			
LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"Extension"
	];
	expr
	)/;Head[OptionValue["Extension"]]=!=List||
			(OptionValue["Extension"]=!={}&&!And@@Map[NumberQ,OptionValue["Extension"]//N])



LinApart[expr_, var_, options : OptionsPattern[]]:=
	(
	Message[
		LinApartError::wrongOption,
		"Parallel"
	];
	expr
	)/;(!MemberQ[OptionParallelCases,OptionValue["Parallel"][[1]]] ||
			Head[OptionValue["Parallel"][[2]]]!=Integer ||
				Head[OptionValue["Parallel"][[3]]]!=String)

	
LinApart[expr_, var_, options : OptionsPattern[]]:=
	Module[
	{
	newOptions
	},
	
	
	Message[
		LinApartError::noParallelKernels
	];
	
	newOptions={options}/.\!\(\*
TagBox[
StyleBox[
RowBox[{
RowBox[{"Rule", "[", 
RowBox[{"\"\<Parallel\>\"", ",", " ", 
RowBox[{"{", 
RowBox[{"True", ",", "a__"}], "}"}]}], "]"}], " ", ":>", " ", 
RowBox[{"Rule", "[", 
RowBox[{"\"\<Parallel\>\"", ",", " ", 
RowBox[{"{", 
RowBox[{"False", ",", "a"}], "}"}]}], "]"}]}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
	LinApart[expr, var, ##]&@@newOptions
	
	]/;(OptionValue["Parallel"][[1]]&&$KernelCount===0)
	

LinApart[expr_, var_, options : OptionsPattern[]]:=
	Module[
	{
	newOptions
	},
	
	Message[
		LinApartError::ParallelComputationError
	];
	
	newOptions={options}/.\!\(\*
TagBox[
StyleBox[
RowBox[{
RowBox[{"Rule", "[", 
RowBox[{"\"\<Parallel\>\"", ",", " ", 
RowBox[{"{", 
RowBox[{"True", ",", "a__"}], "}"}]}], "]"}], " ", ":>", " ", 
RowBox[{"Rule", "[", 
RowBox[{"\"\<Parallel\>\"", ",", " ", 
RowBox[{"{", 
RowBox[{"False", ",", "a"}], "}"}]}], "]"}]}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
	LinApart[expr, var, ##]&@@newOptions
	
	]/;(OptionValue["Parallel"][[1]]&&OptionValue["Method"]=!="ExtendedLaurentSeries")


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
LinApart[expr_, var_, options : OptionsPattern[]]:=Apart[expr,var]/;OptionValue["Method"]==="EquationSystem"
LinApart[expr_, var_, options : OptionsPattern[]]:=PreProccesorLinApart[expr, var, options, 0]/;Head[var]===Symbol

		(*This is a new function might cause an error stating it reached the limit of recursion for lower versions.*)
LinApart[arg___]:=Null/;CheckArguments[LinApart[arg],2]


(* ::Subsection:: *)
(*Preprocessor*)


												(*Main function*)
												
	(*
	The function is written in stages for two main reasons:
		1. easier debugging and readability.
		2. this give more flexibility in coding.
		
	The stages are the following:
		0. stage:  
			-optional term-by-term factoring and or gatering by unique variable dependent structure.
			
		1. stage: 
			-if the expression is a sum, the function applies itself on every term
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
PreProccesorLinApart[expr_, var_, options : OptionsPattern[], 0]:=Module[
{
tmp=Expand[expr, var],tmp1,tmp2,
nums,denoms,quotients,reminders,
a,b,n,pow
},
	
	tmp=If[OptionValue["Factor"],
	
			If[Head[tmp]===Plus,
				Map[Times@@(#//SeparateDependency[#,var]&//MapAt[Factor[#,GaussianIntegers->OptionValue["GaussianIntegers"], Extension -> OptionValue["Extension"]]&,#,2]&)&,tmp],
				Times@@(tmp//SeparateDependency[#,var]&//MapAt[Factor[#,GaussianIntegers->OptionValue["GaussianIntegers"], Extension -> OptionValue["Extension"]]&,#,2]&)
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
		-applies itself on every term in a sum
		-if any of the terms
			o are indepedent of the variable
			o are ploynomials
			o has no denominator or the denominator is independent of the variable
			o there is only one denominator and the numerator is idependent of the variable
		 it gives back the expression;
		-removes non-variable dependent parts
		-selects the multiplicative terms with complex/real/non-number powers
		-factors out the appropiate power from terms with rational powers
	*)
PreProccesorLinApart[expr_Plus,var_,options : OptionsPattern[],1]:=PreProccesorLinApart[#,var,options,1]&/@expr
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;FreeQ[expr,var]
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;PolynomialQ[expr,var]
(*This line might be redundant, since PolynomialQ should take care of this case?*)
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;(Denominator[expr]===1||FreeQ[Denominator[expr],var])&&FreeQ[expr,Power[_,Complex[a_/;a<0,_]]]
PreProccesorLinApart[expr_,var_,options : OptionsPattern[],1]:=expr/;FreeQ[Numerator[expr],var]&&Head[Denominator[expr]]==Plus
PreProccesorLinApart[expr: num_. Power[poly_, pow_],var_,options : OptionsPattern[],1]:=expr/;FreeQ[num,var]&&IrreduciblePolynomialQ[poly]
PreProccesorLinApart[expr_, var_, options : OptionsPattern[], 1]:=Module[
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
		-if a denominator has no root, it will give a dummy one;
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
PreProccesorLinApart[coeff_,ignoreFrac_,keepFrac_,var_, options : OptionsPattern[], 2]:=Module[
{
tmp,
tmpCoeff=coeff,
tmpNum,tmpDen,
keepForDivision=1,
tmpNumExp,tmpDenExp,
tmpKeepFrac=keepFrac,
tmpNormalizationCoeff,tmpNormalizationFrac,

a,b,pow
},	
	
	(*If there is a denominator with root 0, it gaves a dummy root.*)
	tmpKeepFrac=keepFrac(*/.Power[var,pow_.]/;pow<0:>Power[var-LinAparta[0],pow]*);
	
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
	{tmpNormalizationCoeff, tmpKeepFrac} = NormalizeDenominators[tmpKeepFrac, var];
	tmpCoeff=tmpCoeff*tmpNormalizationCoeff;
	
	(*Separating constants, which come from the normalization.*)
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
Module[
{
},

	mathematicaPartialFraction[coeff,ignoreFrac,keepForDivision,keepFrac,var, options]
	
]


(* ::Subsection:: *)
(*Partial fraction function*)


(*Implementation of the partial fracction algorithm based on the residue method.*)

ClearAll[mathematicaPartialFraction]	

	(*
	Setting the default OptionValues; which are inherited from LinApart.
	*)
Options[mathematicaPartialFraction]=Options[LinApart];

mathematicaPartialFraction[coeff_,ignoreFrac_,keepForDivision_,keepFrac_,var_, options : OptionsPattern[]]:=
Module[
{
tmp=keepFrac,prefac,
tmp1a,tmp1a1,tmp1a2,
tmp1b,tmp1b1,tmp1b2,
m,den,const,a,pow,
tmpPolynomialPart,
tmpFractionedPart,

dummyIndex
},
	
	tmp1a=Numerator[tmp];
	tmp1b=1/Denominator[tmp];
	tmp1b=If[Head[tmp1b]===Power,{tmp1b},List@@tmp1b];
	
	
	tmp=GetExponent[#,var]&/@List@@tmp1b;
	den=tmp[[All,1]];
	
	const=den//CoefficientList[#,var]&;
	
	const=const[[All,;;-2]];
	const={Length[#],#}&/@const;
	
	m=-tmp[[All,2]];
	
	tmpPolynomialPart=
		If[FreeQ[keepForDivision,var],
				0,
				coeff*ignoreFrac*(Series[keepForDivision*keepFrac,{var,\[Infinity],0}]//Normal)
		];
		
	tmpFractionedPart=Table[
							Table[
							
											{
											keepForDivision*tmp1a*Times@@Delete[tmp1b,i],
											1/tmp1b[[i]],
											m[[i]],
											poleOrder,
											const[[i]],
											 var
											 },
											 
								{poleOrder, 1, m[[i]]}
								],
						{i,1,Length[tmp1b]}]//Flatten[#,1]&;
			(*
			I place coeff*ignoreFrac because if they are big, then they significantly slow down the sums
			inside ResidueForLaurentSeries. Of course if somebody wants the whole expression expanded then
			they can easily place it inside and spare some time.
			
			But either way this routine does not deliver an expanded coefficients for the residues.
			*)
	
	
		(*
		Important note: Plus has to unpack packed-arrays in order to add everything together, while Total can
		operate on packed-arrays.
		*)
	(If[OptionValue["Parallel"][[1]],
	
		tmpFractionedPart=tmpFractionedPart//RandomSample//Partition[#,UpTo[ Length[#]/OptionValue["Parallel"][[2]]//Ceiling ]]&;
		ComputeParallel[
						tmpFractionedPart,
						(ResidueForLaurentSeries[coeff*ignoreFrac,#])&,
						OptionValue["Parallel"][[2]],
						OptionValue["Parallel"][[3]]
					]//Flatten,
					
		Table[coeff*ignoreFrac*ResidueForLaurentSeries@@dummyIndex,{dummyIndex,tmpFractionedPart}]
	]//Total)+tmpPolynomialPart
	
]/;OptionValue["Method"]==="ExtendedLaurentSeries"

mathematicaPartialFraction[coeff_,ignoreFrac_,keepForDivision_,keepFrac_,var_, options : OptionsPattern[]]:=
	Module[
	{
  tmpPolynomialPart,

	tmp,tmpRules,
	tmp1,tmp2,tmp1Rules,tmp2Rules,
	dens,pairs,
	tmpGCDs,dummyGCDs,
	rulesGCD,dummyRulesGCD,pow1,pow2,tmpCoeff
	},

	
    tmpPolynomialPart=
		If[FreeQ[keepForDivision,var],
				0,
				coeff*ignoreFrac*(Series[keepForDivision*keepFrac,{var,\[Infinity],0}]//Normal)
		];


	tmp=Denominator[keepFrac];
	tmp=If[Head[tmp]===Power,{tmp},List@@tmp];
	
	
	tmp=GetExponent[#,var]&/@List@@tmp;
	dens=tmp[[All,1]];


	pairs=Subsets[dens,{2}];

         tmp=Table[ 

			tmp=PolynomialExtendedGCD[pairs[[i,1]],pairs[[i,2]],var][[2]];

			{tmp1,tmp1Rules}=tmp[[1]]//MakeCoefficientsSymbolic[#,var,Unique[dummyF]]&;
   		      {tmp2,tmp2Rules}=tmp[[2]]//MakeCoefficientsSymbolic[#,var,Unique[dummyF]]&;

				 {{pairs[[i,1]] tmp1, pairs[[i,2]]tmp2},Flatten[{tmp1Rules,tmp2Rules}]} ,

				{i,1,Length[pairs]}
			 ];
	{rulesGCD,tmpRules}={tmp[[All,1]],Flatten[tmp[[All,2]]](*//Dispatch*)};

	dummyRulesGCD=MapThread[

				With[
				{
				tmp11=#1[[1]],
				tmp12=#1[[2]],

				tmp21=#2[[1]],
				tmp22=#2[[2]]
				},

							RuleDelayed[
								tmpCoeff_. Times[tmp11^pow1_, tmp12^pow2_]/;(pow1<0&&pow2<0),
		
								tmpCoeff*tmp21*tmp11^pow1*tmp12^pow2+
									tmpCoeff*tmp22*tmp11^pow1*tmp12^pow2
	
					]

					]&,
					
					{pairs,rulesGCD}
			]//Dispatch;

	tmp=coeff keepForDivision ignoreFrac keepFrac//.dummyRulesGCD;

	(*
	-Eventhough, this gathering takes time and we lose efficiency but at least it does not quit the kernel, if I want to expand the result.
	-Furthermore, since during the reduction we get a lot of multiplication to see the cancelations we must either expand the coefficients
	of the different dependent structures or check them numerically with high random prime numbers. 
	(We have to do this eve if we do not do the reduction with symbolic coefficients.)
	*)

	tmp=tmp//GatherByDependency[#,var,None,Apart[#,var]&]&;
	tmp=tmp//GatherByDependency[#,var, None, If[PolynomialQ[#,var],0,#]&]&;

	tmp+tmpPolynomialPart/.tmpRules
	]/;OptionValue["Method"]==="Euclidean"


(* ::Subsection:: *)
(*Text of messages*)


LinApart::usage = 
"LinApart[expression, variable_Symbol] 
LinApart[expression, variable_Symbol, Options] 
The function gives the partrial fraction decomposition of fractions with linear denominators in the choosen variable; the variable must be a symbol. 
Options: 
	-Factor->True/False: factor each additive term in the expression; the default value is False.
	-GaussianIntegers->True/False: factorization of the input expression is performed over the Gaussian integer; the default value is False.
	-Extension->{a[1], a[2], ...}: option for Factor; factors a polynomial allowing coefficients that are rational combinations of the algebraic numbers a[i].
	-Parallel->{True/False, NumberOfCores, TemporaryPath}: calculate the residues on multiple cores during the extended Laurent-series method.
	-PreCollect->True/False: gather by every unique structure in the expression; the default value is False.
	-ApplyAfterPreCollect -> pure function (e.g. Factor): applies the given function on the variable independent part of each term; the default value is None.
";

LinApartError::noParallelKernels="There are no parallel kernels avaliable, procedding with sequential evaluation.";
LinApartError::ParallelComputationError="Parallel computation is not possible for this method, procedding with sequential evaluation.";
LinApartError::varNotSymbol="The variable `1` is not a symbol.";
LinApartError::wrongOption="Problem with option `1`, OptionName or OptionValue not recognized.";
LinApartError::nonLinearExpression="The expression is non-linear `1`.";
LinApartError::factorIsFalse="`1` is an option for Factor but Factor was set to False.";
