(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*GetExponent*)


(*
  GetExponent[expr, var]

  Extracts the base and exponent of the variable-dependent factor in a 
  monomial (single multiplicative term).

  Given a term of the form  a * f^n, where a is free of var:
    - If f depends on var : returns {f, n}
    - If f is free of var : returns {f^n, 1}
  
  The prefactor a (the part free of var in a product) is intentionally 
  discarded. This function is designed to be called on individual factors 
  obtained from factored denominators, where only the base and its 
  multiplicity matter.

  Also maps over lists: GetExponent[{t1, t2, ...}, var] applies 
  element-wise.

  Parameters:
    expr  -  A monomial expression (single multiplicative term, not a sum).
    var   -  The variable (must be a Symbol).

  Returns:
    {base, exponent}  -  A list of two elements.

  Examples:
    GetExponent[3 (1-x)^4, x]       ->  {1-x, 4}
    GetExponent[c y^2, x]           ->  {y^2, 1}
    GetExponent[x, x]               ->  {x, 1}
    GetExponent[(1+x)^(-2), x]      ->  {1+x, -2}
    GetExponent[{x^2, y^3}, x]      ->  {{x,2}, {y^3,1}}
*)
		
ClearAll[GetExponent]

GetExponent[list_List,var_Symbol]:=GetExponent[#,var]&/@list
GetExponent[a_. expr_^n_.,var_]:={expr^n,1}/;FreeQ[expr,var]
GetExponent[a_. expr_^n_.,var_]:={expr,n}/;!FreeQ[expr,var]


	(*Input validation*)

(*Check for number of arguments.*)
GetExponent[args___] := $Failed/;!CheckArguments[GetExponent[args], 2]

(* var must be a Symbol *)
GetExponent[expr_, var_] := (
    Message[GetExponent::argNotSymbol, var];
    $Failed
) /; !MatchQ[var, _Symbol]

(* expr must not be a sum *)
GetExponent[expr_Plus, var_Symbol] := (
    Message[GetExponent::invalidSum, expr];
    $Failed
)


	(*Main definitions*)

(* Map over lists *)
GetExponent[list_List, var_Symbol] := GetExponent[#, var]&/@list

(* Expression is free of the variable: return {expr_without_prefactor, 1} *)
GetExponent[a_. expr_^n_., var_Symbol] := {expr^n, 1} /; FreeQ[expr, var]

(* Expression depends on the variable: return {base, exponent} *)
GetExponent[a_. expr_^n_., var_Symbol] := {expr, n} /; !FreeQ[expr, var]



	(* Messages *)
	
GetExponent::argNotSymbol = "The second argument `1` is not a Symbol. GetExponent requires a Symbol as the variable.";
GetExponent::invalidSum = "GetExponent received a sum (Plus) as input: `1`. It is designed to operate on single multiplicative terms only.";


(* ::Subsection::Closed:: *)
(*GatherByDependency*)


	(*
	  SeparateDependency[expr, var]
	
	  Splits a single multiplicative term into {free part, dependent part} with 
	  respect to var. Returns {coeff, structure} where coeff*structure == expr 
	  (up to the ordering of factors).
	
	  This function only works on single terms (no sums). It handles special cases
	  (first 4 rules) where Select cannot be used directly:
	    - Atomic expressions (Length 0)
	    - Non-Times heads (single symbols, functions, etc.)
	    - Power expressions with exactly two arguments
	  For these, the entire expression goes into either the free or dependent slot.
	
	  For Times expressions, Select is used to partition factors.
	*)

ClearAll[SeparateDependency]

SeparateDependency[args___] := Null /; !CheckArguments[SeparateDependency[args], 2]

SeparateDependency[expr_Plus, var_] := (
    Message[SeparateDependency::invalidSum, expr];
    $Failed
)

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

SeparateDependency::invalidSum = "SeparateDependency received a sum (Plus) as input: `1`. It operates on single multiplicative terms only.";


	(*
	  Dependent[expr, var]
	
	  Expands the expression and maps SeparateDependency over each additive term.
	  Returns a list of {free, dependent} pairs, or the expression unchanged if 
	  it is free of var.
	
	  Handles the edge case where expansion yields a single product (not a sum)
	  by wrapping it in a list before mapping.
	*)
ClearAll[Dependent]

Dependent[args___] := Null /; !CheckArguments[Dependent[args], 2]

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
	  GatherByDependency[expr, var, ApplyFunctionOnIndependent, ApplyFunctionOnDependent]
	
	  Gathers additive terms in expr by their unique variable-dependent structure and 
	  separates each term into a variable-free coefficient and a variable-dependent part.
	
	  Motivation:
	    Collect is useful but has limitations. For example, collecting by head like
	        expr // Collect[#, _den]&
	    also factors the collected terms. Given
	        C1*den[a1]*C2*den[a2]*den[a3] + C1*den[a1]*den[a2]*den[a4],
	    Collect produces
	        den[a1]*den[a2]*(C1*C2*den[a3] + C1*den[a4]),
	    pulling out common factors rather than grouping by unique dependent structure.
	    It also ignores functions that depend on the variable, e.g.:
	        na + nb*x + x*G[x] + 2*na*nb*x + na^2*x*G[x] + x^2 + x^2*G[x] // Collect[#, x]&
	
	    GatherByDependency instead produces a sum where each term is a product of:
	      1. a coefficient free of var,
	      2. a unique var-dependent structure.
	
	  Parameters:
	    expr                       - The expression to be decomposed.
	    var                        - The variable; can be anything FreeQ recognizes 
	                                 (Symbol, pattern, Alternatives, etc.).
	    ApplyFunctionOnIndependent - (Optional, default None) Function applied to 
	                                 the variable-free coefficient of each term.
	    ApplyFunctionOnDependent   - (Optional, default None) Function applied to 
	                                 the variable-dependent part of each term.
	
	  Algorithm:
	    1. Expands the expression (bottleneck for large expressions).
	    2. Separates each additive term into {independent, dependent} parts.
	    3. Gathers terms by their dependent part using GatherBy.
	    4. Sums the independent coefficients for each unique dependent structure.
	    5. Applies the optional functions on the appropriate parts.
	
	  Helper functions:
	    SeparateDependency - Splits a single multiplicative term into {free, dependent}.
	    Dependent          - Expands and maps SeparateDependency over additive terms.
	*)


ClearAll[GatherByDependency]

GatherByDependency[args___] := Null /; !CheckArguments[GatherByDependency[args], {2, 4}]

(* If expression is free of var, return it directly or apply the independent function. *)
GatherByDependency[expr_, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=
        If[ApplyFunctionOnIndependent===None,

                expr,
                expr//ApplyFunctionOnIndependent
        ]/;FreeQ[expr,var]

(*
  Main definition: handles Plus (sums) and Times (single products).
  After expansion, there are two branches:
    - If the result is not a sum (single term after expansion), separate it
      directly into free and dependent parts using Select.
    - If the result is a sum, use Dependent to get {free, dependent} pairs
      for each additive term, then GatherBy the dependent part.
*)
GatherByDependency[expr_Plus|expr_Times, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=Module[
{
tmp=expr,
tmpFreeOfVar,
tmpNotFreeOfVar
},

		(* Expand with respect to var to expose additive structure. *)
        tmp=tmp//Expand[#,var]&;

		(* Expansion may cancel everything. *)
        If[tmp===0, Return[tmp,Module] ];

        If[Head[tmp]=!=Plus,
		(* 
          After expansion the result is a single term (not a sum).
          This happens when e.g. a Times input does not expand into Plus.
          Split it into var-free and var-dependent factors directly.
        *)
		        If[Length[tmp]===0,
			
				(*
		          Atomic expression (e.g. a single symbol): cannot use Select,
		          so assign the whole expression to one slot based on FreeQ.
		        *)
				tmpFreeOfVar=If[FreeQ[tmp,var], tmp, 1];
				tmpNotFreeOfVar=If[!FreeQ[tmp,var], tmp, 1];,
				
				(* Non-atomic single term: use Select to partition factors. *)
		        tmpFreeOfVar=tmp//Select[#, FreeQ[#,var]& ]&;
		        tmpNotFreeOfVar=tmp//Select[#, !FreeQ[#,var]& ]&;
		        
		        ];
		

		(* Apply the optional functions and recombine. *)
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

		(*
          The expanded result is a sum (Plus).
          Use Dependent to split each additive term into {free, dependent} pairs.
        *)
        tmp=tmp//Dependent[#,var]&;

        If[Head[tmp]=!=List,
		(* 
          Dependent returned the expression unchanged (e.g. fully free of var).
          Nothing to gather, return as is.
        *)
                tmp,

		(*
          tmp is now a list of {free, dependent} pairs.
          Group by the dependent part (second element), then sum the free 
          coefficients (first elements) within each group.
        *)
                tmp=tmp//GatherBy[#,Last]&;

				(* Apply optional functions and rebuild each {coeff, structure} pair. *)
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
                
                (* Reconstruct the expression: sum of (coeff * structure) for each group. *)
                Plus@@Times@@@tmp
        ]

        ]

]

(*
  Fallthrough: expr has a head other than Plus or Times (e.g. Power, Symbol, 
  a custom head). The expression itself is treated as fully dependent on var,
  so apply ApplyFunctionOnDependent if provided.
*)
        
GatherByDependency[expr_, var_, ApplyFunctionOnIndependent_ : None, ApplyFunctionOnDependent_ : None]:=
        If[ApplyFunctionOnDependent===None,

                expr,
                expr//ApplyFunctionOnDependent
        ]


(* ::Subsection::Closed:: *)
(*GatherByDenominator*)


(* ::Input::Initialization:: *)
	(*
	  GatherByDenominator[expr]
	
	  Gathers additive terms in expr by their denominator, sums and expands the 
	  numerators for each unique denominator, then reconstructs the expression.
	  This provides partial simplification without the overhead of Together or 
	  Simplify.
	
	  Motivation:
	    Mathematica does not automatically recognize simplifications like:
	        na/(1-x) - (na+nb)/(1-x)
	    which should simplify to -nb/(1-x). Functions like Together, Factor, or 
	    Simplify can achieve this but are slow for large expressions.
	
	    GatherByDenominator groups terms by denominator, sums the numerators, 
	    and applies Expand to enable cancellation. This is faster than full 
	    simplification while still catching common cancellations.
	
	  Parameters:
	    expr - An expression. If it is a sum (head Plus), terms are gathered 
	           by denominator. Otherwise, the expression is returned unchanged.
	
	  Returns:
	    The simplified expression with terms grouped by denominator.
	
	  Examples:
	    GatherByDenominator[na/(1-x) - (na+nb)/(1-x)]  ->  -nb/(1-x)
	    GatherByDenominator[a/x + b/x + c/y]           ->  (a+b)/x + c/y
	    GatherByDenominator[a + b]                     ->  a + b  (no denominators)
	    GatherByDenominator[x^2]                       ->  x^2    (not a sum)
	
	  Algorithm:
	    1. Convert the sum to a list of terms.
	    2. Extract {Numerator, 1/Denominator} for each term.
	    3. Group by the denominator part (1/Denominator).
	    4. Sum and Expand the numerators within each group.
	    5. Reconstruct as a sum of (numerator * 1/denominator) for each group.
	
	  Notes:
	    - Only applies to sums (head Plus). Non-sums pass through unchanged.
	    - Expand is applied only to the summed numerators, not the full expression.
	    - Terms with denominator 1 are grouped together as well.
	*)

ClearAll[GatherByDenominator]

GatherByDenominator[args___] := Null /; !CheckArguments[GatherByDenominator[args], 1]

GatherByDenominator[expr_Plus] := Module[
    {tmp = expr},

    (* Convert sum to list of additive terms. *)
    tmp = List @@ expr;

    (* Split each term into {numerator, 1/denominator}. 
       Using 1/Denominator ensures terms like a and a/1 group together. *)
    tmp = {Numerator[#], 1/Denominator[#]} & /@ tmp;

    (* Group terms by their denominator part. *)
    tmp = GatherBy[tmp, Last];

    (* For each group: sum numerators, Expand to enable cancellation, 
       keep the common denominator. *)
    tmp = {Expand[Plus @@ #[[All, 1]]], #[[1, 2]]} & /@ tmp;

    (* Reconstruct the expression: sum of (numerator * 1/denominator). *)
    Plus @@ Times @@@ tmp
]

(* Non-sum expressions pass through unchanged. *)
GatherByDenominator[expr_] := expr


(* ::Subsection::Closed:: *)
(*SeparateFrac*)


(*
  TmpPower[expr, n]

  Dummy function used as a placeholder for Power during the separation 
  process. Prevents Mathematica from recombining split powers.
  
  The only rule: TmpPower[expr, 0] = 1 (anything to the zero power is 1).
*)

ClearAll[TmpPower]

TmpPower[expr_, 0] := 1


(*
  GetIntOrRatPower[expr]

  Extracts the integer or rational part of an exponent, handling complex 
  numbers correctly.

  Mathematica represents complex numbers as Complex[a, b]. To extract just 
  the real rational/integer part, we:
    1. Add a dummy term to ensure Plus head.
    2. Replace Complex[a, b] with a + b*tmpI to expose the real part.
    3. Use Cases to pick out Integer or Rational terms.

  Examples:
    GetIntOrRatPower[3]         -> 3
    GetIntOrRatPower[3/2]       -> 3/2
    GetIntOrRatPower[2 + I]     -> 2
    GetIntOrRatPower[n]         -> 0  (symbolic, no integer/rational part)
    GetIntOrRatPower[3 + n]     -> 3
    GetIntOrRatPower[1/2 + I/3] -> 1/2
*)

ClearAll[GetIntOrRatPower]

GetIntOrRatPower[args___] := Null /; !CheckArguments[GetIntOrRatPower[args], 1]

GetIntOrRatPower[expr_] := Module[
    {tmp, dummy, a, b, tmpI},

    (* Add dummy to guarantee Plus head; replace Complex to expose real part. *)
    tmp = dummy + expr /. Complex[a_, b_] -> a + b tmpI;

    (* Sum all integer and rational terms at level 1. *)
    Plus @@ Cases[tmp, n_Rational | n_Integer + _. :> n, 1]
]


(*
  SeparateFrac[expr, var]
  SeparateFrac[expr, vars_List]

  Separates an expression into two parts:
    1. ignoreFrac: factors with non-integer powers (complex, rational, symbolic)
    2. keepFrac: factors with integer powers that are polynomial in var/vars

  This is needed because the partial fraction formula requires integer 
  multiplicities. Factors like (1-x)^(1/2), (1-x)^(2+I), or (1-x)^n (symbolic)
  cannot be processed by the main algorithm and must be factored out.

  Parameters:
    expr - A multiplicative expression (typically a denominator or product).
    var  - The variable of interest (Symbol), OR
    vars - List of variables of interest.

  Returns:
    {ignoreFrac, keepFrac} - A list of two expressions such that
                             ignoreFrac * keepFrac == expr.
    - keepFrac contains only factors A^n where A is polynomial in var/vars 
      and n is an integer free of var/vars.
    - ignoreFrac contains everything else.

  Examples:
    SeparateFrac[(1-x)^3, x]             -> {1, (1-x)^3}
    SeparateFrac[(1-x)^(3/2), x]         -> {(1-x)^(1/2), (1-x)^1}
    SeparateFrac[(1-x)^(2+I), x]         -> {(1-x)^I, (1-x)^2}
    SeparateFrac[(1-x)^n, x]             -> {(1-x)^n, 1}  (symbolic power)
    SeparateFrac[(1-x)^3 (1-y)^2, x]     -> {(1-y)^2, (1-x)^3}
    
    SeparateFrac[(1-x)^3 (1-y)^2, {x,y}]         -> {1, (1-x)^3 (1-y)^2}
    SeparateFrac[(1-x)^(1/2) (1-y)^2, {x,y}]     -> {(1-x)^(1/2), (1-y)^2}
    SeparateFrac[(1-x)^n (1-y)^2, {x,y}]         -> {(1-x)^n, (1-y)^2}

  Algorithm:
    1. For each Power[A, n] where A depends on var/vars, split n into its 
       integer part (Floor of rational/integer component) and remainder.
       Store the remainder in a dummy TmpPower to prevent recombination.
    2. Extract factors that are polynomial in var/vars with var/vars-free 
       integer exponents.
    3. Divide original (transformed) expression by keepFrac to get ignoreFrac.
    4. Substitute TmpPower back to Power in the final result.
*)


(*
  VarPattern[var] / VarPattern[vars]

  Returns an Alternatives pattern for use with FreeQ, etc.
  Single variable: just the variable
  List of variables: Alternatives[var1, var2, ...]
*)
ClearAll[VarPattern]
VarPattern[var_Symbol] := var
VarPattern[vars_List] := Alternatives @@ vars

(*
  IsPolynomialInVars[expr, var] / IsPolynomialInVars[expr, vars]

  Checks if expr is a polynomial in the given variable(s).
  For multivariate, must be polynomial in ALL variables.
*)
ClearAll[IsPolynomialInVars]
IsPolynomialInVars[expr_, var_Symbol] := PolynomialQ[expr, var]
IsPolynomialInVars[expr_, vars_List] := And @@ (PolynomialQ[expr, #] & /@ vars)


ClearAll[SeparateFrac]

SeparateFrac[args___] := Null /; !CheckArguments[SeparateFrac[args], 2]

(* Convert single variable to list and call list version *)
SeparateFrac[expr_, var_Symbol] := SeparateFrac[expr, {var}]

SeparateFrac[expr_, vars_] := (
    Message[SeparateFrac::notList, vars];
    $Failed
) /; !ListQ[vars]

SeparateFrac[expr_, vars_List] := (
    Message[SeparateFrac::emptyVars];
    $Failed
) /; Length[vars] === 0

SeparateFrac[expr_, vars_List] := Module[
    {tmp, keepFrac, ignoreFrac, LinApartOne, a, b, varPattern},

    (* Build pattern to match any of the variables *)
    varPattern = Alternatives @@ vars;

    (* 
      Split each var-dependent power A^n into A^Floor[int/rat part] * TmpPower[A, remainder].
      TmpPower prevents Mathematica from recombining the pieces.
    *)
    tmp = ReplaceAll[
        expr,
        Power[A_, n_] /; !FreeQ[A, varPattern] :> 
            A^Floor[GetIntOrRatPower[n]] TmpPower[A, n - Floor[GetIntOrRatPower[n]]]
    ];

    (*
      Extract the "good" factors: polynomial in vars with var-free integer exponent.
      LinApartOne[0] is a dummy factor to ensure Times head even for single terms.
      
      A factor is "good" if:
        - It is polynomial in ALL variables in vars
        - Its exponent is free of ALL variables in vars
    *)
    keepFrac = Cases[
        LinApartOne[0] * tmp,
        A_^n_. /; And @@ (PolynomialQ[A, #] & /@ vars) && FreeQ[n, varPattern],
        1
    ];
    keepFrac = Times @@ keepFrac;

    (* Everything else goes into ignoreFrac. *)
    ignoreFrac = tmp / keepFrac;

    (* Clean up: remove dummy LinApartOne, restore TmpPower to Power. *)
    {ignoreFrac, keepFrac} /. LinApartOne[0] -> 1 /. TmpPower[a_, b_] -> a^b
]

SeparateFrac::notList = "Second argument `1` must be a Symbol or List of Symbols.";
SeparateFrac::emptyVars = "Variable list must not be empty.";


(* ::Subsection::Closed:: *)
(*NormalizeDenominators*)


(*
  NormalizeDenominators[denominator, var]

  Normalizes a factored denominator so that each factor has leading 
  coefficient 1 (monic) with respect to var. Returns the extracted 
  prefactor and the normalized denominator.

  This is needed because the partial fraction formula assumes monic 
  denominators. For example, (2x + 4)^3 should become 2^3 * (x + 2)^3,
  with 8 extracted as a prefactor.

  Parameters:
    denominator - A product of polynomial factors raised to powers,
                  possibly with a numerator (e.g., from a fraction).
    var         - The variable with respect to which to normalize.

  Returns:
    {prefac, normalizedDenom} - A list of two expressions where:
      - prefac is the product of leading coefficients raised to their 
        respective powers.
      - normalizedDenom is the denominator with each factor made monic,
        multiplied by the original numerator.
    
    The relation: original denominator = prefac * normalizedDenom 
    (up to the numerator factor).

  Examples:
    NormalizeDenominators[(2x + 4)^3, x]       -> {8, (x + 2)^3}
    NormalizeDenominators[(3x - 6)^2, x]       -> {9, (x - 2)^2}
    NormalizeDenominators[(x + 1)^2 (2x)^3, x] -> {8, (x + 1)^2 * x^3}
    NormalizeDenominators[a/(2x + 2), x]       -> {2, a/(x + 1)}

  Algorithm:
    1. Separate numerator and denominator of the input.
    2. Convert denominator factors to a list; extract base and exponent 
       of each using GetExponent.
    3. For each factor, get CoefficientList and extract the leading 
       coefficient (last element).
    4. Compute prefac as the product of (leading coeff)^power for all factors.
    5. Divide each coefficient list by its leading coefficient to make 
       the factor monic.
    6. Reconstruct the normalized denominator using Sum and Product.
    7. Return {prefac, numerator * normalized denominator product}.
*)

ClearAll[NormalizeDenominators]

NormalizeDenominators[args___] := Null /; !CheckArguments[NormalizeDenominators[args], 2]

NormalizeDenominators[denominator_, var_] := Module[
    {tmp1a, tmp1b, tmp, den, pows, const, prefac},

    tmp = denominator;

    (* Separate numerator and denominator. *)
    tmp1a = Numerator[tmp];
    tmp1b = 1/Denominator[tmp];

    (* Convert denominator to list of factors. 
       Handle single Power vs product of Powers. *)
    tmp1b = If[Head[tmp1b] === Power, {tmp1b}, List @@ tmp1b];

    (* Extract {base, exponent} for each factor. *)
    tmp = GetExponent[#, var] & /@ tmp1b;
    den = tmp[[All, 1]];
    pows = tmp[[All, 2]];

    (* Get coefficient lists for each base polynomial. *)
    const = CoefficientList[#, var] & /@ den;

    (* Compute prefactor: product of (leading coefficient)^power. *)
    prefac = Times @@ (const[[All, -1]]^pows);

    (* Normalize each coefficient list by dividing by leading coefficient. *)
    const = # / Last[#] & /@ const;

    (* Pair each coefficient list with its degree for reconstruction. *)
    const = {Length[#] - 1, #} & /@ const;

    (* Reconstruct: numerator * product of monic factors raised to their powers.
       Sum rebuilds polynomial from coefficients, Product combines all factors. *)
    {prefac, tmp1a Product[
        Sum[var^j const[[k, 2, j + 1]], {j, 0, const[[k, 1]]}]^pows[[k]],
        {k, Length[const]}
    ]}
]
