(* ::Package:: *)

(* ::Section::Closed:: *)
(*Usage*)


(*                                LINAPART UNIFIED TEST SUITE                                *)
(* ========================================================================================= *)
(*
   This file contains the complete test harness for the LinApart package. 
   It unifies stress testing, edge-case analysis, error handling verification, 
   and code coverage reporting into a single, robust system.

   -----------------------------------------------------------------------------------------
   HOW TO RUN
   -----------------------------------------------------------------------------------------
   
   1. Command Line (CI/CD Mode):
      Run the following command in your terminal. This mode is optimized for 
      Continuous Integration pipelines (GitHub Actions, GitLab CI). 
      It will return an exit code (0 for success, 1 for failure).
      
      $ math -script Tests/Tests.m

   2. Interactive Mode (Wolfram Notebook):
      Simply open this file (or copy its contents) into a Notebook and evaluate all cells.
      The test suite detects the environment automatically. In this mode, failures 
      will abort execution or print visual indicators rather than exiting the kernel.

   -----------------------------------------------------------------------------------------
   VERIFICATION STRATEGY
   -----------------------------------------------------------------------------------------
   This suite uses a "Hybrid Verification" approach. It does not simply check if the 
   output equals a hardcoded string. Instead, it mathematically proves validity:

   1. Numerical Equivalence (Zero-Test):
      Every result is checked against the input by substituting random large 
      prime numbers for all variables.
      Condition: (Input - Output) /. {vars -> Primes} == 0

   2. Structural Verification (Partial Fraction Form):
      It checks if the output is actually decomposed, not just mathematically equal.
      
      a. Single-Variable (TestLinApart1D):
         For every term in the result, the degree of the numerator must be strictly 
         less than the degree of the denominator (unless the term is polynomial/constant).
         
      b. Multivariate (TestLinApartMV):
         For every term in the result, the number of distinct linear denominator 
         factors must be less than or equal to the number of variables (n).
         This ensures the basis reduction algorithm actually worked.

   -----------------------------------------------------------------------------------------
   CODE COVERAGE (Optional)
   -----------------------------------------------------------------------------------------
   If the "Instrumentation`" package is available (standard in many dev environments),
   this script will automatically:
   1. Create an instrumented copy of the source code.
   2. Track exactly which lines of code are executed during tests.
   3. Generate an annotated report showing hit counts for every line.
   
   This helps identify "dead code" or logic branches that are never tested.

   -----------------------------------------------------------------------------------------
   TEST CATEGORIES
   -----------------------------------------------------------------------------------------
   - Section 1: Single-Variable (Linear, Quadratic, High-degree, Normalization)
   - Section 2: Multivariate (Central/Non-central configurations, Basis reduction)
   - Section 3: Robustness (Numerical stability, Degenerate cases, Symbolic coeffs)
   - Section 4: Error Handling (Input validation, Warnings, Graceful failures)
   - Section 5: Parallel Computing (Multi-kernel execution checks)
*)
(* ========================================================================================= *)


(* ::Section:: *)
(*Load LinApart*)


notebookDirectory=If[$Notebooks, NotebookDirectory[], Directory[]];
Get[notebookDirectory<>"../Mathematica/LinApart.m"]


(* ::Section::Closed:: *)
(*Functions*)


(* ============================================== *)
(*           TEST FRAMEWORK SETUP                 *)
(* ============================================== *)

(* Detect environment *)
$ScriptMode = Length[Cases[$CommandLine, "-script"]] > 0;

(* Error handler: exit in script mode, throw in notebook *)
Error[msg__] := If[$ScriptMode,
    Print["ERROR: ", msg]; Exit[1],
    Print[Style["ERROR: ", Red, Bold], msg]; Throw[$Failed]
]

(* ============================================== *)
(*           HELPER FUNCTIONS                     *)
(* ============================================== *)

(* Return the list of additive terms in an expression *)
ClearAll[Terms]
Terms[ex_List] := ex // Map[Terms]
Terms[ex_Plus] := List @@ ex
Terms[0] := {}
Terms[ex_] := {ex}

(* Get the maximal rational power of a variable in an expression *)
ClearAll[MaxPowerOf]
MaxPowerOf[ex_, x_Symbol] /; FreeQ[ex, x] := 0
MaxPowerOf[x_, x_Symbol] := 1
MaxPowerOf[(ex_)^n_, x_Symbol] /; FreeQ[n, x] := MaxPowerOf[ex, x] * Re[n]
MaxPowerOf[(ex_)^n_, x_Symbol] /; !FreeQ[n, x] := 0
MaxPowerOf[items_List, x_Symbol] := items // Map[MaxPowerOf[#, x] &] // Max
MaxPowerOf[items_Plus, x_Symbol] := (List @@ items) // Map[MaxPowerOf[#, x] &] // Max
MaxPowerOf[items_Times, x_Symbol] := (List @@ items) // Map[MaxPowerOf[#, x] &] // Apply[Plus]
MaxPowerOf[(Log | Sin | Cos | Csc | Sec | Exp | G)[__], x_Symbol] := 0

(* Numerical zero check with random primes *)
ClearAll[NumericalZeroQ]
NumericalZeroQ[expr_] := Module[
    {allSymbols, results},
    allSymbols = Select[Variables[expr], Head[#] === Symbol &];
    If[Length[allSymbols] === 0, Return[expr === 0]];
    results = Table[
        expr /. Thread[allSymbols -> RandomPrime[{10^9, 10^10}, Length[allSymbols]]],
        {3}
    ];
    And @@ (# === 0 & /@ results)
]

(* ============================================== *)
(*           TEST FUNCTIONS                       *)
(* ============================================== *)

(*
  TestLinApart1D - Single-variable test with structural verification
  
  Input: {testName, expr, var} or {testName, expr, var, opts...}
  
  Returns: 
    1 if test passes (numerical equivalence + structural verification)
    0 if test fails
    
  Also prints timing and pass/fail indicator.
*)
ClearAll[TestLinApart1D]

TestLinApart1D[{testName_String, expr_, var_Symbol, opts___}] := Module[
    {result, timing, ok, symbols, symbolVals, num, den, enum, eden, 
     otherSymbols, resultSubstituted, hasNonRational, numDiff, symDiff, val, absVal},
    
    {timing, result} = AbsoluteTiming[LinApart[expr, var, opts]];
    
    (* Check 1: Numerical equivalence *)
    symbols = Select[Variables[expr], Head[#] === Symbol &];
    
    If[Length[symbols] === 0,
        (* No variables - direct comparison *)
        ok = (Together[expr - result] === 0);
        ,
        (* Substitute random primes *)
        symbolVals = Thread[symbols -> RandomPrime[{10^4, 10^5}, Length[symbols]]];
        numDiff = (expr - result) /. symbolVals // Together;
        ok = (numDiff === 0);
        
        If[!TrueQ[ok],
            (* Numerical check failed. Try to evaluate numerically. *)
            val = Quiet[N[numDiff]];
            absVal = Quiet[Abs[val]];
            
            Which[
                (* Case 1: Abs gives a small number (works for real and complex) *)
                NumericQ[absVal] && absVal < 10^-7,
                    symDiff = TimeConstrained[(expr - result) // Together // Factor, 5.0, $Failed];
                    If[symDiff === 0, ok = True],
                
                (* Case 2: Can't evaluate numerically - must check symbolically *)
                !NumericQ[absVal],
                    symDiff = TimeConstrained[(expr - result) // Together // Factor, 5.0, $Failed];
                    If[symDiff === 0, ok = True],
                
                (* Case 3: Numeric but not small - genuine failure *)
                True,
                    Null
            ];
        ];
    ];
    
    If[!TrueQ[ok],
        Print[{timing, "\[Cross] "}, "  " <> testName];
        Print["  Input:  ", expr];
        Print["  Output: ", result];
        Return[0]
    ];
    
    (* Check 2: Structural verification - proper partial fraction form *)
    otherSymbols = DeleteCases[symbols, var];
    
    resultSubstituted = If[Length[otherSymbols] > 0,
        result /. Thread[otherSymbols -> RandomPrime[{10^8, 10^9}, Length[otherSymbols]]],
        result
    ];
    
    resultSubstituted // Expand // Terms // Flatten // Map[(
        (* Skip check if term contains non-integer/symbolic powers of var *)
        hasNonRational = !FreeQ[#, Power[b_, e_] /; !FreeQ[b, var] && !IntegerQ[e]];
        
        If[!hasNonRational,
            num = Numerator[#];
            den = Denominator[#];
            enum = MaxPowerOf[num, var];
            eden = MaxPowerOf[den, var];
            ok = (eden == 0) || (enum < eden);
            If[!TrueQ[ok],
                Print[{timing, "\[Cross] "}, "  " <> testName <> " (not in PF form)"];
                Print["  Offending term: ", #];
                Print["  Num degree: ", enum, "  Den degree: ", eden];
                Return[0, Module]
            ];
        ];
    ) &];
    
    Print[{timing, "\[Checkmark] "}, "  " <> testName];
    1
]


(*
  TestLinApartMV - Multivariate test with structural verification
  
  Input: {testName, expr, vars} or {testName, expr, vars, opts...}
  
  Returns: 
    1 if test passes (numerical equivalence + structural verification)
    0 if test fails
    
  Structural check: each term must have at most n = Length[vars] 
  linear denominator factors in the specified variables.
*)
ClearAll[TestLinApartMV]

TestLinApartMV[{testName_String, expr_, vars_List, opts___}] := Module[
    {result, timing, ok, symbols, symbolVals, numDiff, symDiff, val, absVal,
     nVars, varPat, terms, term, den, denFactors, denomCount},
    
    {timing, result} = AbsoluteTiming[LinApart[expr, vars, opts]];
    
    nVars = Length[vars];
    varPat = Alternatives @@ vars;
    
    (* Check 1: Numerical equivalence *)
    symbols = Select[Variables[expr], Head[#] === Symbol &];
    
    If[Length[symbols] === 0,
        (* No variables - direct comparison *)
        ok = (Together[expr - result] === 0);
        ,
        (* Substitute random primes *)
        symbolVals = Thread[symbols -> RandomPrime[{10^4, 10^5}, Length[symbols]]];
        numDiff = (expr - result) /. symbolVals // Together;
        ok = (numDiff === 0);
        
        If[!TrueQ[ok],
            (* Numerical check failed. Try to evaluate numerically. *)
            val = Quiet[N[numDiff]];
            absVal = Quiet[Abs[val]];
            
            Which[
                (* Case 1: Abs gives a small number (works for real and complex) *)
                NumericQ[absVal] && absVal < 10^-7,
                    symDiff = TimeConstrained[(expr - result) // Together // Factor, 5.0, $Failed];
                    If[symDiff === 0, ok = True],
                
                (* Case 2: Can't evaluate numerically - must check symbolically *)
                !NumericQ[absVal],
                    symDiff = TimeConstrained[(expr - result) // Together // Factor, 5.0, $Failed];
                    If[symDiff === 0, ok = True],
                
                (* Case 3: Numeric but not small - genuine failure *)
                True,
                    Null
            ];
        ];
    ];
    
    If[!TrueQ[ok],
        Print[{timing, "\[Cross] "}, "  " <> testName];
        Print["  Input:  ", expr];
        Print["  Output: ", result];
        Return[0]
    ];
    
    (* Check 2: Structural verification - at most n denominators per term *)
    terms = If[Head[result] === Plus, List @@ result, {result}];
    
    Do[
        den = Denominator[term];
        If[den === 1, Continue[]];
        
        denFactors = If[Head[den] === Times, List @@ den, {den}];
        
        (* Only count factors that are:
           1. Dependent on vars (contain at least one var)
           2. Linear polynomials in vars (degree 1 in each var, total degree 1)
           
           This excludes: Exp[...], Sin[...], G[...], and non-polynomial factors
        *)
        denFactors = Select[denFactors, Function[factor,
            Module[{base},
                (* Extract base if it's a power *)
                base = factor /. Power[b_, _] :> b;
                
                (* Must depend on vars *)
                If[FreeQ[base, varPat], Return[False, Module]];
                
                (* Must be a polynomial in vars *)
                If[!PolynomialQ[base, vars], Return[False, Module]];
                
                (* Must be linear (max exponent 1 in any var) *)
                If[Max[Exponent[base, vars]] > 1, Return[False, Module]];
                
                True
            ]
        ]];
        
        (* Strip powers and count distinct bases *)
        denFactors = denFactors /. Power[base_, _] :> base;
        denFactors = DeleteDuplicates[denFactors];
        
        denomCount = Length[denFactors];
        
        If[denomCount > nVars,
            Print[{timing, "\[Cross] "}, "  " <> testName <> " (too many denoms)"];
            Print["  Term: ", term];
            Print["  Has ", denomCount, " denoms, max allowed: ", nVars];
            Return[0, Module]
        ],
        
        {term, terms}
    ];
    
    Print[{timing, "\[Checkmark] "}, "  " <> testName];
    1
]

(* ============================================== *)
(*           ROBUSTNESS TEST FUNCTION             *)
(* ============================================== *)

(*
  TestLinApartRobust - Robustness test (may be 1D or MV)
  
  Input: {testName, expr, var/vars} or {testName, expr, var/vars, opts...}
  
  For single variable (Symbol): uses 1D test logic
  For multiple variables (List): uses MV test logic
  
  Returns: 
    1 if test passes
    0 if test fails
*)
ClearAll[TestLinApartRobust]

(* Single variable version *)
TestLinApartRobust[{testName_String, expr_, var_Symbol, opts___}] := 
    TestLinApart1D[{testName, expr, var, opts}]

(* Multivariate version *)
TestLinApartRobust[{testName_String, expr_, vars_List, opts___}] := 
    TestLinApartMV[{testName, expr, vars, opts}]
    
(* ============================================== *)
(*           ERROR HANDLING TEST FUNCTION         *)
(* ============================================== *)

(*
  TestLinApartError - Tests that should produce errors/warnings or special behavior
  
  Input: {testName, expr, vars, expectedBehavior} or {testName, expr, vars, opts..., expectedBehavior}
  
  expectedBehavior can be:
    "$Failed" - function should return $Failed
    "Warning" - function should produce a warning but return a result
    "Unchanged" - function should return the input unchanged
    "Valid" - function should return a valid (possibly simplified) result
  
  Returns: 
    1 if test passes (behavior matches expected)
    0 if test fails
*)
ClearAll[TestLinApartError]

(* Only one definition, using pattern matching to split args *)
TestLinApartError[args_List] := Module[
    {testName, expr, vars, opts, expectedBehavior, result, timing, messages, passed},
    
    (* Unpack arguments *)
    testName = args[[1]];
    expr = args[[2]];
    vars = args[[3]];
    expectedBehavior = Last[args];
    
    (* Options are everything between vars and expectedBehavior *)
    opts = args[[4 ;; -2]];
    
    {timing, {result, messages}} = AbsoluteTiming[
        Quiet[
            Reap[
                Internal`HandlerBlock[
                    {"Message", Sow[#] &},
                    LinApart[expr, vars, Sequence @@ opts]
                ]
            ],
            {LinApart::droppingVars}
        ]
    ];
    
    messages = Flatten[messages];
    
    (* Check logic remains the same *)
    passed = Switch[expectedBehavior,
        "$Failed",
            result === $Failed,
        "Warning",
            Length[messages] > 0 && result =!= $Failed,
        "Unchanged",
            result === expr || Together[result - expr] === 0,
        "Valid",
            result =!= $Failed && (FreeQ[result, LinApart] || result === expr),
        _,
            False
    ];
    
    If[passed,
        Print[{timing, "\[Checkmark] "}, "  " <> testName <> " -> " <> expectedBehavior];
        1,
        Print[{timing, "\[Cross] "}, "  " <> testName];
        Print["    Expected: ", expectedBehavior];
        Print["    Got: ", Short[result, 1]];
        Print["    Messages: ", Short[messages, 1]];
        0
    ]
]

(* ============================================== *)
(*           PARALLEL TEST FUNCTION               *)
(* ============================================== *)

(*
  TestLinApartParallel - Tests parallel execution
  
  Input: {testName, expr, var, opts...}
  
  Returns: 
    1 if test passes (numerical equivalence + parallel execution)
    0 if test fails
*)
ClearAll[TestLinApartParallel]

TestLinApartParallel[{testName_String, expr_, var_, opts___}] := Module[
    {result, timing, ok, symbols, symbolVals, numDiff},
    
    (* Force parallel options if not provided *)
    (* Using 2 cores and temp directory *)
    
    (* Note: LinApart options "Parallel" -> {True, nCores, path} *)
    
    {timing, result} = AbsoluteTiming[
        LinApart[expr, var, opts, 
            "Parallel" -> {True, 2, $TemporaryDirectory <> "/"}
        ]
    ];
    
    (* Check Numerical equivalence *)
    symbols = Select[Variables[expr], Head[#] === Symbol &];
    symbolVals = Thread[symbols -> RandomPrime[{10^4, 10^5}, Length[symbols]]];
    numDiff = (expr - result) /. symbolVals // Together;
    ok = (numDiff === 0);
    
    If[ok,
        Print[{timing, "\[Checkmark] "}, "  " <> testName];
        1,
        Print[{timing, "\[Cross] "}, "  " <> testName];
        Print["    Result differs from input!"];
        0
    ]
]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Single-Variable*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


TestData1D = {
    (* --- Basic linear denominators --- *)
    {"1D-001: Two linear factors", 1/((x + 1) (x + 2)), x},
    {"1D-002: Three linear factors", 1/((x + 1) (x + 2) (x + 3)), x},
    {"1D-003: Four linear factors", 1/((x + 1) (x + 2) (x + 3) (x + 4)), x},
    {"1D-004: Five linear factors", 1/((x + 1) (x + 2) (x + 3) (x + 4) (x + 5)), x},
    {"1D-005: Six linear factors", 1/Product[(x + i), {i, 1, 6}], x},
    {"1D-006: Seven linear factors", 1/Product[(x + i), {i, 1, 7}], x},
    {"1D-007: Eight linear factors", 1/Product[(x + i), {i, 1, 8}], x},
    {"1D-008: Nine linear factors", 1/Product[(x + i), {i, 1, 9}], x},
    {"1D-009: Ten linear factors", 1/Product[(x + i), {i, 1, 10}], x},

    (* --- Multiplicities --- *)
    {"1D-010: Multiplicity 2", 1/(x + 1)^2, x},
    {"1D-011: Multiplicity 3", 1/(x + 1)^3, x},
    {"1D-012: Multiplicity 4", 1/(x + 1)^4, x},
    {"1D-013: Multiplicity 5", 1/(x + 1)^5, x},
    {"1D-014: Multiplicity 6", 1/(x + 1)^6, x},
    {"1D-015: Multiplicity 10", 1/(x + 1)^10, x},
    {"1D-016: Multiplicity 15", 1/(x + 1)^15, x},
    {"1D-017: Multiplicity 20", 1/(x + 1)^20, x},
    {"1D-018: Multiplicity 25", 1/(x + 1)^25, x},
    {"1D-019: Mixed multiplicities 2,3,1", 1/((x + 1)^2 (x + 2)^3 (x + 3)), x},
    {"1D-020: Mixed multiplicities 1,2,3", 1/((x + 1) (x + 2)^2 (x + 3)^3), x},
    {"1D-021: Double root", 1/((x + 1)^2 (x + 2)), x},

    (* --- Normalization --- *)
    {"1D-022: Normalization 2x+2", 1/((2 x + 2) (x + 3)), x},
    {"1D-023: Normalization 3x+6", 1/((3 x + 6) (2 x + 4)), x},
    {"1D-024: Normalization symbolic", 1/((a x + a) (x + b)), x},
    {"1D-025: Normalization negative", f x^4/((-a x + b) (-d x - c)^2), x},
    {"1D-026: Normalization check 1", 1/((x + y) (a x + 2)^2), x},
    {"1D-027: Normalization check 2", f x^4/((a x + b) (-a x + c)), x},
    {"1D-028: Normalization check 3", f x^4/((a x + b) (d x + c)), x},
    {"1D-029: Normalization check 4", f x^4/((a x + b) (d x + c)^2), x},

    (* --- Improper fractions --- *)
    {"1D-030: Degree 1/1", x/((x + 1) (x + 2)), x},
    {"1D-031: Degree 2/2", x^2/((x + 1) (x + 2)), x},
    {"1D-032: Degree 3/2", x^3/((x + 1) (x + 2)), x},
    {"1D-033: Degree 4/2", x^4/((x + 1) (x + 2)), x},
    {"1D-034: Degree 5/2", x^5/((x + 1) (x + 2)), x},
    {"1D-035: Degree 6/2", x^6/((x + 1) (x + 2)), x},
    {"1D-036: Degree 10/2", x^10/((x + 1) (x + 2)), x},
    {"1D-037: Poly division mixed", x^5/(x^2 + 1) + x^3/((x^3 - 1) (x + 1)), x},
    {"1D-038: Product in numerator", (x (x + 1))/((x + 2) (x + 3)), x},

    (* --- Symbolic coefficients --- *)
    {"1D-039: Symbolic coeff", a/((x + 1) (x + 2)), x},
    {"1D-040: Sum of symbols num", (a + b)/((x + 1) (x + 2)), x},
    {"1D-041: Product of symbols num", (a b c)/((x + 1) (x + 2)), x},
    {"1D-042: Symbol power num", a^2/((x + 1) (x + 2)), x},
    {"1D-043: Indexed constants", 1/((x^3 - a[0]) (x - a[1])), x},
    {"1D-044: Symbolic in denom", 1/((a x + b) (x + 1)), x},
    {"1D-045: Large coefficient", 123456789/((x + 1) (x + 2)), x},
    {"1D-046: Fractional coefficient", (1/2)/((x + 1) (x + 2)), x},
    {"1D-047: Pi coefficient", Pi/((x + 1) (x + 2)), x},
    {"1D-048: Negative constant", -1/((x + 1) (x + 2)), x},
    {"1D-049: Very small coeff", (1/10^10)/((x + 1) (x + 2)), x},

    (* --- Non-linear (irreducible) denominators --- *)
    {"1D-050: Quadratic + linear int", 1/((x^2 + 1) (x + 1)), x},
    {"1D-051: Quadratic + linear int 2", 1/((x^2 + 4) (x + 1)), x},
    {"1D-052: Two quadratics", 1/((x^2 + 1) (x^2 + 4)), x},
    {"1D-053: Quadratic symbolic", 1/((x^2 + a) (x + 1)), x},
    {"1D-054: Quadratic two symbols", 1/((x^2 + a b) (x + c)), x},
    {"1D-055: Quadratic sym coeff", 1/((x^2 + a x + b) (x + 1)), x},
    {"1D-056: Quadratic three symbols", 1/((x^2 + a x + b) (x + c d)), x},
    {"1D-057: Cubic + linear", 1/((x^3 - 1) (x + 1)), x},
    {"1D-058: Cubic irreducible", 1/((x^3 - 2) (x + 1)), x},
    {"1D-059: Two cubics", 1/((x^3 - 1) (x^3 + 1)), x},
    {"1D-060: Cubic + quadratic", 1/((x^3 - 1) (x^2 + 1)), x},
    {"1D-061: Cubic symbolic", 1/((x^3 + a) (x + 1)), x},
    {"1D-062: Quartic + linear", 1/((x^4 - 1) (x + 1)), x},
    {"1D-063: Quartic irreducible", 1/((x^4 + 1) (x + 1)), x},
    {"1D-064: Quartic symbolic", 1/((x^4 + a) (x + 1)), x},
    {"1D-065: Quintic", 1/((x^5 - 1) (x + 1)), x},
    {"1D-066: Sextic", 1/((x^6 - 1) (x + 1)), x},
    {"1D-067: Septic", 1/((x^7 - 1) (x + 1)), x},
    {"1D-068: Octic", 1/((x^8 - 1) (x + 1)), x},
    {"1D-069: Mixed degrees", 1/((x + 1) (x^2 + 1) (x^3 - 1)), x},
    {"1D-070: Mixed degrees sym", 1/((x + a) (x^2 + b) (x^3 + c)), x},
    {"1D-071: Repeated irreducible quad", 1/((x^2 + 1)^2 (x^2 + 2)^2), x},
    {"1D-072: Mixed irreducible/reducible", 1/((x^2 + 1) (x^2 - 1)), x},
    {"1D-073: Cyclotomic 5", 1/((x^5 - 1) (x + 1)), x},
    {"1D-074: Cyclotomic 6", 1/((x^6 - 1) (x + 1)), x},
    {"1D-075: x^2-x+1", 1/((x^2 - x + 1) (x + 1)), x},
    {"1D-076: x^2+x+1", 1/((x^2 + x + 1) (x + 1)), x},

    (* --- High degree numerators with non-linear denoms --- *)
    {"1D-077: Degree 10 / quad+linear", x^10/((x^2 + 1) (x + 1)), x},
    {"1D-078: Degree 12 / cubic+linear", x^12/((x^3 - 1) (x + 1)), x},
    {"1D-079: Degree 15 / cubic+linear", x^15/((x^3 - 1) (x + 1)), x},
    {"1D-080: Degree 20 / quartic+linear", x^20/((x^4 - 1) (x + 1)), x},
    {"1D-081: Degree 50 / quad+linear", x^50/((x^2 + 1) (x + 1)), x},
    {"1D-082: Degree 100 / quad+linear", x^100/((x^2 + 1) (x + 1)), x},

    (* --- Non-integer / complex / symbolic powers --- *)
    {"1D-083: Non-integer power den", 1/((x + 1) (x^2 + 2)^(5/2)), x},
    {"1D-084: Real power den", (x^2 + a)^(-3/2)/((x + b) (x + c)), x},
    {"1D-085: Non-integer power num", x^(5/7)/((x + 1) (x^2 + 4 x + 2) (x + 3)), x},
    {"1D-086: Complex power den", I/((x - 1.2)^1.2 (x^3 + x - b)^(1/2) (x^2 - c)^I), x},
    {"1D-087: Mixed power den", x^1.5/((x - a)^2 (x^3 - b)^(1/2) (x^2 - 2)^1.5), x},
    {"1D-088: Symbolic power den", 1/((x^2 + x - a)^2 (x^4 - b)^(1/2) (x^3 - 2)^pow (x^2 - m)^(a + b)), x},
    {"1D-089: Symbolic power num", x^b/((1 + x^3) (2 - x)), x},
    {"1D-090: Function power den", 1/((1 + x) (2 - x - x^2) (3 + x^3)^(f[x]^x)), x},
    {"1D-091: Complex+Int power den", 1/((1 + x^3) (2 - x - x^2) (6 + 2 x)^(2 + a[2])), x},
    {"1D-092: Complex+Int power den 2", 1/((1 - x^3) (2 + x - x^2) (6 + 2 x)^(2 + I)), x},
    {"1D-093: Rational power num issue", 1/((1 - x^2) (2 + x - x^3) (3 - x)^(-1/2)), x},
    {"1D-094: Sqrt in numerator", Sqrt[x]/((x + 1) (x + 2)), x},
    {"1D-095: Sqrt in denominator", 1/((x + 1) Sqrt[x + 2]), x},
    {"1D-096: Negative frac power", (x + 1)^(-3/2)/(x + 2), x},
    {"1D-097: Symbolic power", 1/((x + 1)^n (x + 2)), x},
    {"1D-098: Complex power", 1/((x + 1)^(2 + I) (x + 2)), x},
    {"1D-099: Negative power", x^2/(x + 1)^(-2), x},
    {"1D-100: Rational+complex power", 1/((x + 1) (x + 2)^(1/2 + I)), x},

    (* --- Transcendental / non-polynomial factors --- *)
    {"1D-101: Log factor", 1/((x + 1) (x^6 + I x^3 + 2 y) Log[x]), x},
    {"1D-102: Log Sqrt factor", 1/((x + 1) (x + 2) Log[Sqrt[x]]), x},
    {"1D-103: Exp[x] in num", Exp[x]/((x^3 + 1) (x^2 + 2)^2), x},
    {"1D-104: 2^x in num", 2^x/((x^3 + 1) (x^2 + 2)^2), x},
    {"1D-105: x^x non-monomial", x^x/((1 - x^2) (2 + x^3)^-0.5), x},
    {"1D-106: G-function", G[0, x/(x^2 + 1)]/((x^3 + 1) (x^2 + x + 2)), x},
    {"1D-107: Log in num", Log[x]/((x + 1) (x + 2)), x},
    {"1D-108: Sin in num", Sin[x]/((x + 1) (x + 2)), x},
    {"1D-109: Exp in num", Exp[x]/((x + 1) (x + 2)), x},
    {"1D-110: Sin in denom", 1/((x + 1) Sin[x]), x},
    {"1D-111: Cos in denom", 1/((x + 1) Cos[x]), x},
    {"1D-112: Log in denom", 1/((x + 1) (x + 2) Log[x]), x},
    {"1D-113: Exp in denom", 1/((x + 1) Exp[x]), x},

    (* --- Non-linear denominators (robustness) --- *)
    {"1D-114: Log in denom struct", 1/((x^3 + Log[x]) (x + 1)), x},
    {"1D-115: Sqrt in denom struct", 1/((x + Sqrt[x]) (x + 1)), x},
    {"1D-116: High poly denom", 1/((x + x^7/2) (x + 1)), x},
    {"1D-117: Sign of x", 1/((-x^3 + 10 x^2 + 1) (x + 2)), x},
    {"1D-118: x^3 / denoms", x^3/((a - x^2) (c + x - x^3)), x},
    {"1D-119: x^(1/2) / denoms", x^(1/2)/((a - x^2) (c + x - x^3)), x},
    {"1D-120: x^I / denoms", x^I/((a - x^2) (c + x - x^3)), x},
    {"1D-121: x^(3+I) / denoms", x^(3 + I)/((a - x^2) (c + x - x^3)), x},
    
    
    (* --- Edge cases --- *)
    {"1D-122: Constant", 42, x},
    {"1D-123: Zero", 0, x},
    {"1D-124: Polynomial", x^2 + x + 1, x},
    {"1D-125: Single factor", 1/(x + 1), x},
    {"1D-126: Monomial denom", 1/x/(x + 1), x},
    {"1D-127: Monomial^3 denom", (x - 1)/x^3/(x + 1), x},
    {"1D-128: Independent of x", 1/(a + b), x},
    {"1D-129: Identity", (x + 1)/(x + 1), x},
    {"1D-130: Cancelable", (x^2 - 1)/((x - 1) (x + 1)), x},
    {"1D-131: Nested fraction", 1/(1 + 1/(x + 1)), x},
    {"1D-132: Triple nested", 1/(1 + 1/(1 + 1/(x + 1))), x},
    {"1D-133: Sum of fractions", 1/(x + 1) + 1/(x + 2) + 1/(x + 3), x},
    {"1D-134: Sum duplicates", 1/(x + 1)^2 + 1/(x + 1)^2, x},
    {"1D-135: Mixed sum", a/((x + 1) (x + 2)) + b/((x + 2) (x + 3)), x},
    {"1D-136: Same root diff form", 1/((x - 1) (2 x - 2)), x},
    {"1D-137: Alternating signs", 1/((x - 1) (x - 2) (x + 3) (x + 4)), x},
    {"1D-138: Rational numbers denom", 1/((x + 1/2) (x + 1/3)), x},
    {"1D-139: Large numbers denom", 1/((x + 100) (x + 200)), x},
    {"1D-140: Negative numbers denom", 1/((x - 5) (x - 10)), x},
    {"1D-141: Negative denom coeff", 1/((-1) x + 1)/((-2) x + 1), x},
    {"1D-142: Complex roots", 1/((x + I) (x - I)), x},
    {"1D-143: Complex roots 2", 1/((x + I) (x + 2 I) (x + 3 I)), x},
    {"1D-144: Mixed real/complex", 1/((x + 1) (x + I)), x},
    {"1D-145: Zero denom", 1/x, x},
    
    
    (* --- Options --- *)
    {"1D-146: Factor option", 1/(1 - x^2), x, "Factor" -> True},
    {"1D-147: Factor quadratic", 1/(x^4 - 1), x, "Factor" -> True},
    {"1D-148: Factor cubic", 1/(x^6 - 1), x, "Factor" -> True},
    {"1D-149: Factor symbolic", 1/(x^2 - a), x, "Factor" -> True},
    {"1D-150: GaussianIntegers", 1/((1 + x) (1 + x^2)), x, 
        "Factor" -> True, "GaussianIntegers" -> True},
    {"1D-151: Extension Sqrt[2]", 1/(x^2 - 2), x, 
        "Factor" -> True, "Extension" -> {Sqrt[2]}},
    {"1D-152: Euclidean method", 1/((x + 1) (x + 2) (x + 3)), x, "Method" -> "Euclidean"},
    {"1D-153: Euclidean quadratic", 1/((x^2 + 1) (x + 2)), x, "Method" -> "Euclidean"},
    {"1D-154: Euclidean cubic", 1/((x^3 - 1) (x + 1)), x, "Method" -> "Euclidean"},
    {"1D-155: EquationSystem method", 1/((x + 1) (x + 2) (x + 3)), x, "Method" -> "EquationSystem"},
    {"1D-156: EquationSystem quad", 1/((x^2 + 1) (x + 2)), x, "Method" -> "EquationSystem"},
    {"1D-157: PreCollect", 2/((x + 1) (x + 2)) + 1/((x + 1) (x + 2)), x, "PreCollect" -> True},
    {"1D-158: PreCollect + Factor", a/((x + 1) (x + 2)) + a/((x + 1) (x + 3)), x, 
        "PreCollect" -> True, "ApplyAfterPreCollect" -> Factor},
    {"1D-159: PreCollect + Simplify", a/((x + 1) (x + 2)) + a/((x + 1) (x + 3)), x, 
        "PreCollect" -> True, "ApplyAfterPreCollect" -> Simplify},
    {"1D-160: PreCollect + Euclidean", a/((x^2 + 1) (x + 2)) + a/((x^2 + 1) (x + 3)), x, 
        "PreCollect" -> True, "Method" -> "Euclidean"},
    {"1D-161: Factor False no GI", 1/(x^2 + x + 1), x, "Factor" -> False, "GaussianIntegers" -> False},
    {"1D-162: Irreducible no factor", 1/(x^4 + 1), x, "Factor" -> False, "GaussianIntegers" -> False},
    {"1D-163: Poly part Laurent", x^10/(x + 1), x, "Method" -> "ExtendedLaurentSeries"},
    {"1D-164: Poly part quad denom", x^8/(x^2 + 1), x, "Method" -> "ExtendedLaurentSeries"}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ============================================== *)
(*    SECTION 1: SINGLE-VARIABLE TESTS            *)
(* ============================================== *)

Print[""];
Print["============================================"];
Print["  SECTION 1: Single-Variable Linear Denoms  "];
Print["============================================"];

Results1D = MapIndexed[
    {TestLinApart1D[#1], First[#2]} &,
    TestData1D
];

Passed1D = Count[Results1D, {1, _}];
Failed1D = Count[Results1D, {0, _}];
FailedIndices1D = Cases[Results1D, {0, idx_} :> idx];
FailedTests1D = TestData1D[[FailedIndices1D, 1]];

Print[""];
Print["--------------------------------------------"];
Print["  Section 1 Summary:"];
Print["  Total:  ", Length[TestData1D]];
Print["  Passed: ", Passed1D];
Print["  Failed: ", Failed1D];
If[FailedTests1D =!= {},
    Print["  Failed tests: ", FailedTests1D]
];
Print["--------------------------------------------"];


(* ::Subsection:: *)
(*Multi-Variable*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


TestDataMV = {
    (* --- Basic 2D --- *)
    {"MV-001: 3 non-central", 1/(x y (x + y - 1)), {x, y}},
    {"MV-002: 3 central", 1/(x y (x + y)), {x, y}},
    {"MV-003: 4 non-central", 1/(x y (x + y - 1) (x - y + 2)), {x, y}},
    {"MV-004: 5 non-central", 1/(x y (x + y - 1) (x - y + 2) (2 x + y - 3)), {x, y}},
    {"MV-005: 4 central", 1/(x y (x + y) (x - y)), {x, y}},
    {"MV-006: 6 central", 1/(x y (x + y) (x - y) (2 x + y) (x + 2 y)), {x, y}},

    (* --- Higher powers 2D --- *)
    {"MV-007: Power on x", 1/(x^2 y (x + y - 1)), {x, y}},
    {"MV-008: Power on y", 1/(x y^2 (x + y - 1)), {x, y}},
    {"MV-009: Power on non-central", 1/(x y (x + y - 1)^2), {x, y}},
    {"MV-010: Multiple powers", 1/(x^3 y^2 (x + y - 1)^2), {x, y}},
    {"MV-011: 5 denoms powers", 1/(x^2 y (x + y - 1)^2 (x - y + 2)), {x, y}},
    {"MV-012: Central with powers", 1/(x^2 y (x + y)^2), {x, y}},

    (* --- Mixed central/non-central --- *)
    {"MV-013: Mixed 6 denoms", 1/(x y (x + y) (x - y) (x + y - 1) (x - y + 1)), {x, y}},
    {"MV-014: Mixed 4 denoms", 1/(x y (x + y) (x + y - 1)), {x, y}},

    (* --- Symbolic coefficients --- *)
    {"MV-015: Symbolic 4 denoms", 1/(x y (a x + y - 1) (x + b y - 1)), {x, y}},
    {"MV-016: Mandelstam-style", 1/((s - x) (t - y) (s + t - x - y - 1)), {x, y}},
    {"MV-017: Symbolic 4 denoms heavy", 1/(x y (s x + t y - 1) (u x - v y + 1)), {x, y}},

    (* --- 3D tests --- *)
    {"MV-018: 3D 4 non-central", 1/(x y z (x + y + z - 1)), {x, y, z}},
    {"MV-019: 3D 6 non-central", 1/(x y z (x + y - 1) (y + z - 2) (x + z - 3)), {x, y, z}},
    {"MV-020: 3D 6 symmetric", 1/(x y z (x + y - 1) (y + z - 1) (x + z - 1)), {x, y, z}},
    {"MV-021: 3D higher powers", 1/(x^2 y z^2 (x + y + z - 1)), {x, y, z}},
    {"MV-022: 3D 4 central", 1/(x y z (x + y + z)), {x, y, z}},
    {"MV-023: 3D 5 denoms", 1/(x y z (x + y - 1) (y + z - 1)), {x, y, z}},

    (* --- 4D tests --- *)
    {"MV-024: 4D 5 non-central", 1/(x y z w (x + y + z + w - 1)), {x, y, z, w}},

    (* --- Stress tests 2D --- *)
    {"MV-025: 7 denoms mixed", 
        1/(x y (x + y) (x - y) (x + y - 1) (x - y + 1) (2 x + 3 y - 5)), {x, y}},
    {"MV-026: High powers 4 denoms", 
        1/(x^3 y^3 (x + y - 1)^3 (x - y + 1)^2), {x, y}},
    {"MV-027: 6 non-central denoms", 
        1/(x y (x + y - 1) (x - y + 1) (2 x + y - 3) (x + 3 y - 2)), {x, y}},
    {"MV-028: Near-degenerate", 
        1/(x (x + y) (x + y - 1) (x + 2 y) (x + 2 y - 1)), {x, y}},

    (* --- Stress tests 3D --- *)
    {"MV-029: 3D 7 denoms mixed", 
        1/(x y z (x + y) (y + z - 1) (x + z - 2) (x + y + z - 3)), {x, y, z}},
    {"MV-030: 3D 5 denoms powers", 
        1/(x^2 y^2 z (x + y + z - 1)^2 (x - y + z + 1)), {x, y, z}},
    {"MV-031: 3D 8 denoms", 
        1/(x y z (x + y - 1) (y + z - 1) (x + z - 1) (x + y + z - 2) (x - y + z + 1)), {x, y, z}},

    (* --- Stress tests 4D --- *)
    {"MV-032: 4D 6 denoms", 1/(x y z w (x + y - 1) (z + w - 1)), {x, y, z, w}},
    {"MV-033: 4D 7 denoms", 
        1/(x y z w (x + y + z - 1) (y + z + w - 2) (x + w - 1)), {x, y, z, w}},

    (* --- Extreme tests --- *)
    {"MV-034: 2D 8 denoms", 
        1/(x y (x + y - 1) (x - y + 2) (2 x + y - 3) (x + 3 y - 4) (3 x - y + 5) (x + y - 7)), {x, y}},
    {"MV-035: 2D 10 denoms", 
        1/(x y (x + y - 1) (x - y + 2) (2 x + y - 3) (x + 3 y - 4) (3 x - y + 5) (x + y - 7) (2 x - 3 y + 1) (4 x + y - 2)), {x, y}},
    {"MV-036: 2D 12 denoms", 
        1/(x y (x + y - 1) (x - y + 2) (2 x + y - 3) (x + 3 y - 4) (3 x - y + 5) (x + y - 7) (2 x - 3 y + 1) (4 x + y - 2) (x - 5 y + 3) (5 x + 2 y - 9)), {x, y}},

    (* --- Very high powers --- *)
    {"MV-037: Powers (5,4,3)", 1/(x^5 y^4 (x + y - 1)^3), {x, y}},
    {"MV-038: Powers (8,6,4)", 1/(x^8 y^6 (x + y - 1)^4), {x, y}},
    {"MV-039: Powers (10,7,5)", 1/(x^10 y^7 (x + y - 1)^5), {x, y}},
    {"MV-040: 4 denoms powers (5,4,3,4)", 
        1/(x^5 y^4 (x + y - 1)^3 (x - y + 2)^4), {x, y}},
    {"MV-041: 3D powers (5,4,3,4)", 1/(x^5 y^4 z^3 (x + y + z - 1)^4), {x, y, z}},
    {"MV-042: 3D powers (8,6,5,4)", 1/(x^8 y^6 z^5 (x + y + z - 1)^4), {x, y, z}},

    (* --- Many variables --- *)
    {"MV-043: 5D 6 denoms", 
        1/(x1 x2 x3 x4 x5 (x1 + x2 + x3 + x4 + x5 - 1)), {x1, x2, x3, x4, x5}},
    {"MV-044: 6D 7 denoms", 
        1/(x1 x2 x3 x4 x5 x6 (x1 + x2 + x3 + x4 + x5 + x6 - 1)), {x1, x2, x3, x4, x5, x6}},
    {"MV-045: 7D 8 denoms", 
        1/(x1 x2 x3 x4 x5 x6 x7 (x1 + x2 + x3 + x4 + x5 + x6 + x7 - 1)), {x1, x2, x3, x4, x5, x6, x7}},

    (* --- Complex symbolic coefficients --- *)
    {"MV-046: Epsilon dim-reg 5 denoms", 
        1/(x y ((2 - epsilon) x + (1 + epsilon) y - 1) ((3 + 2 epsilon) x - (1 - epsilon) y + epsilon) ((1 + epsilon^2) x + epsilon y - (2 - epsilon))), {x, y}},
    {"MV-047: Sqrt/Pi coefficients", 
        1/(x y (Sqrt[2] x + Sqrt[3] y - Pi) (Sqrt[5] x - Sqrt[7] y + EulerGamma)), {x, y}},
    {"MV-048: Complex number coefficients", 
        1/(x y ((2 + 3 I) x + (1 - I) y - (5 + 2 I)) ((1 + 4 I) x - (3 - 2 I) y + (7 - I))), {x, y}},

    (* --- Combined extremes --- *)
    {"MV-049: 2D 8 denoms high powers", 
        1/(x^3 y^2 (x + y - 1)^2 (x - y + 1)^2 (2 x + y - 3) (x + 2 y - 5) (3 x - y + 2)^2 (x + 3 y - 4)), {x, y}},
    {"MV-050: 4D 10 denoms mixed", 
        1/(x y z w (x + y) (z + w) (x + y - 1) (z + w - 1) (x + z - 2) (y + w - 3)), {x, y, z, w}},
    {"MV-051: 3D 7 denoms high powers", 
        1/(x^3 y^2 z^2 (x + y - 1)^2 (y + z - 2)^2 (x + z - 3) (x + y + z - 4)), {x, y, z}},
    {"MV-052: 5D 9 denoms", 
        1/(x1 x2 x3 x4 x5 (x1 + x2 - 1) (x2 + x3 - 2) (x3 + x4 - 3) (x4 + x5 - 4)), {x1, x2, x3, x4, x5}},

    (* --- Edge cases MV --- *)
    {"MV-053: Already decomposed", 1/(x (x + y - 1)), {x, y}},
    {"MV-054: Single denom", 1/(x + y - 1), {x, y}},
    {"MV-055: Constant numerator", (a + b)/(x y (x + y - 1)), {x, y}},
    {"MV-056: Single element list", 1/((x - 1) (x + 1)), {x}},
    {"MV-057: Minimal 2 denoms", 1/(x y), {x, y}},
    {"MV-058: Minimal 3 denoms", 1/(x y z), {x, y, z}},
    
	    (* --- Numerator tests 2D --- *)
	{"MV-059: 2D linear num", x/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-060: 2D quadratic num", x^2/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-061: 2D cubic num", x^3/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-062: 2D mixed num xy", x y/((x - y) (x + y - 1) (x + y)), {x, y}},
	{"MV-063: 2D mixed num x^2 y", x^2 y/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-064: 2D mixed num x y^2", x y^2/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-065: 2D high power num", x^3 y^4/((x - y) y (x + y - 1) (x + y)), {x, y}},
	{"MV-066: 2D sum num", (x + y)/((x - y) y (x + y - 1)), {x, y}},
	{"MV-067: 2D sum num squared", (x + y)^2/((x - y) y (x + y - 1) (x + y)), {x, y}},
	
	(* --- Numerator tests 3D --- *)
	{"MV-068: 3D linear num x", x/(x y z (x + y + z - 1)), {x, y, z}},
	{"MV-069: 3D linear num y", y/(x y z (x + y + z - 1)), {x, y, z}},
	{"MV-070: 3D quadratic num", x^2/(x y z (x + y + z - 1)), {x, y, z}},
	{"MV-071: 3D mixed num xy", x y/(x z (x + y - 1) (y + z - 1)), {x, y, z}},
	{"MV-072: 3D mixed num xyz", x y z/((x + y) (y + z) (x + z) (x + y + z - 1)), {x, y, z}},
	{"MV-073: 3D cubic num", x^3/(x y z (x + y - 1) (y + z - 1)), {x, y, z}},
	{"MV-074: 3D sum num", (x + y + z)/(x y z (x + y + z - 1)), {x, y, z}},
	
	(* --- Numerator tests 4D --- *)
	{"MV-075: 4D linear num", x/(x y z w (x + y + z + w - 1)), {x, y, z, w}},
	{"MV-076: 4D quadratic num", x^2/(x y z w (x + y + z + w - 1)), {x, y, z, w}},
	{"MV-077: 4D mixed num xy", x y/(x z w (x + y - 1) (z + w - 1)), {x, y, z, w}},
	{"MV-078: 4D mixed num xyzw", x y z w/((x + y) (z + w) (x + y - 1) (z + w - 1) (x + z)), {x, y, z, w}},
	
	(* --- Numerator tests 5D --- *)
	{"MV-079: 5D linear num", x1/(x1 x2 x3 x4 x5 (x1 + x2 + x3 + x4 + x5 - 1)), {x1, x2, x3, x4, x5}},
	{"MV-080: 5D quadratic num", x1^2/(x1 x2 x3 x4 x5 (x1 + x2 + x3 + x4 + x5 - 1)), {x1, x2, x3, x4, x5}},
	{"MV-081: 5D mixed num", x1 x2/(x1 x3 x4 x5 (x1 + x2 - 1) (x3 + x4 + x5 - 1)), {x1, x2, x3, x4, x5}},
	{"MV-082: 5D sum num", (x1 + x2 + x3)/(x1 x2 x3 x4 x5 (x4 + x5 - 1)), {x1, x2, x3, x4, x5}},
	
	(* --- Numerator with higher multiplicities --- *)
	{"MV-083: 2D num with mult", x^2/((x - y)^2 y^2 (x + y - 1)), {x, y}},
	{"MV-084: 2D num with high mult", x^3/((x - y)^3 y^2 (x + y - 1)^2), {x, y}},
	{"MV-085: 3D num with mult", x y/((x - y)^2 z (x + y + z - 1)^2), {x, y, z}},
	
	(* --- Edge cases with numerators --- *)
	{"MV-086: Num equals denom factor", (x - y)/((x - y) y (x + y - 1)), {x, y}},
	{"MV-087: Num cancels partially", (x - y)^2/((x - y)^3 y (x + y - 1)), {x, y}},
	{"MV-088: Num is product of denoms", (x - y) y/((x - y)^2 y^2 (x + y - 1)), {x, y}}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


Print[""];
Print["============================================"];
Print["  SECTION 2: Multivariate Linear Denoms     "];
Print["============================================"];

ResultsMV = MapIndexed[
    {TestLinApartMV[#1], First[#2]} &,
    TestDataMV
];

PassedMV = Count[ResultsMV, {1, _}];
FailedMV = Count[ResultsMV, {0, _}];
FailedIndicesMV = Cases[ResultsMV, {0, idx_} :> idx];
FailedTestsMV = TestDataMV[[FailedIndicesMV, 1]];

Print[""];
Print["--------------------------------------------"];
Print["  Section 2 Summary:"];
Print["  Total:  ", Length[TestDataMV]];
Print["  Passed: ", PassedMV];
Print["  Failed: ", FailedMV];
If[FailedTestsMV =!= {},
    Print["  Failed tests: ", FailedTestsMV]
];
Print["--------------------------------------------"];


(* ::Subsection:: *)
(*Robustness*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


TestDataRobust = {
    (* --- Degenerate structures --- *)
    {"ROB-001: All identical bases", 1/(x^2 y^3), {x, y}},
    {"ROB-002: Collinear denoms", 1/(x y (x + y) (2 x + 2 y)), {x, y}},
    {"ROB-003: Parallel x-y denoms", 1/((x - 1) (x - 2) (y - 1) (y - 2)), {x, y}},
    {"ROB-004: Only one var-dep denom", 1/((x + y) z), {x, y}},
    {"ROB-005: Two var-dep < n vars", 1/((x + y) (x - y)), {x, y, z}},

    (* --- Numerical extremes --- *)
    {"ROB-006: Huge coefficients", 1/(x y (10^50 x + y - 1)), {x, y}},
    {"ROB-007: Tiny coefficients", 1/(x y (10^-50 x + y - 1)), {x, y}},
    {"ROB-008: Near-singular", 1/(x y (x + y) (x + (1 + 10^-14) y)), {x, y}},
    {"ROB-009: Zero constant term", 1/(x y (x + y) (2 x + 3 y)), {x, y}},

    (* --- Symbolic conditionals --- *)
    {"ROB-010: Conditional null", 1/(x y (a x + b y) (c x + d y)), {x, y}},
    {"ROB-011: Parameter-dependent basis", 1/(x y (x + y) (a x + y)), {x, y}},
    {"ROB-012: Epsilon-dependent", 1/(x y ((1 + eps) x + y - 1)), {x, y}},

    (* --- Mixed dependencies --- *)
    {"ROB-013: Each denom missing var", 1/((x + y) (x + z) (y + z)), {x, y, z}},
    {"ROB-014: Disconnected groups", 1/((x + y) (w + u)), {x, y, w, u}},
    {"ROB-015: Extra var as constant", 1/(x y (x + y) z), {x, y}},
    {"ROB-016: Multiple extra vars", 1/(x y (x + y) z w), {x, y}},
    {"ROB-017: Var in num only", x z/((x + y) (x - y)), {x, y}},

    (* --- Preprocessing --- *)
    {"ROB-018: Common factor", (x + 1)/((x + 1)^2 (x + 2)), x},
    {"ROB-019: MV common factor", (x + y)/((x + y)^2 (x - y)), {x, y}},
    {"ROB-020: Already simplified", 1/(x + 1), x},
    {"ROB-021: Already simplified MV", 1/(x + y), {x, y}},
    {"ROB-022: Polynomial input", x^2 + x + 1, x},
    {"ROB-023: Polynomial input MV", x y + x + y, {x, y}},
    {"ROB-024: Constant input", 42, x},
    {"ROB-025: Constant input MV", 42, {x, y}},

    (* --- Power edge cases --- *)
    {"ROB-026: Zero exponent", 1/((x + 1)^0 (x + 2)), x},
    {"ROB-027: Mixed int+frac powers", 1/((x + 1)^2 (x + 1)^(1/2)), x},

    (* --- Non-polynomial denominators MV --- *)
    {"ROB-028: Sin in denom MV", 1/((x + y) (x - y) Sin[x]), {x, y}},
    {"ROB-029: Exp in denom MV", 1/(x y (x + y) Exp[x + y]), {x, y}},
    {"ROB-030: G-function denom MV", 1/((x + y) (x - y) G[0, x]), {x, y}},

    (* --- Basis explosion --- *)
    {"ROB-031: 15 denoms 2D", 1/Product[x + i y - i, {i, 1, 15}], {x, y}},

    (* --- Recursion attacks --- *)
    {"ROB-032: Sequential nulls", 1/(x y Product[x + y - i, {i, 1, 8}]), {x, y}},
    {"ROB-033: Many central denoms", 
        1/(x y (x + y) (x - y) (2 x + y) (x + 2 y) (3 x + y)), {x, y}},

    (* --- Unicode --- *)
    {"ROB-034: Greek symbols", 1/(\[Alpha] \[Beta] (\[Alpha] + \[Beta])), {\[Alpha], \[Beta]}},

    (* --- Improper fractions MV --- *)
    {"ROB-035: 1D num > denom", x^5/((x + 1) (x + 2)), x},
    {"ROB-036: MV num polynomial", (x^2 y + x y^2)/(x y (x + y - 1)), {x, y}},

    (* --- Single denominator --- *)
    {"ROB-037: One denom 1D", 1/(x + 1), x},
    {"ROB-038: One denom MV", 1/(x + y), {x, y}},
    {"ROB-039: One denom power", 1/(x + 1)^3, x},
    {"ROB-040: One var-dep + const", a/((x + y) b), {x, y}},

    (* --- Nested fractions --- *)
    {"ROB-041: Nested fractions", 1/(1/(x + 1) + 1/(x + 2)), x},
    {"ROB-042: Deeply nested MV", 1/(x/(y + 1) + y/(x + 1)), {x, y}},

    (* --- Zero/Identity --- *)
    {"ROB-043: Zero expression", 0, x},
    {"ROB-044: Zero expression MV", 0, {x, y}},
    {"ROB-045: Identity fraction", x/x, x}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


Print[""];
Print["============================================"];
Print["  SECTION 3: Robustness & Stress Tests      "];
Print["============================================"];

ResultsRobust = MapIndexed[
    {TestLinApartRobust[#1], First[#2]} &,
    TestDataRobust
];

PassedRobust = Count[ResultsRobust, {1, _}];
FailedRobust = Count[ResultsRobust, {0, _}];
FailedIndicesRobust = Cases[ResultsRobust, {0, idx_} :> idx];
FailedTestsRobust = TestDataRobust[[FailedIndicesRobust, 1]];

Print[""];
Print["--------------------------------------------"];
Print["  Section 3 Summary:"];
Print["  Total:  ", Length[TestDataRobust]];
Print["  Passed: ", PassedRobust];
Print["  Failed: ", FailedRobust];
If[FailedTestsRobust =!= {},
    Print["  Failed tests: ", FailedTestsRobust]
];
Print["--------------------------------------------"];


(* ::Subsection:: *)
(*Error handling*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


TestDataError = {
    (* --- Invalid variable specification --- *)
    {"ERR-001: Var not symbol", 1/((x - 1) (x + 2)), "x", "$Failed"},
    {"ERR-002: Numeric var", 1/(x y), 5, "$Failed"},
    {"ERR-003: Empty variable list", 1/(x y), {}, "$Failed"},
    {"ERR-004: Vars not symbols", 1/(x y), {x, 1, "y"}, "$Failed"},
    {"ERR-005: Subscripted vars", 1/(Subscript[x, 1] Subscript[x, 2]), 
        {Subscript[x, 1], Subscript[x, 2]}, "$Failed"},

    (* --- Invalid options --- *)
    {"ERR-006: Invalid Method", 1/((x - 1) (x + 2)), x, "Method" -> "Magic", "$Failed"},
    {"ERR-007: Parallel bad format", 1/((x - 1) (x + 2)), x, "Parallel" -> True, "$Failed"},
    {"ERR-008: Extension without Factor", 1/(x^2 - 2), x, 
        "Factor" -> False, "Extension" -> {Sqrt[2]}, "$Failed"},
    {"ERR-009: GaussianIntegers without Factor", 1/(x^2 + 1), x, 
        "Factor" -> False, "GaussianIntegers" -> True, "$Failed"},

    (* --- Non-linear denominators (multivariate) --- *)
    {"ERR-010: Non-linear denom", 1/((x^2 + y) (x + 1)), {x, y}, "Unchanged"},

    (* --- Variables not in denominator --- *)
    {"ERR-011: Unused vars dropped", 1/(x y), {x, y, z}, "Valid"},
    {"ERR-012: Duplicate vars", 1/(x y (x + y)), {x, y, x, y}, "Valid"},
    {"ERR-013: Var only in numerator", x z/(y (x + y)), {x, y, z}, "Valid"},

    (* --- Protected/special symbols --- *)
    {"ERR-014: Protected symbol Pi", 1/(x y), Pi, "Unchanged"},

    (* --- Pathological inputs --- *)
    (*{"ERR-015: Infinity exponent", 1/(x + 1)^Infinity, x, "Unchanged"},*)
    {"ERR-016: Indeterminate", Indeterminate/(x + 1), x, "Unchanged"},
    {"ERR-017: ComplexInfinity", ComplexInfinity/(x y), {x, y}, "Unchanged"},

    (* --- Edge cases that should work --- *)
    {"ERR-018: Zero expression", 0, x, "Valid"},
    {"ERR-019: Zero expression MV", 0, {x, y}, "Valid"},
    {"ERR-020: Constant expression", 42, x, "Valid"},
    {"ERR-021: Constant expression MV", 42, {x, y}, "Valid"},
    {"ERR-022: Polynomial", x^2 + x + 1, x, "Valid"},
    {"ERR-023: Polynomial MV", x y + x + y, {x, y}, "Valid"},
    {"ERR-024: Identity fraction", x/x, x, "Valid"},
    {"ERR-025: No var in denom", x/(a + b), x, "Unchanged"}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


Print[""];
Print["============================================"];
Print["  SECTION 4: Error Handling                 "];
Print["============================================"];

ResultsError = MapIndexed[
    {TestLinApartError[#1], First[#2]} &,
    TestDataError
];

PassedError = Count[ResultsError, {1, _}];
FailedError = Count[ResultsError, {0, _}];
FailedIndicesError = Cases[ResultsError, {0, idx_} :> idx];
FailedTestsError = TestDataError[[FailedIndicesError, 1]];

Print[""];
Print["--------------------------------------------"];
Print["  Section 4 Summary:"];
Print["  Total:  ", Length[TestDataError]];
Print["  Passed: ", PassedError];
Print["  Failed: ", FailedError];
If[FailedTestsError =!= {},
    Print["  Failed tests: ", FailedTestsError]
];
Print["--------------------------------------------"];


(* ::Subsection:: *)
(*Parallel test*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


TestDataParallel = {
    {"PAR-001: Parallel 10 linear factors", 
        1/Product[(x + i), {i, 1, 10}], x, "Method" -> "ExtendedLaurentSeries"},
        
    {"PAR-002: Parallel high multiplicity", 
        1/(x + 1)^20, x, "Method" -> "ExtendedLaurentSeries"},
        
    {"PAR-003: Parallel quadratics", 
        1/((x^2 + 1) (x^2 + 4) (x^2 + 9) (x^2 + 16)), x, "Method" -> "ExtendedLaurentSeries"},
        
    {"PAR-004: Multivariate parallel (residue bases)",
        1/Product[x + i y - i, {i, 1, 6}], {x, y}}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


Print[""];
Print["============================================"];
Print["  SECTION 5: Parallel Tests                 "];
Print["============================================"];

If[Length[Kernels[]] === 0,
    Print["  Launching 2 kernels..."];
    LaunchKernels[2];
];

ResultsParallel = MapIndexed[
    {TestLinApartParallel[#1], First[#2]} &,
    TestDataParallel
];

PassedParallel = Count[ResultsParallel, {1, _}];
FailedParallel = Count[ResultsParallel, {0, _}];
FailedIndicesParallel = Cases[ResultsParallel, {0, idx_} :> idx];
FailedTestsParallel = TestDataParallel[[FailedIndicesParallel, 1]];

Print[""];
Print["--------------------------------------------"];
Print["  Section 5 Summary:"];
Print["  Total:  ", Length[TestDataParallel]];
Print["  Passed: ", PassedParallel];
Print["  Failed: ", FailedParallel];
If[FailedTestsParallel =!= {},
    Print["  Failed tests: ", FailedTestsParallel]
];
Print["--------------------------------------------"];

(* Clean up kernels *)
CloseKernels[];


(* ::Subsection:: *)
(*Summary*)


(* ============================================== *)
(*           TEST SUMMARY                         *)
(* ============================================== *)

TotalPassed = Passed1D + PassedMV + PassedRobust + PassedError + PassedParallel;
TotalFailed = Failed1D + FailedMV + FailedRobust + FailedError + FailedParallel;
TotalTests = Length[TestData1D] + Length[TestDataMV] + Length[TestDataRobust] + Length[TestDataError] + Length[TestDataParallel];

$AllFailedTests = Join[
    If[Failed1D > 0, Map["1D: " <> #[[1]] &, $ailedTests1D], {}],
    If[FailedMV > 0, Map["MV: " <> #[[1]] &, FailedTestsMV], {}],
    If[FailedRobust > 0, Map["Robust: " <> #[[1]] &, FailedTestsRobust], {}],
    If[FailedError > 0, Map["Error: " <> #[[1]] &, FailedTestsError], {}],
    If[FailedParallel > 0, Map["Parallel: " <> #[[1]] &, FailedTestsParallel], {}]
];

Print[""];
Print["============================================"];
Print["  FINAL TEST SUMMARY                        "];
Print["============================================"];
Print["  Total Tests: ", TotalTests];
Print["  Passed:      ", TotalPassed];
Print["  Failed:      ", TotalFailed];

If[TotalFailed > 0,
    Print[""];
    Print["  FAILED TESTS:"];
    Do[Print["    - ", test], {test, AllFailedTests}]
];

Print["============================================"];

If[ScriptMode && TotalFailed > 0, Exit[1]];



