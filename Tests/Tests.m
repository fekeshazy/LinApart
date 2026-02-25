(* Run these tests as:
 * math -script Tests/Tests.m
 *)

(* TEST COVERAGE SETUP
 *)
Needs["Instrumentation`"];
Needs["Instrumentation`Coverage`"];

(* Define paths *)
testDir = ExpandFileName[FileNameDrop[$InputFileName] /. "" -> "."];
rootDir = FileNameDrop[testDir];
sourceDir = FileNameJoin[{rootDir, "Mathematica"}];
instrumentedSourceDir = FileNameJoin[{rootDir, "Tests", "MMA-Instrumeted"}];
annotatedSourceDir = FileNameJoin[{rootDir, "Tests", "MMA-Annotated"}];

(* Clean up the needed directories *)
If[DirectoryQ[instrumentedSourceDir],
  DeleteDirectory[instrumentedSourceDir, DeleteContents -> True];
];
CreateDirectory[instrumentedSourceDir];
If[DirectoryQ[annotatedSourceDir],
  DeleteDirectory[annotatedSourceDir, DeleteContents -> True]
];
CreateDirectory[annotatedSourceDir];

(* Instrument the code and load it *)
baselineCoverage = CoverageInstrument[sourceDir, instrumentedSourceDir];
Get[FileNameJoin[{instrumentedSourceDir, "LinApart.m"}]];

(* TEST FRAMEWORK SETUP
 *)

(* Print the error message and stop the computation. Exit with
 * an error code if running in a script; raise an exception when
 * in GUI. *)
Error[msg__] := If[Length[Cases[$CommandLine, "-script"]] > 0,
  Print["ERROR: ", msg]; Exit[1];
  ,
  Print[Style["ERROR: ", Red, Bold], msg]; Throw[$Failed];
]

(* Return the list of terms in an expression. Zero is considered
 * to have no terms. *)
Terms[ex_List] := ex // Map[Terms]
Terms[ex_Plus] := List @@ ex
Terms[0] := {}
Terms[ex_] := {ex}

(* Get the maximal rational power of a variable in an expression.
 * Fudge it for expressions where such a concept doesn't exist.
 *)
ClearAll[MaxPowerOf];
MaxPowerOf[ex_, x_Symbol] /; FreeQ[ex, x] := 0
MaxPowerOf[x_, x_Symbol] := 1
(* We'll ignore the imaginary part of the exponents *)
MaxPowerOf[(ex_)^n_, x_Symbol] /; FreeQ[n, x] := MaxPowerOf[ex, x]*Re[n]
(* We'll ignore exponents with x in them *)
MaxPowerOf[(ex_)^n_, x_Symbol] /; Not[FreeQ[n, x]] := 0
MaxPowerOf[items_List, x_Symbol] := items // Map[MaxPowerOf[#, x]&] // Max
MaxPowerOf[items_Plus, x_Symbol] := (List @@ items) // Map[MaxPowerOf[#, x]&] // Max
MaxPowerOf[items_Times, x_Symbol] := (List @@ items) // Map[MaxPowerOf[#, x]&] // Apply[Plus]
(* We'll ignore nested functions *)
MaxPowerOf[(Log|Sin|Cos|Csc|Sec)[_], x_Symbol] := 0

(* Check if LinApart produces an expression equal to the
 * input by value, and properly decomposed into partial
 * fractions.
 *)
TestLinApart[testName_, expr_, opts___] :=
Module[{result, diff, symbols, symbolVals, ok, num, den, enum, eden},
  Print["# ", testName];
  result = LinApart[expr, x, opts];
  symbols = Cases[expr, _Symbol, {0, Infinity}] // Union;
  (* Compare the result with the input at some random point *)
  symbolVals = symbols //
    Map[(# -> RandomInteger[{10000, 99999}])&] //
    Association;
  diff = expr - result /. symbolVals // Together;
  ok = If[MatchQ[diff, _List],
    diff // Flatten // Map[# === 0&] // Apply[And]
    ,
    diff === 0
  ];
  If[Not[TrueQ[ok]],
    Print["Expression:\n  ", expr];
    Print["LinApart[]:\n  ", result];
    Print["Difference at ", symbolVals // Normal, ":\n  ", diff];
    Error["LibApart[] result is not equivalent to the input!"];
  ];
  (* Check if the result is in a partial fraction form *)
  symbolVals = symbols //
    DeleteCases[x] //
    Map[(# -> RandomInteger[{10000, 99999}])&] //
    Association;
  result /. symbolVals //
    Expand //
    Terms //
    Flatten //
    Map[(
      num = Numerator[#];
      den = Denominator[#];
      enum = MaxPowerOf[num, x];
      eden = MaxPowerOf[den, x];
      ok = (eden == 0) || (enum < eden);
      If[Not[TrueQ[ok]],
        Print["Expression:\n  ", expr];
        Print["LinApart[]:\n  ", result];
        Print["Offending term at ", symbolVals // Normal, ":\n  ", #];
        Print["Numerator:\n  ", num];
        Print["Denominator:\n  ", den];
        Print["Numerator exponent: ", enum];
        Print["Denominator exponent: ", eden];
        Error["LibApart[] result is not in a partial fraction form!"];
      ];
    )&];
]

(* TESTS
 *)

(* Run all tests inside CoverageEvaluate to collect coverage *)
{testResults, collectedCoverage} = CoverageEvaluate[

TestLinApart["T001: quad denom int", 1/((x^2+1)*(x+1))];
TestLinApart["T002: quad denom int 2", 1/((x^2+4)*(x+1))];
TestLinApart["T003: two quadratics int", 1/((x^2+1)*(x^2+4))];
TestLinApart["T004: quad+linear int", 1/((x^2+3*x+2)*(x+1))];
TestLinApart["T005: quad+cubic int", 1/((x^2+1)*(x^3-1))];
TestLinApart["T006: quad denom rational", 1/((x^2+1/2)*(x+1))];
TestLinApart["T007: quad denom rational 2", 1/((x^2+1/3)*(x^2+1/4))];
TestLinApart["T008: quad rationalcoeff", 1/((x^2+x/2+1)*(x+1))];
TestLinApart["T009: quad denom symbol", 1/((x^2+a)*(x+1))];
TestLinApart["T010: quad denom two symbols", 1/((x^2+a*b)*(x+c))];
TestLinApart["T011: quad denom sym coeff", 1/((x^2+a*x+b)*(x+1))];
TestLinApart["T012: quad denom three symbols", 1/((x^2+a*x+b)*(x+c*d))];
TestLinApart["T013: quad denom sym rational", 1/((x^2+a/2+1)*(x+b))];
TestLinApart["T014: cubic denom int", 1/((x^3-1)*(x+1))];
TestLinApart["T015: cubic denom int 2", 1/((x^3+1)*(x+1))];
TestLinApart["T016: cubic denom irreducible", 1/((x^3-2)*(x+1))];
TestLinApart["T017: two cubics int", 1/((x^3-1)*(x^3+1))];
TestLinApart["T018: cubic+quad int", 1/((x^3-1)*(x^2+1))];
TestLinApart["T019: cubic denom symbol", 1/((x^3+a)*(x+1))];
TestLinApart["T020: cubic denom sym coeff", 1/((x^3+a*x^2+b*x+c)*(x+1))];
TestLinApart["T021: quartic denom int", 1/((x^4-1)*(x+1))];
TestLinApart["T022: quartic denom irreducible", 1/((x^4+1)*(x+1))];
TestLinApart["T023: quartic denom symbol", 1/((x^4+a)*(x+1))];
TestLinApart["T024: quintic denom", 1/((x^5-1)*(x+1))];
TestLinApart["T025: sextic denom", 1/((x^6-1)*(x+1))];
TestLinApart["T026: mixed degrees int", 1/((x+1)*(x^2+1)*(x^3-1))];
TestLinApart["T027: mixed degrees sym", 1/((x+a)*(x^2+b)*(x^3+c))];
TestLinApart["T028: mixed degrees mixed", 1/((x+1)*(x^2+a)*(x^3+b*c))];
TestLinApart["T029: simple linear", 1/(x+1)/(2*x+3)];
TestLinApart["T030: constant", 1/2];
TestLinApart["T031: polynomial", x^2 + 1];
TestLinApart["T032: single linear factor", 1/(x+1)];
TestLinApart["T033: multiplicity 2 linear", 1/(x+1)^2];
TestLinApart["T034: three linear factors", 1/((x+1)(x+2)(x+3))];
TestLinApart["T035: improper fraction linear", x^3/((x+1)(x+2))];
TestLinApart["T036: multiplicity 5 linear", 1/(x+1)^5];
TestLinApart["T037: symbolic coeff linear", a/((x+1)(x+2))];
TestLinApart["T038: sum of linear fractions", 1/(x+1) + 1/(x+2)];
TestLinApart["T039: negative power linear", x^2/(x+1)^(-2)];
TestLinApart["T040: rational power linear", 1/((x+1)*(x+2)^(1/2))];
TestLinApart["T041: complex coeff linear", 1/((x+I)*(x+2*I))];
TestLinApart["T042: Factor option", 1/(1 - x^2), "Factor" -> True];
TestLinApart["T043: GaussianIntegers",
  1/((1 + x)*(1 + x^2)),
  "Factor" -> True, "GaussianIntegers" -> True];
TestLinApart["T044: Euclidean method linear",
  1/((x+1)*(x+2)*(x+3)),
  "Method" -> "Euclidean"];
TestLinApart["T045: EquationSystem linear",
  1/((x+1)*(x+2)*(x+3)),
  "Method" -> "EquationSystem"];
TestLinApart["T046: PreCollect linear",
  2/((x+1)*(x+2)) + 1/((x+1)*(x+2)),
  "PreCollect" -> True];
TestLinApart["T047: zero denominator", 1/x];
TestLinApart["T048: independent of x", 1/(a+b)];
TestLinApart["T049: multiplicity 10 linear", 1/(x+1)^10];
TestLinApart["T050: mixed multiplicities linear", 1/((x+1)^2*(x+2)^3*(x+3))];
TestLinApart["T051: negative coeff linear", 1/((-x+1)*(x+2))];
TestLinApart["T052: symbolic power linear", 1/((x+1)*(x+2)^a)];
TestLinApart["T053: nested fraction linear", 1/(1+1/(x+1))];
TestLinApart["T054: same root diff form", 1/((x-1)*(2*x-2))];
TestLinApart["T055: large coefficient", 123456789/((x+1)*(x+2))];
TestLinApart["T056: fractional coefficient", 1/2/((x+1)*(x+2))];
TestLinApart["T057: Pi coefficient", Pi/((x+1)*(x+2))];
TestLinApart["T058: high degree num linear", x^10/((x+1)*(x+2))];
TestLinApart["T059: multiple improper linear", x^2/(x+1) + x^3/(x+2)];
TestLinApart["T060: Extension option",
  1/(x^2 - 2),
  "Factor" -> True, "Extension" -> {Sqrt[2]}];
TestLinApart["T061: ApplyAfterPreCollect",
  2/((x+1)*(x+2)) + 3/((x+1)*(x+2)),
  "PreCollect" -> True, "ApplyAfterPreCollect" -> Simplify];
TestLinApart["T062: root of unity", 1/((x-1)*(x+1))];
TestLinApart["T063: imaginary unit", 1/((x-I)*(x+I))];
TestLinApart["T064: zero in numerator term", x/((x+1)*(x+2))];
TestLinApart["T065: nested multivariate fraction", (6*y)/(6+(1+x+2*y+z)^(-2))^2];
TestLinApart["T066: very small coeff", 1/10^10/((x+1)*(x+2))];
TestLinApart["T067: mixed powers linear", x^2/((x+1)*(x+2)^(3/2))];
TestLinApart["T068: negative denom coeff", 1/((-1)*x+1)/((-2)*x+1)];
TestLinApart["T069: three term sum linear", 1/(x+1) + 1/(x+2) + 1/(x+3)];
TestLinApart["T070: product in numerator", (x*(x+1))/((x+2)*(x+3))];
TestLinApart["T071: nested power linear", 1/((x+1)^2*(x+2)^2)];
TestLinApart["T072: 6 linear factors", 1/Product[(x+i), {i, 1, 6}]];
TestLinApart["T073: multi-term with symbols", a/((x+1)*(x+2)) + b/((x+2)*(x+3))];
TestLinApart["T074: Log in numerator", Log[x]/((x+1)*(x+2))];
TestLinApart["T075: Sin in numerator", Sin[x]/((x+1)*(x+2))];
TestLinApart["T076: Exp in numerator", Exp[x]/((x+1)*(x+2))];
TestLinApart["T077: Listable", {1/(x+1), 1/(x+2), 1/(x+3)}];
TestLinApart["T078: double root linear", 1/(x+1)^2/(x+2)];
TestLinApart["T079: four linear factors", 1/((x+1)(x+2)(x+3)(x+4))];
TestLinApart["T080: five linear factors", 1/((x+1)(x+2)(x+3)(x+4)(x+5))];
TestLinApart["T081: powers 1-2-3 linear", 1/((x+1)*(x+2)^2*(x+3)^3)];
TestLinApart["T082: alternating signs", 1/((x-1)(x-2)(x+3)(x+4))];
TestLinApart["T083: large numbers", 1/((x+100)(x+200))];
TestLinApart["T084: negative numbers", 1/((x-5)(x-10))];
TestLinApart["T085: rational numbers in denom", 1/((x+1/2)(x+1/3))];
TestLinApart["T086: mixed types", x/((x+1)(x+2)) + 1/(x+3)];
TestLinApart["T087: coefficient 1", 1*(x+1)/((x+2)(x+3))];
TestLinApart["T088: unity power", 1/(x+1)^1];
TestLinApart["T089: identity", (x+1)/(x+1)];
TestLinApart["T090: cancelable", (x^2-1)/((x-1)*(x+1))];
TestLinApart["T091: duplicate factors", 1/((x+1)^2) + 1/((x+1)^2)];
TestLinApart["T092: seven linear factors", 1/Product[(x+i), {i, 1, 7}]];
TestLinApart["T093: eight linear factors", 1/Product[(x+i), {i, 1, 8}]];
TestLinApart["T094: nine linear factors", 1/Product[(x+i), {i, 1, 9}]];
TestLinApart["T095: ten linear factors", 1/Product[(x+i), {i, 1, 10}]];
TestLinApart["T096: complex product", (a*b)/((x+1)*(x+2))];
TestLinApart["T097: sum in numerator", (x+x^2)/((x+1)*(x+2))];
TestLinApart["T098: constant times fraction", 5*1/((x+1)*(x+2))];
TestLinApart["T099: negative constant", -1/((x+1)*(x+2))];
TestLinApart["T100: fraction of fraction", (1/2)/((x+1)*(x+2))];
TestLinApart["T101: degree 1/1 linear", x/((x+1)*(x+2))];
TestLinApart["T102: degree 2/2 linear", x^2/((x+1)*(x+2))];
TestLinApart["T103: degree 3/2 linear", x^3/((x+1)*(x+2))];
TestLinApart["T104: degree 4/2 linear", x^4/((x+1)*(x+2))];
TestLinApart["T105: degree 5/2 linear", x^5/((x+1)*(x+2))];
TestLinApart["T106: degree 6/2 linear", x^6/((x+1)*(x+2))];
TestLinApart["T107: mult 3 linear", 1/(x+1)^3];
TestLinApart["T108: mult 4 linear", 1/(x+1)^4];
TestLinApart["T109: mult 6 linear", 1/(x+1)^6];
TestLinApart["T110: mult 7 linear", 1/(x+1)^7];
TestLinApart["T111: mult 8 linear", 1/(x+1)^8];
TestLinApart["T112: mult 9 linear", 1/(x+1)^9];
TestLinApart["T113: mult 11 linear", 1/(x+1)^11];
TestLinApart["T114: mult 12 linear", 1/(x+1)^12];
TestLinApart["T115: complex root 1", 1/(x + I)];
TestLinApart["T116: complex root 2", 1/((x + I)*(x - I))];
TestLinApart["T117: complex root 3", 1/((x + I)*(x + 2*I)*(x + 3*I))];
TestLinApart["T118: mixed real/complex", 1/((x + 1)*(x + I))];
TestLinApart["T119: param a", 1/((x + a)*(x + b))];
TestLinApart["T120: param in denom", 1/((x + a[1])*(x + a[2]))];
TestLinApart["T121: multiple params", (a + b)/((x + 1)*(x + 2))];
TestLinApart["T122: param power linear", 1/((x+1)^a*(x+2))];
TestLinApart["T123: Sqrt in numerator", Sqrt[x]/((x+1)(x+2))];
TestLinApart["T124: Sqrt in denominator", 1/((x+1)*Sqrt[x+2])];
TestLinApart["T125: factorable quadratic", 1/(x^2 - 1), "Factor" -> True];
TestLinApart["T126: factorable cubic", 1/(x^3 - 1), "Factor" -> True];
TestLinApart["T127: normalize test 1", 1/(2*x+2)/(x+3)];
TestLinApart["T128: normalize test 2", 1/(3*x+6)/(2*x+4)];
TestLinApart["T129: normalize test 3", 1/(a*x+a)/(x+b)];
TestLinApart["T130: complex power linear", 1/((x+1)*(x+2)^(1+I))];
TestLinApart["T131: rational+complex linear", 1/((x+1)*(x+2)^(1/2+I))];
TestLinApart["T132: negative rational linear", 1/((x+1)*(x+2)^(-1/2))];
TestLinApart["T133: high num quad denom", x^10/((x^2+1)*(x+1))];
TestLinApart["T134: high num cubic denom", x^15/((x^3-1)*(x+1))];
TestLinApart["T135: high num quartic denom", x^20/((x^4-1)*(x+1))];
TestLinApart["T136: high num mixed denom", x^25/((x+1)*(x^2+1)*(x+1))];
TestLinApart["T137: integer times symbol", (2*a)/((x^2+1)*(x+1))];
TestLinApart["T138: rational times symbol", (a/2)/((x^2+1)*(x+1))];
TestLinApart["T139: sum of symbols", (a+b)/((x^2+1)*(x+1))];
TestLinApart["T140: product of symbols", (a*b*c)/((x^2+1)*(x+1))];
TestLinApart["T141: symbol power", (a^2)/((x^2+1)*(x+1))];
TestLinApart["T142: mixed sym num", (a*x+b)/((x^2+1)*(x+1))];
TestLinApart["T143: sym in denom coeff", 1/((a*x+b)*(x+1))];
TestLinApart["T144: sym rational mix", (a/2 + b/3)/((x^2+1)*(x+1))];
TestLinApart["T145: cyclotomic 3", 1/((x^3-1)*(x+1))];
TestLinApart["T146: cyclotomic 4", 1/((x^4-1)*(x+1))];
TestLinApart["T147: cyclotomic 5", 1/((x^5-1)*(x+1))];
TestLinApart["T148: cyclotomic 6", 1/((x^6-1)*(x+1))];
TestLinApart["T149: x^4+1 irreducible", 1/((x^4+1)*(x+1))];
TestLinApart["T150: x^6+1 irreducible", 1/((x^6+1)*(x+1))];
TestLinApart["T151: x^2-x+1", 1/((x^2-x+1)*(x+1))];
TestLinApart["T152: x^2+x+1", 1/((x^2+x+1)*(x+1))];
TestLinApart["T153: two variables", 1/((x+y)*(x+z))];
TestLinApart["T154: three variables", 1/((x+y)*(x+z)*(x+w))];
TestLinApart["T155: vars in poly coeff", 1/((x^2+a*y+b*z)*(x+1))];
TestLinApart["T156: vars everywhere", (a+b)/((x+y)*(x^2+z))];
TestLinApart["T157: x in exponent", 1/((x+1)*(x+2)^x)];
TestLinApart["T158: exp in denominator", 1/((x+1)*Exp[x])];
TestLinApart["T159: sin in denominator", 1/((x+1)*Sin[x])];
TestLinApart["T160: cos in denominator", 1/((x+1)*Cos[x])];
TestLinApart["T161: log in denominator", 1/((x+1)*Log[x])];
TestLinApart["T162: euclidean quad", 1/((x^2+1)*(x+2)), "Method" -> "Euclidean"];
TestLinApart["T163: euclidean cubic", 1/((x^3-1)*(x+1)), "Method" -> "Euclidean"];
TestLinApart["T164: eqsys quad", 1/((x^2+1)*(x+2)), "Method" -> "EquationSystem"];
TestLinApart["T165: eqsys cubic", 1/((x^3-1)*(x+1)), "Method" -> "EquationSystem"];
TestLinApart["T166: precollect quad", a/((x^2+1)*(x+2)) + a/((x^2+1)*(x+3)), "PreCollect" -> True];
TestLinApart["T167: precollect cubic", a/((x^3-1)*(x+2)) + b/((x^3-1)*(x+2)), "PreCollect" -> True];
TestLinApart["T168: factor quad", 1/(x^4-1), "Factor" -> True];
TestLinApart["T169: factor cubic", 1/(x^6-1), "Factor" -> True];
TestLinApart["T170: factor with symbols", 1/(x^2 - a), "Factor" -> True];
TestLinApart["T171: degree 50 numerator", x^50/((x^2+1)*(x+1))];
TestLinApart["T172: degree 100 numerator", x^100/((x^2+1)*(x+1))];
TestLinApart["T173: mult 15 linear", 1/(x+1)^15];
TestLinApart["T174: mult 20 linear", 1/(x+1)^20];
TestLinApart["T175: mult 25 linear", 1/(x+1)^25];
TestLinApart["T176: zero constant", 0];
TestLinApart["T177: monomial in the denominator", 1/x/(x+1)];
TestLinApart["T178: monomial^3 in the denominator", (x-1)/x^3/(x+1)];
TestLinApart["T179: one over denominator", 1/(x+1)];
TestLinApart["T180: nested fraction", 1/(1 + 1/(x+1))];
TestLinApart["T181: triple nested fraction", 1/(1 + 1/(1 + 1/(x+1)))];
TestLinApart["T182: PreCollect + Euclidean quad", 
  a/((x^2+1)*(x+2)) + a/((x^2+1)*(x+3)), 
  "PreCollect"->True, "Method"->"Euclidean"];
TestLinApart["T183: PreCollect + Euclidean cubic", 
  a/((x^3-1)*(x+2)) + b/((x^3-1)*(x+2)), 
  "PreCollect"->True, "Method"->"Euclidean"];
TestLinApart["T184: PreCollect + EquationSystem quad", 
  a/((x^2+1)*(x+2)) + a/((x^2+1)*(x+3)), 
  "PreCollect"->True, "Method"->"EquationSystem"];
TestLinApart["T185: PreCollect + EquationSystem cubic", 
  a/((x^3-1)*(x+2)) + b/((x^3-1)*(x+2)), 
  "PreCollect"->True, "Method"->"EquationSystem"];
TestLinApart["T186: irreducible quadratic no factor", 1/(x^2+x+1), "Factor"->False];
TestLinApart["T187: irreducible quartic no factor", 1/(x^4+1), "Factor"->False];
TestLinApart["T188: irreducible symbolic no factor", 1/(x^2+a*x+b), "Factor"->False];
TestLinApart["T189: polynomial part degree 10", x^10/(x+1), "Method"->"ExtendedLaurentSeries"];
TestLinApart["T190: polynomial part quadratic denom", x^8/(x^2+1), "Method"->"ExtendedLaurentSeries"];
TestLinApart["T191: Extension without Factor", 
  1/(x^2-2), "Factor"->False, "Extension"->{Sqrt[2]}];
TestLinApart["T192: GaussianIntegers without Factor", 
  1/(x^2+1), "Factor"->False, "GaussianIntegers"->True];
TestLinApart["T193: PreCollect + ApplyAfterPreCollect Factor", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Factor];
TestLinApart["T194: PreCollect + ApplyAfterPreCollect Simplify", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Simplify];
TestLinApart["T195: septic denominator", 1/((x^7-1)*(x+1))];
TestLinApart["T196: octic denominator", 1/((x^8-1)*(x+1))];
TestLinApart["T197: mixed irreducible reducible", 1/((x^2+1)*(x^2-1))];
TestLinApart["T198: repeated irreducible quadratic", 1/((x^2+1)^2*(x^2+2)^2)];
TestLinApart["T199: high num cubic denom", x^12/((x^3-1)*(x+1))];
TestLinApart["T200: symbolic irreducible", 1/(x^2+a*x+1), "Factor"->False];
TestLinApart["T201: complex irreducible", 1/((x^2+I*x+1)*(x+1))];
TestLinApart["T202: multi-var PreCollect", 
  a/((x^2+y)*(x+z)) + a/((x^2+y)*(x+w)), 
  "PreCollect"->True];
TestLinApart["T203: polynomial numerator cubic", (x^3+x^2+x+1)/((x+1)*(x+2)*(x+3))];
TestLinApart["T204: mixed int symbol coeffs", 1/((2*x^2+a*x+3)*(x+b))];
TestLinApart["T205: Extension without Factor", 
  1/(x^2-2), "Factor"->False, "Extension"->{Sqrt[2]}];
TestLinApart["T206: GaussianIntegers without Factor", 
  1/(x^2+1), "Factor"->False, "GaussianIntegers"->True];
TestLinApart["T207: PreCollect + ApplyAfterPreCollect Factor", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Factor];
TestLinApart["T208: PreCollect + ApplyAfterPreCollect Simplify", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Simplify];
TestLinApart["T209: septic denominator", 1/((x^7-1)*(x+1))];
TestLinApart["T210: octic denominator", 1/((x^8-1)*(x+1))];
TestLinApart["T211: mixed irreducible reducible", 1/((x^2+1)*(x^2-1))];
TestLinApart["T212: repeated irreducible quadratic", 1/((x^2+1)^2*(x^2+2)^2)];
TestLinApart["T213: high num cubic denom", x^12/((x^3-1)*(x+1))];
TestLinApart["T214: symbolic irreducible", 1/(x^2+a*x+1), "Factor"->False];
TestLinApart["T215: complex irreducible", 1/((x^2+I*x+1)*(x+1))];
TestLinApart["T216: multi-var PreCollect", 
  a/((x^2+y)*(x+z)) + a/((x^2+y)*(x+w)), 
  "PreCollect"->True];
TestLinApart["T217: polynomial numerator cubic", (x^3+x^2+x+1)/((x+1)*(x+2)*(x+3))];
TestLinApart["T218: mixed int symbol coeffs", 1/((2*x^2+a*x+3)*(x+b))];
TestLinApart["T219: Extension without Factor", 
  1/(x^2-2), "Factor"->False, "Extension"->{Sqrt[2]}];
TestLinApart["T220: GaussianIntegers without Factor", 
  1/(x^2+1), "Factor"->False, "GaussianIntegers"->True];
TestLinApart["T221: PreCollect + ApplyAfterPreCollect Factor", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Factor];
TestLinApart["T222: PreCollect + ApplyAfterPreCollect Simplify", 
  a/((x+1)(x+2)) + a/((x+1)(x+3)), 
  "PreCollect"->True, "ApplyAfterPreCollect"->Simplify];
TestLinApart["T223: septic denominator", 1/((x^7-1)*(x+1))];
TestLinApart["T224: octic denominator", 1/((x^8-1)*(x+1))];
TestLinApart["T225: mixed irreducible reducible", 1/((x^2+1)*(x^2-1))];
TestLinApart["T226: repeated irreducible quadratic", 1/((x^2+1)^2*(x^2+2)^2)];
TestLinApart["T227: high num cubic denom", x^12/((x^3-1)*(x+1))];
TestLinApart["T228: symbolic irreducible", 1/(x^2+a*x+1), "Factor"->False];
TestLinApart["T229: complex irreducible", 1/((x^2+I*x+1)*(x+1))];
TestLinApart["T230: multi-var PreCollect",
  a/((x^2+y)*(x+z)) + a/((x^2+y)*(x+w)),
  "PreCollect"->True];
TestLinApart["T231: polynomial numerator cubic", (x^3+x^2+x+1)/((x+1)*(x+2)*(x+3))];
TestLinApart["T232: mixed int symbol coeffs", 1/((2*x^2+a*x+3)*(x+b))];

(* PARALLEL TESTS
 *)

LaunchKernels[2];

TestLinApart["T233: Parallel LaurentSeries 10 factors", 
  1/Product[(x+i),{i,1,10}], 
  "Method"->"ExtendedLaurentSeries", "Parallel"->{True, 2, $TemporaryDirectory <> "/"}];
TestLinApart["T234: Parallel high multiplicity LaurentSeries", 
  1/(x+1)^20, 
  "Method"->"ExtendedLaurentSeries", "Parallel"->{True, 2, $TemporaryDirectory <> "/"}];
TestLinApart["T235: Parallel LaurentSeries quadratic", 
  1/((x^2+1)*(x^2+4)*(x^2+9)*(x^2+16)), 
  "Method"->"ExtendedLaurentSeries", "Parallel"->{True, 2, $TemporaryDirectory <> "/"}];

StopKernels[];

Print["All tests completed!"];

]; (* End of CoverageEvaluate *)

(* COVERAGE REPORT
 *)

Print["Test suite line coverage:"];
Do[
  srcRelPath = StringDrop[path, StringLength[sourceDir] + 1];
  source = ReadString[path] // StringSplit[#, "\n"]&;
  baseline = baselineCoverage[[1,1]][path];
  collected = collectedCoverage[[1,1]][path];
  linesCovered = 0;
  linesTotal = 0;
  annotatedPath = FileNameJoin[{annotatedSourceDir, srcRelPath}];
  stream = OpenWrite[annotatedPath, BinaryFormat->True];
  WR[items__] := {items} // Flatten // Map[BinaryWrite[stream, # // ToString]&];
  Do[
    If[KeyExistsQ[baseline, line],
      hits = Lookup[collected, line, 0];
      If[hits > 0,
        WR[StringPadLeft[ToString[hits], 6, " "], " | ", source[[line]], "\n"];
        linesCovered = linesCovered + 1;
        ,
        WR["###### | ", source[[line]], "\n"];
      ];
      linesTotal = linesTotal + 1;
      ,
      WR["       | ", source[[line]], "\n"];
    ];
    ,
    {line, Length[source]}];
  Close[stream];
  Print["* ", StringDrop[path, StringLength[rootDir] + 1], ": ", linesCovered, " of ", linesTotal];
  Print["  see annotated version at ", StringDrop[annotatedPath, StringLength[rootDir] + 1]];
  ,
  {path, baselineCoverage[[1,1]] // Keys}];
