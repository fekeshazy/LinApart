(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*ReportTime*)


	(*
	  ReportTime[label, lastTime]
	
	  Simple timing utility for debugging and progress tracking. Prints the 
	  elapsed time since lastTime with a descriptive label, then returns 
	  the current time for chaining.
	
	  Parameters:
	    label    - A string describing what was just completed.
	    lastTime - The AbsoluteTime value from the start of the operation.
	
	  Returns:
	    The current AbsoluteTime, which can be passed to the next ReportTime call.
	
	  Example:
	    startTime = AbsoluteTime[];
	    (* ... do something ... *)
	    t1 = ReportTime["Step 1 completed", startTime];
	    (* ... do something else ... *)
	    t2 = ReportTime["Step 2 completed", t1];
	
	  Output:
	    Step 1 completed: 1.2345 seconds
	    Step 2 completed: 0.5678 seconds
	*)

ClearAll[ReportTime]

ReportTime[args___] := Null /; !CheckArguments[ReportTime[args], 2]

ReportTime[label_, lastTime_] := (
    Message[ReportTime::invalidLabel, label];
    $Failed
) /; !StringQ[label]

ReportTime[label_, lastTime_] := (
    Message[ReportTime::invalidTime, lastTime];
    $Failed
) /; !NumericQ[lastTime]

ReportTime[label_String, lastTime_?NumericQ] := Module[
    {currentTime, elapsed},

    currentTime = AbsoluteTime[];
    elapsed = currentTime - lastTime;
    Print[label, ": ", NumberForm[elapsed, {5, 4}], " seconds"];

    currentTime
]

ReportTime::invalidLabel = "First argument `1` is not a string. Expected a descriptive label.";
ReportTime::invalidTime = "Second argument `1` is not numeric. Expected an AbsoluteTime value.";


(* ::Subsection::Closed:: *)
(*ComputeParallel *)


	(*
	  ComputeParallel[expr, function, numberOfCores, $PATHTMP]
	
	  Applies a function to each element of a list in parallel, using file-based 
	  communication to minimize overhead when transferring large symbolic 
	  expressions between kernels.
	
	  Motivation:
	    Mathematica's built-in parallel functions (ParallelMap, ParallelTable, etc.) 
	    suffer from massive overhead when dealing with large symbolic expressions:
	      - Initialization overhead when distributing expressions to subkernels
	      - Finalization overhead when collecting results back to the main kernel
	    
	    This function works around these issues by:
	      1. Writing each list element to a .mx file before parallel evaluation
	      2. Having subkernels read from and write to disk independently
	      3. Importing results back to the main kernel from disk
	    
	    For large expressions, this disk-based approach is significantly faster 
	    than letting Mathematica handle the inter-kernel communication directly.
	
	  Parameters:
	    expr         - A list of expressions to process.
	    function     - The function to apply to each element (Symbol or Function).
	    numberOfCores - Number of parallel kernels to use (currently informational; 
	                   ParallelDo uses available kernels).
	    $PATHTMP     - Path for temporary files. Can be a RAM disk (e.g., /dev/shm/ 
	                   on Linux) for better performance.
	
	  Returns:
	    A list of results in the same order as the input list.
	
	  Algorithm:
	    1. Create a temporary directory with a random name.
	    2. Export each list element to a separate .mx file.
	    3. Use ParallelDo to process each file:
	       - Import the expression
	       - Apply the function
	       - Export the result back to the same file
	    4. Import all results back to the main kernel.
	    5. Clean up the temporary directory.
	
	  Notes:
	    - Progress is printed to track both export/import and computation stages.
	    - Uses Method -> "FinestGrained" to avoid ParallelDo's load balancing 
	      overhead, which doesn't work well for symbolic computations.
	    - The numberOfCores parameter is not currently used to limit kernels; 
	      it relies on the pre-launched parallel kernels.
	
	  Example:
	    results = ComputeParallel[
	        {expr1, expr2, expr3, expr4},
	        Expand,
	        4,
	        "/dev/shm/"
	    ];
	*)

ClearAll[ComputeParallel]



ComputeParallel[args___] := Null/;!CheckArguments[ComputeParallel[args], 4]

ComputeParallel[expr_, function_, numberOfCores_, $PATHTMP_] := (
    Message[ComputeParallel::invalidList, expr];
    $Failed
) /; !ListQ[expr]

ComputeParallel[expr_, function_, numberOfCores_, $PATHTMP_] := (
    Message[ComputeParallel::invalidCores, numberOfCores];
    $Failed
) /; !IntegerQ[numberOfCores] || numberOfCores < 1

ComputeParallel[expr_, function_, numberOfCores_, $PATHTMP_] := (
    Message[ComputeParallel::invalidPath, $PATHTMP];
    $Failed
) /; !StringQ[$PATHTMP]

ComputeParallel[expr_, function_, numberOfCores_, $PATHTMP_] := (
    Message[ComputeParallel::pathNotFound, $PATHTMP];
    $Failed
) /; !DirectoryQ[$PATHTMP]



ComputeParallel[expr_List, function_, numberOfCores_Integer, $PATHTMP_String] := Module[
    {
        results, tmp, tmpTiming,
        list = expr,
        lengthList = Length[expr],
        tmpFolderName, tmpJobNumber,
        startTime, tmpTime
    },

    (* Generate random folder name to avoid collisions. *)
    tmpFolderName = $PATHTMP <> "tmp" <> ToString[RandomInteger[{1, 10^10}]] <> "/";

    Print["Writing to file starts. Length of the list: " <> ToString[lengthList] <> "."];

    (* If the directory already exists (unlikely), remove it. *)
    If[DirectoryQ[tmpFolderName],
        DeleteDirectory[tmpFolderName, DeleteContents -> True]
    ];
    CreateDirectory[tmpFolderName];

    (* Export each list element to a .mx file. *)
    startTime = AbsoluteTime[];
    Do[
        Print["Writing to file " <> ToString[i] <> "/" <> ToString[lengthList] <> "."];
        tmp = list[[i]];
        DumpSave[tmpFolderName <> "tmp" <> ToString[i] <> ".mx", tmp];,
        {i, 1, lengthList}
    ];
    tmpTime = ReportTime["Exporting is done in", startTime];

    (* Prepare shared variable for progress tracking across kernels. *)
    Clear[tmp, tmpJobNumber];
    tmpJobNumber = lengthList;
    SetSharedVariable[tmpJobNumber];

    (* 
      Distribute computation to subkernels.
      Each kernel imports its assigned file, applies the function, 
      and exports the result back to the same file.
    *)
    SetSharedVariable[tmpFolderName];
    ParallelDo[

        Print[ToString[$KernelID] <> ": Calculation started."];

        (* Import the expression from disk. *)
        Import[tmpFolderName <> "/tmp" <> ToString[i] <> ".mx"];

        (* Apply function with timing. *)
        {tmpTiming, tmp} = tmp // function // AbsoluteTiming;

        Print[
            ToString[$KernelID] <> ": Calculation finished in: " <> 
            ToString[tmpTiming] <> "; remaining jobs: " <> 
            ToString[tmpJobNumber--] <> "."
        ];

        (* Export result back to disk. *)
        DumpSave[tmpFolderName <> "/tmp" <> ToString[i] <> ".mx", tmp];,

        {i, 1, lengthList},
        Method -> "FinestGrained",
        ProgressReporting -> False
    ];

    (* Import results back to main kernel. *)
    Clear[tmp];
    startTime = AbsoluteTime[];

    results = Table[
        Import[tmpFolderName <> "tmp" <> ToString[i] <> ".mx"];
        Print[ToString[i] <> "/" <> ToString[lengthList]];
        tmp,
        {i, 1, lengthList}
    ];

    tmpTime = ReportTime["Importing is done in", startTime];

    (* Clean up temporary directory. *)
    DeleteDirectory[tmpFolderName, DeleteContents -> True];
    tmpTime = ReportTime["Deleting directory is done in", tmpTime];

    results
]

ComputeParallel::invalidList = "First argument `1` is not a list.";
ComputeParallel::invalidCores = "Third argument `1` is not a positive integer. Expected number of cores.";
ComputeParallel::invalidPath = "Fourth argument `1` is not a string. Expected a directory path.";
ComputeParallel::pathNotFound = "Directory `1` does not exist.";
