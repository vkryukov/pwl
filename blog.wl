(* ::Package:: *)

(* ::Subsection:: *)
(*Public interface*)


BeginPackage["Blog`"];


ConvertToMarkdown::usage="ConvertToMarkdown[\!\(\*
StyleBox[\"obj\",\nFontSize->12,\nFontSlant->\"Italic\"]\)] converts notebook represented by notebook object \!\(\*
StyleBox[\"obj\",\nFontSize->12,\nFontSlant->\"Italic\"]\) into a markdown file and writes it to Jekyll directory.";


CopyTextToClipboard::usage="CopyTextToClipboard[obj] extracts all text from notebook represented by notebook object" <>
	" obj and copies it into a clipboard.";


NotebookWordCount::usage="NotebookWordCount[obj] returns number of words (not counting formulas) in notebook represented by notebook object obj.";


NotebookWordCloud::usage="NotebookWordCloud[obj] returns a world cloud of notebook represented by notebook object obj."


(* ::Subsection:: *)
(*Private interface*)


Begin["`Private`"];


nbCells[nb_NotebookObject] := Cases[
	NotebookGet[nb],
	Cell[
		_, 
		"Text"|"Input"|"Output"|"Section"|"Subsection"|"Subsubsection"|
		"Item"|"Subitem"|"Subsubitem"|"ItemNumbered"|"SubitemNumbered"|"SubsubitemNumbered", 
		___],
	Infinity]


(* ::Subsubsection:: *)
(*Text cells*)


(* ::Text:: *)
(*Simple cells without any internal formatting are just converted to strings.*)


processCell[cell:Cell[text_String, type:"Text"|"Section"|"Subsection"|"Subsubsection", ___]] :=
	Switch[type,
		"Section", "# ",
		"Subsection", "## ",
		"Subsubsection", "### ",
		"Text", ""
	] <> text


(* ::Text:: *)
(*For cells with internal formatting, we convert a variety of styles according to Markdown specs.*)


processCell[cell:Cell[TextData[data__], "Text", ___]] := StringJoin[processTextData /@ data]


processTextData[data:StyleBox[text_String, "Input", ___]] := "`{{raw}}" <> text <> "{{endraw}}`"
processTextData[data:StyleBox[text_String, FontSlant->"Italic"]] := "*" <> text <> "* "
processTextData[data:StyleBox[text_String, FontWeight->"Bold"]] := "**" <> text <> "** "
processTextData[data:ButtonBox[text_String, ___, ButtonData ->{___, URL[url_String], ___}, ___]] := 
	"[" <> text <> "](" <> url <> ")"
processTextData[text_String] := text


(* ::Text:: *)
(*For everything that's not captured by the above rules, we issue a warning message.*)


processTextData::unrecognized = "Unsupported text expression `1` will be replaced with --UNPARSED--";
processTextData[other_] := (Message[processTextData::unrecognized, ToString[other]]; "--UNPARSED--")


(* ::Subsubsection:: *)
(*Items numbered and unnumbered*)


processCell[cell:Cell[text_String, "Item"]] := "\n- " <> text <> "\n"
processCell[cell:Cell[text_String, "Subitem"]] := "\n  - " <> text <> "\n"
processCell[cell:Cell[text_String, "Subsubitem"]] := "\n    - " <> text <> "\n"


processCell[cell:Cell[text_String, "ItemNumbered"]] := "\n1. " <> text <> "\n"
processCell[cell:Cell[text_String, "SubitemNumbered"]] := "\n  1. " <> text <> "\n"
processCell[cell:Cell[text_String, "SubsubitemNumbered"]] := "\n    1. " <> text <> "\n"


(* ::Subsubsection:: *)
(*Inputs, outputs and images*)


(* ::Text:: *)
(*We convert all inputs and outputs to a PNG file. We also need to export images (such as screenshots etc.) that we copy pasted directly into notebook; they will typically reside in either Code or Text cell. The three global variables $pageWidth, $imageOutputDir, and $imageNumber will be provided by ConvertToMarkdown.*)


processCell[cell:Cell[data_, tag:"Input"|"Output", ___]] := exportCell[cell, True, tag]
processCell[cell:Cell[BoxData[GraphicsBox[___]], __]] := exportCell[cell, False]

exportCell[cell_, limitPageWidth_?BooleanQ, tag_:""] := Module[
	{imageName = "image" <> IntegerString[$imageNumber] <> ".png"},
	Export[
		FileNameJoin[{$imageOutputDir, imageName}],
		If[limitPageWidth,
			Append[cell, PageWidth->$pageWidth],
			cell]];
	$imageNumber++;
	If[tag != "", "{:."<>tag<>"}\n",""]<>"![" <> imageName <> "](" <> $imagePrefix <> imageName <> ")"]


(* ::Subsubsection:: *)
(*Parsing blog header*)


(* ::Text:: *)
(*We want the publishing of the blog to be accomplished with a single press of a button. That means that ConvertToMarkdown should accept as few arguments as possible, and be able to deduce what it needs to do from the default options and/or notebook content. Therefore, each blog *must* start with a title, followed by the date and optional abstract. getHeader gets a  list of cells in the notebooks and extracts that information from the first few, returning a failure if a proper format is not followed.*)


ConvertToMarkdown::badtitle = "No title found: expected a text styled as section, found `1`";
ConvertToMarkdown::baddate = "No date found: expected a text convertable to date, found `1`";

getHeader[cells_List] := Module[
	{title, date},
	
	(* First cell should be a title, with section style *)
	title = cells[[1]] /. Cell[text_String, "Section"] -> text;
	If[!StringQ[title],
		Message[ConvertToMarkdown::badtitle, ToString[title]];
		Return[$Failed]];
	
	(* Second cell should be a text convertable to a date *)
	date = cells[[2]] /. Cell[text_String, "Text"] -> text;
	If[!StringQ[date],
		Message[ConvertToMarkdown::baddate, ToString[date]];
		Return[$Failed]];
	date = DateObject[date];
	If[!DateObjectQ[date],
		Message[ConvertToMarkdown::baddate, date];
		Return[$Failed]];
		
	{title, DateString[date, "ISODate"]}]


(* ::Subsubsection:: *)
(*Converting to Markdown*)


(* ::Text:: *)
(*In order for Markdown conversion to be a "one button press" operation, we need to provide defaults for two of the required parameters. jekyllDir is taken from a LocalObject with the same name, so that once set by user, the value can survive the Wolfram engine restarts.*)


Options[ConvertToMarkdown] = {
	"jekyllDir" :> Get[LocalObject["jekyllDir"]], (* destination for output files *)
	"width" -> 800                                (* page width while generating images *)
};


(* ::Text:: *)
(*An optional summary can be provided. It is delimited by a Subsubsection called "Summary", which should occur right after the date, and the following (sub..)section. If such section exists, it's content will be inserted as an excerpt on the summary page, otherwise excerpt will only contain the first cell.*)


(* ::Text:: *)
(*The following function returns a place where jekyll delimiter for excerpt (by default, <!--more-->) should be inserted.*)


findExcerpt[cells_List] := Module[
	{pos},
	If[
		MatchQ[cells[[3]], Cell["Summary", "Subsubsection"]],
		(* True - looking for (sub...) section *)
		pos = Position[cells[[4;;]], Cell[_, "Section"|"Subsection"|"Subsubsection"]];
		If[Length[pos] > 0, pos[[1,1]], 1],
		(* False *)
		1]]


(* ::Text:: *)
(*As the output of the conversion, we put resulting markdown in _posts/, all the generated images in it's own folder under assets/, and the compressed original notebook into /assets/notebooks.*)


ConvertToMarkdown[nb_NotebookObject, OptionsPattern[]] := Block[
	{$pageWidth = OptionValue["width"], target = OptionValue["jekyllDir"],
	 $imageNumber=1, $imageOutputDir, $imagePrefix, 
	 cells = nbCells[nb], processedCells,
	 header, title, date, excerpt, wordcloud, source,
	 postName, output},
		 
	target = ExpandFileName[target];
	header = getHeader[cells]; If[FailureQ[header], Return[$Failed]];
	{title, date} = header;
	excerpt = findExcerpt[cells];
	
	
	postName = date <> "-" <> StringReplace[ ToLowerCase[title], Except[LetterCharacter]..->"-"];
	$imageOutputDir = FileNameJoin[{target, "assets", postName}];
	CreateDirectory[$imageOutputDir];
	$imagePrefix = "/assets/" <> postName <>"/";
	wordcloud = FileNameJoin[{"/assets", postName, "wordcloud.png"}];
	source = "/assets/notebooks/" <> postName <> ".nb.gz";
	processedCells = Insert[
		processCell /@ cells[[3;;]],
		"\n\n<!--more-->\n\n", 
		excerpt + 1];
	(* Remove explicit Summary title *)
	If[excerpt > 1, processedCells = Delete[processedCells, 1]];

	output = Join[
		generateFrontMatter[<|
			"layout" -> "post", 
			"title" -> title, 
			"wordcloud" -> wordcloud,
			"source" -> source
			|>],
		processedCells,
		{"[<small>Download this notebook</small>](/assets/notebooks/"<>postName<>".nb.gz)"}];
	Export[
		FileNameJoin[{target, "_posts", postName <> ".markdown"}],
		StringRiffle[output, "\n\n"],
		"Text"];
	Export[
		FileNameJoin[{target, source}],
		nb];
	Export[
		FileNameJoin[{target, wordcloud}],
		NotebookWordCloud[nb]];]


generateFrontMatter[assoc_] := {StringRiffle[
	{
		"---", 
		Splice@KeyValueMap[Function[{key,value}, key <> ": " <> value], assoc], 
		"---", 
		"\n"
	},
	"\n"]}


(* ::Subsubsection:: *)
(*Extracting text*)


CopyTextToClipboard[obj_] := CopyToClipboard[extractText[obj]]
NotebookWordCount[obj_] := WordCount[extractText[obj]]
NotebookWordCloud[obj_, maxItems_Integer:57] := WordCloud[
	ToLowerCase @ extractText[obj],
	WordSelectionFunction -> (And[StringLength[#]>3, StringMatchQ[#, LetterCharacter..]]&),
	ImageSize -> {700,100},
	ColorFunction->ColorData["Pastel"],
	MaxItems -> maxItems]

extractText[obj_] := StringRiffle[
	NotebookImport[obj, "Text"|"Section"|"Subsection"|"Subsubsection"|"Item"|
		"Subitem"|"Subsubitem"|"ItemNumbered"|"SubitemNumbered"|"SubsubitemNumbered"->"Text"],
	"\n\n"]


(* ::Subsection:: *)
(*Epilog*)


End[];


(*Protect[ConvertToMarkdown];*)


EndPackage[];
