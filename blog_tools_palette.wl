(* ::Package:: *)

buttons = {
	Button["Publish", ConvertToMarkdown[InputNotebook[]], Method -> "Queued"],
	Button["Copy text", CopyTextToClipboard[InputNotebook[]]],
	Button["Word cloud", CreateDocument[NotebookWordCloud[InputNotebook[]]]],
	Button["Count words", MessageDialog["Number of words: " <>
		ToString@NotebookWordCount[InputNotebook[]]]],
	Button["New blog", CreateDocument[{
		Cell["Title", "Section"],
		Cell[DateString[{"MonthNameShort", " ", "Day", ", ", "Year"}], "Text"],
		Cell["Summary", "Subsubsection"],
		Cell["summary text", "Text"],
		Cell["Main", "Subsubsection"],
		Cell["main text", "Text"]	
		},
		StyleDefinitions->"Blog.nb"]]
}


Column[buttons]


Grid[{{
	Item[Style["Practical Wolf", FontFamily->"Helvetica", FontSize->16, FontWeight->Bold]],
	Spacer[1],
	Splice[buttons[[1;;4]]],
	Spacer[100],
	buttons[[5]]
}}]


CellPrint[%]
