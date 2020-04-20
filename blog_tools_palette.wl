(* ::Package:: *)

Column[{
	Button["Publish locally", ConvertToMarkdown[InputNotebook[]], Method -> "Queued"],
	Button["Copy text to clipboard", CopyTextToClipboard[InputNotebook[]]],
	Button["Word Cloud", CreateDocument[NotebookWordCloud[InputNotebook[]]]],
	Button["Count words", MessageDialog["Number of words: " <>
		ToString@NotebookWordCount[InputNotebook[]]]]
}]
