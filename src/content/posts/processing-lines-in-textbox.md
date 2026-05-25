---
title: "Processing lines in a TextBox"
date: "2005-07-05"
description: "C# code to ingest all TextBox content."
author: "Greg Bacon"
tags:
 - "C#"
---

Below is a simple example of processing all lines in a TextBox.

```csharp
public static void WriteFile(string path, string contents)
{
  using (StreamWriter w = new StreamWriter(path))
    w.Write(contents);
}

public static void LineByLine(TextReader reader)
{
  Console.WriteLine("input type = " + reader.GetType());

  int n = 1;
  string line;
  while ((line = reader.ReadLine()) != null)
    Console.WriteLine(n++ + ". [" + line + "]");
}

static void Main(string[] args)
{
  string text =
    "The quick\n" +
    "brown fox jumped over\n" +
    "the lazy dog.";

  string path = "foo.txt";
  WriteFile(path, text);
  using (StreamReader streamr = new StreamReader(path))
    LineByLine(streamr);

  TextBox textbox = new TextBox();
  textbox.Text = text;
  using (StringReader stringr = new StringReader(textbox.Text))
    LineByLine(stringr);
}
```