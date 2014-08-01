Format
======

Utilities for building structured output.

The module `postscript.ml` helps create postscript documents that we've used for on-paper grading in the past.
Functions here are very specific.

The module `spreadsheet.ml` is more generic, and helps build spreadsheets.
Commands `cms`, `diff`, and `harness` use it.
The functor `Make` takes basic information about a sheet, like how to represent and print rows, and helps organize a sheet.
In particular it keeps rows sorted and can parse a spreadsheet file.

The one big problem with spreadsheets is that all the data needs to be kept in memory.
