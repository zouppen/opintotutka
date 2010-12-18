JORE student record parser
==========================

Installation
------------

This requires Haskell Platform and at least the following package:

- text

This can be fetched on Ubuntu by installing the following package:

- libghc6-text-dev

Compiling can be done with the following command:

$ ghc --make Main.hs

Usage
-----

I suppose you have access to Korppi which is study data system in
University of Jyväskylä.

Open Studies ... Transcript of Records. Copy the block which is
typesetted in monospace to a file. The file should start with
"Jyväskylän yliopisto" and the transcript should be in Finnish.

You can use the parser manually by running ghci:

$ ghci RecordParser.hs Records.hs
> jore <- parseJoreFromFile "my_records.txt"
> sum $ map credits $ snd jore

If you are lucky enough, you should get number of total credits! If
not, congratulations; you have found a bug.

Or you can simply output cumulative student record table:

$ ./Main "my_records.txt"
