JORE student record parser
==========================

I suppose you have access to Korppi which is study data system in
University of Jyväskylä.

Open Studies ... Transcript of Records. Copy the block which is
typesetted in monospace to a file. The file should start with
"Jyväskylän yliopisto" and the transcript should be in Finnish.

Run ghci:

$ ghci Testi.hs Records.hs
> jore <- parseJoreFromFile "jopesale.txt"
> sum $ map credits $ snd jore

If you are lucky enough, you should get number of total credits! If
not, congratulations; you have found a bug.
