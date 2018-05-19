#!/bin/sh

cp test/stack_template.tpl test/Main.hs
alex src/Scan.x -o tmp.hs
cat tmp.hs >> test/Main.hs
rm tmp.hs
stack test/Main.hs < examples/GarageDoor.sgf
rm test/Main.hs
