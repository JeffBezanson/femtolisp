#!/bin/sh

mv flisp.boot flisp.boot.bak

echo "Creating stage 0 boot file..."
../../branches/interpreter/femtolisp/flisp mkboot0.lsp system.lsp compiler.lsp > flisp.boot

echo "Creating stage 1 boot file..."
./flisp mkboot1.lsp

echo "Testing..."
make test
