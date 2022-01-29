#!/bin/bash

# Fixing a WSL problem with aliases. Just skip this, linux user
shopt -s expand_aliases
source ~/.bash_aliases

# Generate .tex file from .lhs file
lhs2Tex mivika.lhs > mivika.tex

# Generate black and white pdf from the created .tex file
pdflatex -shell-escape mivika.tex
mv mivika.pdf mivikaGray.pdf

# Generate colorful pdf from the .lhs file
pdflatex -shell-escape mivika.lhs
mv mivika.pdf mivikaColor.pdf

#Check whether they are equal
if [[ $1 == 'compile' ]]
then
    ghc mivika.lhs
fi

