#!/bin/bash

# Fixing a WSL problem with aliases. Just skip this, linux user
shopt -s expand_aliases
source ~/.bash_aliases

if [[ $1 == 'compile' ]] 
then
    # Compile the program
    ghc mivika.lhs
else
    if [[ $1 == 'gray' ]] 
    then
        # Generate .tex file from .lhs file
        lhs2Tex mivika.lhs > mivika.tex

        # Generate black and white pdf from created .tex file
        pdflatex -shell-escape mivika.tex

        # Renaming
        mv mivika.pdf mivikaGray.pdf
    else
        if [[ $1 == 'colorful' ]]
        then
            # Generate colorful pdf from .lhs file
            pdflatex -shell-escape mivika.lhs

            # Renaming
            mv mivika.pdf mivikaColorful.pdf

        else
            echo "No available option! Use compile, gray or colorful!"
        fi
    fi
fi

