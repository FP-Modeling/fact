#!/usr/bin/env bash

# Fixing a WSL problem with aliases. Just skip this, linux user
shopt -s expand_aliases
source ~/.bash_aliases

if [[ $1 == 'manual' ]]
then
    if [[ $2 == 'gray' ]]
    then
        # Generate .tex file from .lhs file
        lhs2Tex manual/manual.lhs > manual/manual.tex

        # Generate black and white pdf from created .tex file
        pdflatex -shell-escape -output-directory='manual/' manual/manual.tex

        # Renaming
        mv manual/manual.pdf manualGray.pdf

    elif [[ $2 == 'colorful' ]]
    then
        # Generate colorful pdf from .lhs file
        pdflatex -shell-escape -output-directory='manual/' manual/manual.lhs

        # Renaming
        mv manual/manual.pdf manualColorful.pdf

    elif [[ $2 == 'compile' ]]
    then
        # Compile the manual with GHC
        ghc manual/manual.lhs

	# Moving to current directory (Unix and Windows)
	mv manual/manual manual
	mv manual/manual.exe manual.exe
    elif [[ $2 == 'repl' ]]
    then
        # Enter GHCi with the manual loaded
        ghci -Wdefault manual/manual.lhs

    else
        echo "No available option! Use compile, repl, gray or colorful!"
    fi
elif [[ $1 == 'thesis' ]]
then
    if [[ $2 == 'gray' ]]
    then
        echo "----------------------------"
        echo "Generating intermediate PDF"
        # Generate .tex file from .lhs file
        lhs2Tex MastersThesis/thesis.lhs > MastersThesis/thesis.tex

        echo "----------------------------"
        echo "Generating PDF with pdflatex"
        pdflatex -shell-escape -output-directory='MastersThesis/' MastersThesis/thesis.tex

        # Generate references
        echo "----------------------------"
        echo "Generating references with bibtex"
        cp MastersThesis/bibliography.bib .
        bibtex MastersThesis/thesis
        rm bibliography.bib

        # Generate black and white pdf from created .tex file
        echo "----------------------------"
        echo "Generating PDF with pdflatex"
        pdflatex -shell-escape -output-directory='MastersThesis/' MastersThesis/thesis.tex
        pdflatex -shell-escape -output-directory='MastersThesis/' MastersThesis/thesis.tex

        echo "----------------------------"
        echo "Moving PDF to the parent folder"

        # Renaming and moving
        mv MastersThesis/thesis.pdf thesisGray.pdf

        echo "----------------------------"
        echo "END"

    elif [[ $2 == 'colorful' ]]
    then
        echo "----------------------------"
        echo "Generating PDF with pdflatex"
	if [[ $3 == 'doubled' ]]
        then
	    pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{}\def\doubleSpaced{} \input{MastersThesis/thesis.lhs}"
	else
            pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{} \input{MastersThesis/thesis.lhs}"
	fi

        # Generate references
        echo "----------------------------"
        echo "Generating references with bibtex"
        cp MastersThesis/bibliography.bib .
        bibtex MastersThesis/thesis
        rm bibliography.bib

        # Generate colorful pdf from .lhs file
        echo "----------------------------"
        echo "Generating PDF with pdflatex"
	if [[ $3 == 'doubled' ]]
	then
	    pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{}\def\doubleSpaced{} \input{MastersThesis/thesis.lhs}"
	    pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{}\def\doubleSpaced{} \input{MastersThesis/thesis.lhs}"
	else
            pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{} \input{MastersThesis/thesis.lhs}"
            pdflatex -shell-escape -output-directory='MastersThesis/' "\def\iscolorful{} \input{MastersThesis/thesis.lhs}"
	fi

        echo "----------------------------"
        echo "Moving PDF to the parent folder"

        # Renaming and moving
        mv MastersThesis/thesis.pdf thesisColorful.pdf

        echo "----------------------------"
        echo "END"

    elif [[ $2 == 'compile' ]]
    then
        # Compile the thesis with GHC
        ghc MastersThesis/thesis.lhs

	# Moving to current directory (Unix and Windows)
	mv MastersThesis/thesis thesis
	mv MastersThesis/thesis.exe thesis.exe
    elif [[ $2 == 'repl' ]]
    then
        # Enter GHCi with the thesis loaded
        ghci -Wdefault MastersThesis/thesis.lhs

    else
        echo "No available option! Use compile, repl, gray or colorful!"
    fi
else
    echo "No available documentation! Use manual or thesis!"
fi

