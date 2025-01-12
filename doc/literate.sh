#!/usr/bin/env bash

# Fixing a WSL problem with aliases. Just skip this, linux user
shopt -s expand_aliases
source ~/.bash_aliases

if [[ $1 == 'gray' ]]; then
  echo "----------------------------"
  echo "Generating intermediate PDF"
  # Generate .tex file from .lhs file
  lhs2Tex MastersThesis/thesis.lhs >MastersThesis/thesis.tex

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

elif [[ $1 == 'colorful' ]]; then
  echo "----------------------------"
  echo "Generating PDF with pdflatex"
  if [[ $2 == 'doubled' ]]; then
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
  if [[ $2 == 'doubled' ]]; then
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

elif [[ $1 == 'compile' ]]; then
  # Compile the thesis with GHC
  ghc MastersThesis/thesis.lhs

  # Moving to current directory (Unix and Windows)
  mv MastersThesis/thesis thesis
  mv MastersThesis/thesis.exe thesis.exe
elif [[ $1 == 'repl' ]]; then
  # Enter GHCi with the thesis loaded
  ghci -Wdefault MastersThesis/thesis.lhs

else
  echo "No available option! Use compile, repl, gray or colorful!"
fi
