# mivika

## Credits

Mivika is a modern and primitive version of [aivika](https://github.com/dsorokin/aivika). This project has the main goal of incresing the understanding of aivika incrementally, using the earlier versions of the code (the earliest version is only available in [hackage](https://hackage.haskell.org/package/aivika-0.1)). Hence, it is necessary to update the code, since the ghc compiler changed a lot since back these versions where developed.

## Literate Programming

There is a copy of the project in a single file, called `mivika.lhs`, making it possible to apply [literate programming](https://en.wikipedia.org/wiki/Literate_programming), given that Haskell has a [great support](https://wiki.haskell.org/Literate_programming) to this feature.

### Requirements

In order to use this functionality, it is necessary to have installed in your system [lhs2Tex](https://hackage.haskell.org/package/lhs2tex) and pdflatex. Two versions of the document will be generated, one using lhs2Tex and pdflatex together, thus building a black and white PDF, and a version using just pdflatex. The latter version will be using package minted, adding color to the code inside the document.

The script `literate.sh` will, given that the PATH variable contains these two programs, do it altogether for you. Windows users will have problems executing the script, so it is recommended to do these commands manually.

Finally, there's an extra function built in the script. If you do `./literate.sh compile` the script will start compilation of the file using `ghc`, thus executing the program contained in the same file used to make documentation.
