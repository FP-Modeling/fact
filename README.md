# mivika

## Credits

Mivika is a modern and primitive version of [aivika](https://github.com/dsorokin/aivika). This project has the main goal of incresing the understanding of aivika incrementally, using the earlier versions of the code (the earliest version is only available in [hackage](https://hackage.haskell.org/package/aivika-0.1)). Hence, it is necessary to update the code, since the `ghc` compiler changed a lot since back these versions where developed.

## Literate Programming

There is a copy of the project in a single file, called `mivika.lhs`, making it possible to apply [literate programming](https://en.wikipedia.org/wiki/Literate_programming), given that Haskell has [great support](https://wiki.haskell.org/Literate_programming) to this feature.

### How to Use

The script `literate.sh` is responsable to create documents and compile the fie. Below there are the available commands to execute in the terminal.

- `/literate.sh compile` - It is necessaty to have installed the `ghc` compiler (must be in your PATH variable). It will compile the file `mivika.lhs`.
- `/literate.sh gray` - It is necessary to have installed [lhs2Tex](https://hackage.haskell.org/package/lhs2tex) and pdflatex (both must be in your PATH variable). It will generate a black and white document, following lhs2Tex guidelines.
- `/literate.sh colorful` - It is necessary to have installed [pygments](https://pygments.org/download/) (must be in your PATH variable). It will generate a colorful version of the document using the minted package.

All of these commands use the same file, `mivika.lhs`, to execute different things. In this way documentation and source code come from the same file.
