# rivika

## Credits

Rivika is a **reduced** and updated version of [aivika](https://github.com/dsorokin/aivika). This project has the main goal of incresing the understanding of aivika incrementally, using the earlier versions of the code (the earliest version is only available in [hackage](https://hackage.haskell.org/package/aivika-0.1)). Hence, it is necessary to update the code, since the `ghc` compiler changed a lot since back these versions where developed.

## Literate Programming

There are two types of documentation: `thesis.lhs` and `manual.lhs`, both of which use [literate programming](https://en.wikipedia.org/wiki/Literate_programming), given that Haskell has [great support](https://wiki.haskell.org/Literate_programming) to this feature. The former is the graduation thesis of one contributor, and latter is the manual of the software.

### How to Use

The most convenient way to compile the documentation and the executable from these files is by using the script `literate.sh`, inside the `doc` folder. Below, there are the available commands to execute in the terminal:

- `/literate.sh manual compile` - It is necessaty to have installed the `ghc` compiler (must be in your PATH variable). It will compile the file `manual.lhs`.
- `/literate.sh manual repl` - It is necessaty to have installed the `ghci` interactive enironment (must be in your PATH variable). It will start a REPL of the file `manual.lhs`.
- `/literate.sh manual gray` - It is necessary to have installed [lhs2Tex](https://hackage.haskell.org/package/lhs2tex) and pdflatex (both must be in your PATH variable). It will generate a black and white document from `manual.lhs`, following lhs2Tex guidelines.
- `/literate.sh manual colorful` - It is necessary to have installed [pygments](https://pygments.org/download/) (must be in your PATH variable). It will generate a colorful version of the document from `manual.lhs` using the minted package.
- `/literate.sh thesis compile` - It is necessaty to have installed the `ghc` compiler (must be in your PATH variable). It will compile the file `thesis.lhs`.
- `/literate.sh thesis repl` - It is necessaty to have installed the `ghci` interactive enironment (must be in your PATH variable). It will start a REPL of the file `thesis.lhs`.
- `/literate.sh thesis gray` - It is necessary to have installed [lhs2Tex](https://hackage.haskell.org/package/lhs2tex) and pdflatex (both must be in your PATH variable). It will generate a black and white document from `thesis.lhs`, following lhs2Tex guidelines.
- `/literate.sh thesis colorful` - It is necessary to have installed [pygments](https://pygments.org/download/) (must be in your PATH variable). It will generate a colorful version of the document from `thesis.lhs` using the minted package.

**All commands need to be executed inside the doc folder**. All of these commands use the same file, `manual.lhs` or `thesis.lhs`, to execute different things. In this way, documentation and source code come from the same file.
