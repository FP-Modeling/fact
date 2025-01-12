# FACT

FACT is a domain specific language to model continuous-time systems. 
For motivation and in-depth presentation of the main ideas, please refer to 

- Edil Medeiros, Eduardo Peixoto, Eduardo Lemos, [*FACT: A Domain-Specific Language Based on a Functional Algebra for Continuous Time Modeling*](https://doi.org/10.1109/WSC60868.2023.10408703), 2023 Winter Simulation Conference.

## How to use

It mandatory to have [cabal](https://cabal.readthedocs.io/en/3.4/getting-started.html) installed. From the root of the project, the following command starts the REPL environment:

`cabal repl`

Moreover, the project can be compiled and executed using the following command (the `main` function is redirecting to running Lorenz Attractor benchmarks):

`cabal run`

All the examples in the `Examples` folder are acessible via the REPL environment.

## Literate Programming

There are two types of documentation: `thesis.lhs`, both of which use [literate programming](https://en.wikipedia.org/wiki/Literate_programming), given that Haskell has [great support](https://wiki.haskell.org/Literate_programming) to this feature.

### Dependencies

- `ghc` - minimum version `9.6.6`.
- `pdflatex` - minimum version `3.141592653-2.6-1.40.25`.
- `bibtex` - minimum version `0.99d`.

The library `lhs2Tex` is necessary specifically to run the `gray` subcommand (check next subsection).

### How to Use

The most convenient way to compile the documentation and the executable from these files is by using the script `literate.sh`, inside the `doc` folder. Below, there are the available commands to execute in the terminal:

- `/literate.sh compile` - It is necessary to have installed the `ghc` compiler (must be in your PATH variable). It will compile the file `thesis.lhs`.
- `/literate.sh repl` - It is necessary to have installed the `ghci` interactive enironment (must be in your PATH variable). It will start a REPL of the file `thesis.lhs`.
- `/literate.sh gray` - It is necessary to have installed [lhs2Tex](https://hackage.haskell.org/package/lhs2tex) and pdflatex (both must be in your PATH variable). It will generate a black and white document from `thesis.lhs`, following lhs2Tex guidelines.
- `/literate.sh colorful` - It is necessary to have installed [pygments](https://pygments.org/download/) (must be in your PATH variable). It will generate a colorful version of the document from `thesis.lhs` using the minted package.

**All commands need to be executed inside the doc folder**. All of these commands use the same file, `thesis.lhs`, to execute different things. In this way, documentation and source code come from the same file.

### Shortcut

As a shortcut, the colorful version of the thesis can be compiled using the following sequence of commands in a linux machine (Ubuntu) from the root folder:

``` 
sudo apt-get install texlive-latex-base
sudo apt-get install texlive-fonts-extra
cd doc
./literate.sh colorful
```

## Credits

FACT (Functional Algebra for Continuous Time) is a **reduced** and updated version of [aivika](https://github.com/dsorokin/aivika). This project has the main goal of incresing the understanding of aivika incrementally, using the earlier versions of the code (the earliest version is only available in [hackage](https://hackage.haskell.org/package/aivika-0.1)). Hence, it is necessary to update the code, since the `ghc` compiler changed a lot since back these versions where developed.
