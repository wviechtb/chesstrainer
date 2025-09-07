chesstrainer: An R Package to Create and Test Yourself on Chess Lines
=====================================================================

## Description

The main purpose of the `chesstrainer` package is to create sequences of moves (lines), save them, and then test yourself on these sequences. This is especially useful for training openings, but can also be used for creating chess puzzles or training tactics. Can you find the forced mate below?

![](man/figures/screenshot.png "Find the mate in three!")

## Features

- provides an add mode for entering sequences and a test mode to practice sequences
- the trainer tracks your performance in terms of a 'sequence score' for each sequence
- sequences are presented randomly but with higher probabilities for those with poorer performance
- alternatively can let the trainer choose sequences based on play frequency or last play date
- automatically repeat sequences when a wrong move was made (optionally)
- add comments to sequences or particular moves (e.g., to name variations, provide hints, or other information)
- add annotations (arrows and circles) to sequences, which can also be replayed in test mode
- select sequences to practice based on their name, performance, play frequency, last play date, or moves/position
- timed mode for practicing sequences under time pressure
- board editor for setting up advanced starting positions or chess puzzles
- the trainer can be used with multiple players (performance is tracked separately for each player)
- when [Stockfish](https://stockfishchess.org) is installed:
  - automatically obtain and show position evaluations for each move in a sequence
  - show the best move(s) for a given position
  - play mode to play against Stockfish
- adjust colors, text sizes, and various other settings
- manage multiple sequence directories
- toggle the language (English or German)
- plays move and capture sounds
- keeps track of session history

## Installation

The development version of the `chesstrainer` package can be installed with:
```r
install.packages("remotes")
remotes::install_github("wviechtb/chesstrainer")
```

## Play

To start playing, first load the package with `library(chesstrainer)` and then type: `play()`. For an introduction, type: `help(chesstrainer)` or you can read the documentation online [here](https://wviechtb.github.io/chesstrainer/reference/chesstrainer-package.html).

## For Non-R Users

If you are not an R user, but still would like to use the trainer, you will first have to install R, which is freely available from [here](https://cran.r-project.org). Once R is installed, start up R and then enter the commands above for installing the package after the prompt (`>`) on the 'R Console'. If you receive a prompt whether to use/create a personal library, answer 'Yes'. If you are asked for a download location, you can select the first choice. Once the package is installed, type `library(chesstrainer)` to load the package and `play()` to start the trainer.

## Meta

The `chesstrainer` package is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/lgpl-3.0.txt). To report any issues or bugs, please go [here](https://github.com/wviechtb/chesstrainer/issues).
