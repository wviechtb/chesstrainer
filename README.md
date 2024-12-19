chesstrainer: An R Package to Create and Quiz Yourself on Chess Lines
=====================================================================

## Description

The purpose of the `chesstrainer` package is to create sequences of moves (lines), save them, and then quiz yourself on these sequences. This is especially useful for training openings, but can also be used for creating chess puzzles. Can you find the forced mate below?

![](man/figures/screenshot.png "Find the mate in three!")

## Documentation

You can read the documentation online at [https://wviechtb.github.io/chesstrainer/](https://wviechtb.github.io/chesstrainer/).

## Installation

The development version of the `chesstrainer` package can be installed with:
```r
install.packages("remotes")
remotes::install_github("wviechtb/chesstrainer")
```

## To Do

- [x] Add en passent.
- [ ] Add a board editor.
- [x] Save settings across sessions.
- [ ] Allow adjusting colors.
- [ ] Allow branching in sequences (maybe, as this requires a more substantial update).

## Meta

The `chesstrainer` package is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/lgpl-3.0.txt). To report any issues or bugs, please go [here](https://github.com/wviechtb/chesstrainer/issues).
