
# wordle

<!-- badges: start -->
[![R-CMD-check](https://github.com/slager/wordle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/slager/wordle/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/slager/wordle/branch/main/graph/badge.svg)](https://app.codecov.io/gh/slager/wordle?branch=main)
<!-- badges: end -->

This package produces a list of [Wordle](https://www.nytimes.com/games/wordle/index.html) words to guess based on the information contained in your previous guesses.

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
if (!"remotes" %in% rownames(installed.packages())){
  install.packages("remotes")}
remotes::install_github("slager/wordle")
```

## Usage

For typical usage, provide a vector of the words you've guessed so far and a vector of the color indicators for those respective words. (`Y` = yellow, `G` = green, `-` = gray). The function will return possible words remaining.

``` r
library(wordle)
show_words(
  c('AROSE', 'UNITY', 'DINGY'),
  c('-GG--', '-YY-G', '-YY-G'))
#> [1] "IRONY"
```
