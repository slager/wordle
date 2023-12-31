% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wordle.R
\name{show_words}
\alias{show_words}
\title{Show possible wordle words v.2}
\usage{
show_words(
  word_vec = character(0),
  color_vec = character(0),
  master_wordlist = wordle::words
)
}
\arguments{
\item{word_vec}{Ordered character vector of guessed words}

\item{color_vec}{Ordered character vector of colors of guessed words, see \code{\link[=collect_colors]{collect_colors()}} for format}

\item{master_wordlist}{Master word list to use. This argument usually omitted.}
}
\value{
A vector of possible words
}
\description{
Show possible wordle words v.2
}
\examples{
show_words(c('AROSE', 'UNITY', 'DINGY'),
           c('-GG--', '-YY-G', '-YY-G'))
}
