#' Show possible wordle words v.2
#'
#' @param word_vec Ordered character vector of guessed words
#' @param color_vec Ordered character vector of colors of guessed words, see [collect_colors()] for format
#' @param master_wordlist Master word list to use. This argument usually omitted.
#'
#' @returns A vector of possible words
#' @examples
#' show_words(c('AROSE', 'UNITY', 'DINGY'),
#'            c('-GG--', '-YY-G', '-YY-G'))
#' @export
show_words <- function(word_vec = character(0),
                        color_vec = character(0),
                        master_wordlist = wordle::words){
  # sanitize inputs
  if (length(word_vec) != length(color_vec)) stop(
    "word_vec and color_vec must be same length")
  words <- master_wordlist
  for (i in seq_along(word_vec)){
    words <- process_word(words, word_vec[i], color_vec[i])
  }
  words
}

#' Process filters for a given word and its colors
#'
#' @param words Vector of words to start with
#' @param word Current word to process
#' @param colors Current colors to process
#' @returns Output words vector
process_word <- function(words, word, colors){
  word <- unlist(strsplit(toupper(word), ''))
  if (length(word) != 5) stop(
    "each element of word_vec must contain 5 characters")
  if (!all(word %in% LETTERS)) stop(
    "word may not contain non-alpha characters")
  colors <- collect_colors(colors)
  words <- filter_greens(words, word, colors)
  words <- filter_yellows(words, word, colors)
  words <- filter_grays(words, word, colors)
  words
}

#' Extract structured vectors from colors string
#'
#' @param colors String of length 5, G = green, Y = yellow, '-' = gray
#' @examples
#' collect_colors('-YY-G')
#' @returns Named list of 3 logical vectors indicating color
#' @export
collect_colors <- function(colors){
  # sanitize input
  colors <- unlist(strsplit(toupper(colors), ''))
  if (length(colors) != 5) stop(
    "each element of color_vec must contain 5 characters")
  if (!all(colors %in% c('G', 'Y', '-'))) stop(
    "color_vec elements may only contain 'G', 'Y', or '-'")
  green <- ifelse(colors == 'G', TRUE, FALSE)
  yellow <- ifelse(colors == 'Y', TRUE, FALSE)
  gray <- ifelse(colors == '-', TRUE, FALSE)
  stopifnot(identical(green + yellow + gray, rep(1L, 5)))
  list(green = green, yellow = yellow, gray = gray)
}

#' Filter word list for green letters
#'
#' @param words Input word vector
#' @param word Word to process
#' @param colors Output from [collect_colors()] for this word
#'
#' @returns Output word vector
filter_greens <- function(words, word, colors){
  green_regex <- paste(ifelse(colors$green, word, '[A-Z]'), collapse = '')
  grep(green_regex, words, value = TRUE)
}

#' Filter word list for yellow letters
#'
#' @param words Input word vector
#' @param word Word to process
#' @param colors Output from [collect_colors()] for this word
#'
#' @returns Output word vector
filter_yellows <- function(words, word, colors){
  yellow_exclude_regex <- paste(
    ifelse(colors$yellow, paste0('[^', word, ']'), '[A-Z]'),
    collapse = '')
  words <- grep(yellow_exclude_regex, words, value = TRUE)
  yellow_include_regex <- paste0("^(?=.*",
                                 paste(
                                   word[colors$yellow],
                                   collapse = ")(?=.*"),
                                 ").+$")
  words <- grep(yellow_include_regex, words, perl = TRUE, value = TRUE)
  words
}

#' Exclude words with gray-only letters, or too many of a letter when known
#'
#' @param words Input word vector
#' @param word Word to process
#' @param colors Output from [collect_colors()] for this word
#'
#' @returns Output word vector
filter_grays <- function(words, word, colors){
  # counts of grays
  gray_f <- sapply(unique(word), function(i) sum(grepl(i, word[colors$gray])))
  # counts of yellows and greens
  yg_f <- sapply(unique(word), function(i){
    sum(grepl(i, word[colors$yellow | colors$green]))
  })
  # frequencies above which to filter out
  ff <- (gray_f + yg_f)[gray_f > 0]
  fl <- names(ff)
  if (length(ff) == 0) return(words)
  re <- sapply(seq_along(ff),
               function(i) paste0(rep(fl[i], ff[i]), collapse = '.*'))
  re <- paste(re, collapse = '|')
  grep(re, words, value = TRUE, invert = TRUE)
}
