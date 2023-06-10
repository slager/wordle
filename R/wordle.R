#' Filter word list to exclude those with gray letters
#'
#' @param words A character vector of words to filter.
#' @param grays A string containing gray wordle letters, e.g. `"ABOPD"`
#' @returns A character vector of filtered words
filter_grays <- function(words, grays = ""){
  grays <- toupper(grays)
  if (nchar(grays) == 0) return(words)
  if (!grepl("^[A-Z]+$", grays)) stop('Non-alpha character in grays')
  grays_regex <- paste0('^[^', grays, ']+$')
  grep(grays_regex, words, value = TRUE)
}

#' Filter word list to only include those that match pattern in greens
#'
#' @param words A character vector of words to filter.
#' @param greens A single string, e.g. `"A--I-"`. Greens are letters, and hyphens are placeholders for non-greens.
#' @returns A character vector of filtered words
filter_greens <- function(words, greens){
  greens <- toupper(greens)
  green_vec <- strsplit(greens, "")[[1]]
  green_regex <- paste(
    sapply(green_vec,
           function(i){
             ifelse(i == "-",
                    "[A-Z]",
                    i)}),
    collapse = "")
  grep(green_regex, words, value = TRUE)
}

#' Filter word list to those consistent with yellows
#'
#' @param words A character vector of words to filter.
#' @param yellows A list of length 5. Each element is a character vector of yellows at that position.
#' @returns A character vector of filtered words
filter_yellows <- function(words, yellows){
  yellows <- lapply(yellows, toupper)
  # Yellow excludes filter
  yellow_exclude_regex <-
    lapply(yellows, function(i){
      if (identical(i, '')){
        '[A-Z]'
      } else {
        paste0('[^', paste(i, collapse = ''), ']')
      }
    })
  yellow_exclude_regex <- paste(unlist(yellow_exclude_regex), collapse = '')
  words <- grep(yellow_exclude_regex, words, value = TRUE)
  # Yellow includes filter
  yellow_includes <- lapply(yellows, function(x) if (identical(x, '')) character(0) else x)
  yellow_includes <- Reduce(c, yellow_includes)
  yellow_include_regex <- paste0("^(?=.*", paste(yellow_includes, collapse = ")(?=.*"), ").+$")
  grep(yellow_include_regex, words, perl = TRUE, value = TRUE)
}

show_words <- function(grays = "", yellows, greens = "-----"){
  # Grays filter
  words <- filter_grays(wordle::words, grays)
  # Greens filter
  words <- filter_greens(words, greens)
  # Yellows filter
  filter_yellows(words, yellows)
}
