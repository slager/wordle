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
#' @param greens A single string, e.g. `"A--I-"`. Letters are greens and hyphens are non-greens.
#' @returns A character vector of filtered words
filter_greens <- function(words, greens = "-----"){
  greens <- toupper(greens)
  if (nchar(greens) != 5) stop('Greens needs to be a string of length 5')
  if (!grepl('^[A-Z-]+$', greens)) stop('Only letters and hyphen placeholders are allowed in greens')
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
#' @param yellows A character vector of length 5. Each element is a string of yellows at that position. If no yellows at a position, string should be "".
#' @returns A character vector of filtered words
filter_yellows <- function(words, yellows = rep("", 5)){
  yellows <- toupper(yellows)
  if (length(yellows) != 5) stop("Yellows must have length 5")
  if (!all(grepl('^[A-Z]*$', yellows))) stop("Non-letter found in yellows")
  # Yellow excludes filter
  yellow_exclude_regex <-
    sapply(yellows, function(i){
      if (identical(i, '')){
        '[A-Z]'
      } else {
        paste0('[^', paste(i, collapse = ''), ']')
      }
    })
  yellow_exclude_regex <- paste(yellow_exclude_regex, collapse = '')
  words <- grep(yellow_exclude_regex, words, value = TRUE)
  # Yellow includes filter
  yellow_includes <- lapply(yellows, function(x){
    if (identical(x, "")){
      character(0)
    } else {
      strsplit(x, "")[[1]]
    }
  })
  yellow_includes <- Reduce(c, yellow_includes)
  yellow_include_regex <- paste0("^(?=.*", paste(yellow_includes, collapse = ")(?=.*"), ").+$")
  grep(yellow_include_regex, words, perl = TRUE, value = TRUE)
}

#' Show possible wordle words
#'
#' @param grays grays, see [filter_grays()]
#' @param yellows yellows, see [filter_yellows()]
#' @param greens greens, see [filter_greens()]
#' @returns A vector of possible words
#' @export
show_words <- function(grays = "", yellows = rep("", 5), greens = "-----"){
  # Grays filter
  words <- filter_grays(wordle::words, grays)
  # Greens filter
  words <- filter_greens(words, greens)
  # Yellows filter
  filter_yellows(words, yellows)
}
