words_path <- system.file('data-raw', 'words_alpha.txt', package = 'wordle')
words <- suppressWarnings(readLines(words_path))
words <- toupper(words)
words <- words[nchar(words) == 5]

usethis::use_data(words, overwrite = TRUE)
