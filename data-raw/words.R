words <- readLines('https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt')
words <- toupper(words)
words <- words[nchar(words) == 5]

usethis::use_data(words, overwrite = TRUE)
