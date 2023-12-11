test_that("show_words works", {
  expect_no_error(result2 <- show_words())
  expect_equal(result2, words)
  expect_no_error(result22 <- show_words(
    c('AROSE', 'AERIE', 'ALURE'), c('GY--G', 'G-Y-G', 'G-GGG')))
  expect_true('AZURE' %in% result22)
  expect_no_error(result23 <- show_words(
    c('AROSE', 'UNITY', 'DINGY'), c('-GG--', '-YY-G', '-YY-G')))
  expect_equal('IRONY', result23)
  # double letter with 1 green and 1 gray works
  expect_true('BEGET' %in% show_words('GIGAS','--G--'))
  # double letter with 1 yellow and 1 gray works
  expect_true('BEGET' %in% show_words('TITLE','Y---Y'))
})

test_that("errors as expected", {
  # collect_colors()
  expect_error(
    show_words('AROSE', 'GGA--'),
    "color_vec elements may only contain 'G', 'Y', or '-'"
    )
  expect_error(
    show_words('AROSE', 'GA--'),
    "each element of color_vec must contain 5 characters"
  )
  # show_words()
  expect_error(
    show_words(c('AROSE', 'ALFLF'), 'GGA--'),
    "word_vec and color_vec must be same length"
    )
  # process_word()
  expect_error(
    show_words('AR=SE', 'GG---'),
    "word may not contain non-alpha characters"
    )
  expect_error(
    show_words('AOSE', 'GGA--'),
    "each element of word_vec must contain 5 characters"
  )
})

test_that("filter_grays works", {
  # filters nothing when no gray letters in word
  expect_equal(
    wordle:::filter_grays(
      wordle::words, 'AROSE', wordle:::collect_colors('YYYYY')), words)
})
