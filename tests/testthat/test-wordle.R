test_that("show_word works", {
  result <- expect_no_error(
    show_words(
      grays = 'DEUWTVL',
      yellows = list('',
                     '',
                     'I',
                     '',
                     ''),
      greens = 'A-AI-')
    )
  expect_true('AGAIN' %in% result)
  expect_true(! 'AVAIL' %in% result)
  expect_length(result, 4)
})

test_that("show_word() is-insensitive", {
  result1 <- show_words(
    grays = 'DEUWTVL',
    yellows = list('',
                   '',
                   'I',
                   '',
                   ''),
    greens = 'A-AI-')
  result2 <- show_words(
    grays = 'deuwtvl',
    yellows = list('',
                   '',
                   'i',
                   '',
                   ''),
    greens = 'a-ai-')
  expect_equal(result1, result2)
})

test_that("filter_grays works", {
  expect_error(filter_grays(words, "A,"), 'Non-alpha character in grays')
  # blank grays vector
  expect_no_error(result <- filter_grays(words, grays = ""))
  # default argument
  expect_equal(filter_grays(words), result)
})
