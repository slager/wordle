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

test_that("filter_greens works", {
  expect_error(filter_greens(words, 'ABCD'), 'Greens needs to be a string of length 5')
  expect_no_error(filter_greens(words, 'A--I-'))
  expect_no_error(filter_greens(words))
  expect_error(filter_greens(words, "ABCD,"), 'Only letters and hyphen placeholders are allowed in greens')
})
