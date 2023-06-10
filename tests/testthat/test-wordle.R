test_that("show_word works", {
  expect_no_error(result0 <- show_words())
  expect_equal(result0, words)
  result1 <- expect_no_error(
    show_words(
      grays = 'DEUWTVL',
      yellows = c("", "", "I", "", ""),
      greens = 'A-AI-')
    )
  expect_true('AGAIN' %in% result1)
  expect_true(! 'AVAIL' %in% result1)
  expect_length(result1, 4)
  result2 <- show_words(
    grays = 'deuwtvl',
    yellows = c("", "", "i", "", ""),
    greens = 'a-ai-')
  expect_equal(result1, result2)
})

test_that("filter_grays works", {
  expect_error(filter_grays(words, "A,"), 'Non-alpha character in grays')
  expect_no_error(result <- filter_grays(words, grays = ""))
  expect_equal(filter_grays(words), result)
  expect_equal(
    filter_grays(c('ABCDE', 'FFFFF'), 'CG'),
    'FFFFF')
})

test_that("filter_greens works", {
  expect_error(filter_greens(words, 'ABCD'), 'Greens needs to be a string of length 5')
  expect_no_error(filter_greens(words, 'A--I-'))
  expect_no_error(filter_greens(words))
  expect_error(filter_greens(words, "ABCD,"), 'Only letters and hyphen placeholders are allowed in greens')
  expect_equal(
    filter_greens(c('ABCDE', 'ACBDE'), '-B---'),
    'ABCDE')
})

test_that("filter_yellows works", {
  expect_no_error(filter_yellows(words, c("", "", "I", "", "")))
  expect_no_error(filter_yellows(words))
  expect_equal(
    filter_yellows(c('ABCDE', 'BCDEF', 'GGGGA'), c("A", "", "", "", "")),
    'GGGGA')
})
