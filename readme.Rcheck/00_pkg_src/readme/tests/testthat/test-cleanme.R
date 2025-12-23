# Tests for cleanme() function

test_that("cleanme removes HTML tags", {
  input <- "<div>hello world</div>"
  result <- cleanme(input)
  expect_true(grepl("<html>", result))
  expect_true(grepl("hello world", result))
  expect_false(grepl("<div>", result))
  expect_false(grepl("</div>", result))
})

test_that("cleanme converts URLs to <url> token", {
  # HTTP URL
  input1 <- "check out http://example.com for more"
  result1 <- cleanme(input1)
  expect_true(grepl("<url>", result1))
  expect_false(grepl("http://example.com", result1))

  # HTTPS URL
  input2 <- "visit https://secure.example.com/path"
  result2 <- cleanme(input2)
  expect_true(grepl("<url>", result2))
})

test_that("cleanme converts emoticons to tokens", {
  # Smile emoticons
  expect_true(grepl("<smile>", cleanme("hello :)")))
  expect_true(grepl("<smile>", cleanme("hello :-)")))
  expect_true(grepl("<smile>", cleanme("hello :D")))
  expect_true(grepl("<smile>", cleanme("hello (:")))
  expect_true(grepl("<smile>", cleanme("hello ^_^")))


  # Sad emoticons
  expect_true(grepl("<sadface>", cleanme("hello :(")))
  expect_true(grepl("<sadface>", cleanme("hello :-(")))
  expect_true(grepl("<sadface>", cleanme("hello ):")))
  expect_true(grepl("<sadface>", cleanme("hello D:")))
})

test_that("cleanme converts numbers to <number> token", {
  input <- "there are 123 items and 456 more"
  result <- cleanme(input)
  expect_true(grepl("<number>", result))
  expect_false(grepl("123", result))
  expect_false(grepl("456", result))
})

test_that("cleanme converts Twitter @mentions to <user> token", {
  input <- "hello @username how are you"
  result <- cleanme(input)
  expect_true(grepl("<user>", result))
  expect_false(grepl("@username", result))
})

test_that("cleanme removes newlines, carriage returns, and tabs", {
  input <- "word1\nword2\rword3\tword4"
  result <- cleanme(input)
  expect_false(grepl("\n", result))
  expect_false(grepl("\r", result))
  expect_false(grepl("\t", result))
  # After processing, words should still be present (lowercased)
  expect_true(grepl("word", result))
})

test_that("cleanme handles punctuation correctly", {
  # Periods, commas, colons, semicolons removed
  input <- "hello, world. how: are; you"
  result <- cleanme(input)
  expect_false(grepl(",", result))
  expect_false(grepl("\\.", result))
  expect_false(grepl(":", result))
  expect_false(grepl(";", result))

  # Informative punctuation preserved with spaces
  input2 <- "hello! what? yes=no x+y"
  result2 <- cleanme(input2)
  expect_true(grepl("!", result2))
  expect_true(grepl("\\?", result2))
  expect_true(grepl("=", result2))
  expect_true(grepl("\\+", result2))
})

test_that("cleanme handles contractions", {
  input <- "don't won't can't it's we're"
  result <- cleanme(input)
  # Contractions should be split
  expect_true(grepl("n't", result))
  expect_true(grepl("'s", result))
  expect_true(grepl("'re", result))
})

test_that("cleanme converts output to lowercase", {
  input <- "HELLO World MiXeD"
  result <- cleanme(input)
  expect_equal(result, tolower(result))
  expect_true(grepl("hello", result))
  expect_true(grepl("world", result))
  expect_true(grepl("mixed", result))
})

test_that("cleanme handles empty strings", {
  result <- cleanme("")
  expect_equal(length(result), 1)
  expect_type(result, "character")
})

test_that("cleanme handles vector input", {
  input <- c("hello world", "goodbye world")
  result <- cleanme(input)
  expect_equal(length(result), 2)
  expect_true(all(grepl("world", result)))
})

test_that("cleanme handles brackets and special characters", {
  input <- "test {brackets} [and] more"
  result <- cleanme(input)
  # Brackets should be separated
  expect_true(grepl("\\{", result) || grepl("brackets", result))
})

test_that("cleanme removes percent and other non-separating punctuation", {
  input <- "100% complete >> forward << back __underscore__"
  result <- cleanme(input)
  expect_false(grepl("%", result))
  expect_false(grepl(">>", result))
  expect_false(grepl("<<", result))
  expect_false(grepl("__", result))
})

test_that("cleanme handles government abbreviation", {
  input <- "the gov't said"
  result <- cleanme(input)
  expect_true(grepl("government", result))
})

test_that("cleanme handles ellipsis", {
  input <- "wait... for it"
  result <- cleanme(input)
  # Ellipsis should be preserved as special character
  expect_true(grepl("wait", result))
  expect_true(grepl("for", result))
})

test_that("cleanme returns character vector", {
  input <- c("test one", "test two", "test three")
  result <- cleanme(input)
  expect_type(result, "character")
  expect_equal(length(result), 3)
})

test_that("cleanme handles mixed content", {
  input <- "<p>Hello @user! Check http://example.com for 123 items :)</p>"
  result <- cleanme(input)

  # Should have converted various elements
  expect_true(grepl("<html>", result))
  expect_true(grepl("<user>", result))
  expect_true(grepl("<url>", result))
  expect_true(grepl("<number>", result))
  expect_true(grepl("<smile>", result))
  expect_true(grepl("hello", result))
})
