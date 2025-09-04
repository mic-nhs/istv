
x1 <- "some string containing a valid postcode sw19 6ls and some other text"
x2 <- "some string containing a valid upper case outcode sw19 and some other text"
x3 <- "some string with an outcode like chunk m25 that isn't valid"
x4 <- "postcode has sw19-6ls punctuation"
x5 <- "some string withacompressedpostcodekt211bt"

test_that("simple case of well formatted postcode", {
  expect_equal(extract_whole_postcode(x1, "sw19"), "sw196ls")
})

test_that("with outcode only works", {
  expect_equal(extract_whole_postcode(x2, "sw19"), NA_character_)
})

test_that("invalid outcode works", {
  expect_equal(extract_whole_postcode(x3, "sw"), NA_character_)
})

test_that("with space and punctuation", {
  expect_equal(extract_whole_postcode(x4, "sw19"), "sw196ls")
})

test_that("with space and punctuation but fil false", {
  expect_equal(extract_whole_postcode(x4, "kt21"), NA_character_)
})

test_that("with no spaces", {
  expect_equal(extract_whole_postcode(x5, "kt21"), "kt211bt")
})
