s <- c("sw19", "sw18", "b1", "kt2", "kt12", "kt21")
x1 <- "some string containing a valid postcode sw19 6ls and some other text"
x2 <- "some string containing a valid upper case outcode SW19 and some other text"
x3 <- "some string with an outcode like chunk m25 that isn't valid"
x4 <- "outcode has kt- 12 a space and punctuation"
x5 <- "some string withacompressedpostcodekt211bt"

test_that("with postcode works", {
  expect_equal(extract_outcode(x1, s), "sw19")
})

test_that("with outcode works", {
  expect_equal(extract_outcode(x2, s), "sw19")
})

test_that("invalid outcode works", {
  expect_equal(extract_outcode(x3, s), NA_character_)
})

test_that("with space and punctuation", {
  expect_equal(extract_outcode(x4, s, fil = TRUE), "kt12")
})

test_that("with space and punctuation but fil false", {
  expect_equal(extract_outcode(x4, s, fil = FALSE), NA_character_)
})

test_that("with no spaces and an incode", {
  expect_equal(extract_outcode(x5, s, fil = TRUE), "kt21")
})
