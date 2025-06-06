s <- c("sw19", "sw18", "b1", "kt2", "kt12")
x1 <- "some string containing a valid postcode sw19 6ls and some other text"
x2 <- "some string containing a valid outcode sw19 and some other text"
x3 <- "some string with an outcode like chunk m25 that isn't valid"
x4 <- "outcode has kt- 12 a space and punctuation"

test_that("with postcode works", {
  expect_equal(istv::detect_outcode(x1, s), TRUE)
})

test_that("with outcode works", {
  expect_equal(istv::detect_outcode(x2, s), TRUE)
})

test_that("invalid outcode works", {
  expect_equal(istv::detect_outcode(x3, s), FALSE)
})

test_that("with space and punctuation", {
  expect_equal(istv::detect_outcode(x4, s), TRUE)
})

