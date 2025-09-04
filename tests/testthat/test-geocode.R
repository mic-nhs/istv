
txt <- tibble::tibble(
  site_code = c("RVR50"),
  mechanism = "blunt injury",
  injury_datetime = lubridate::as_datetime("1999-12-31 23:59:59"),
  assault_location_description = c(
    "outside nandos at the top epsom high street kt19"
  )
)

g <- tibble::tibble(
             site_code                   ="RVR50",
             mechanism                   ="blunt injury",
             injury_datetime             = lubridate::as_datetime("1999-12-31 23:59:59"),
             assault_location_description="outside nandos at the top epsom high street kt19",
             selected_id                 ="o_200004011656",
             name                        ="nandos",
             fulladdress                 ="40, THE OAKS SQUARE, WATERLOO ROAD, EPSOM, KT19 8AS",
             class                       ="org",
             LSOA21CD                    ="E01034290")


test_that("basic function works", {
  expect_equal(geocode_text(txt), g)
})

# txt$assault_location_description[1] <- "there won't be a match from this"
#
# test_that("nomatch works", {
#   expect_equal(geocode_text(txt), g)
# })
