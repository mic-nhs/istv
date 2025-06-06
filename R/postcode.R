#' Detects whether or not a string x contains one of the outcodes listed in
#' the vector s
#'
#' An outcode (also known as postcode district) is the part of a UK postcode
#' before the space, e.g. SW19, EC1A
#'
#' @param x the string to be tested
#' @param s a vector of valid outcodes for the relevant area
#' @details this could be a simple regex, but by comparing against legitimate
#' outcodes properly formatted but invalid outcodes are rejected
#' @returns logical indicating the presence of a valid outcode in x
#'
#' @examples
#' utils::data(pndf)
#' pcd <- pndf |>
#'   dplyr::filter(class == "pcd") |>
#'   dplyr::select(name) |>
#'   unlist(use.names = TRUE)
#' detect_outcode("the outcode is sw1a in this example", pcd)
#'
#' @export

detect_outcode <- function(x, s) {
  # use a loose regex first to speed things up then validate against a vector of
  # postcode districts

  s <- stringr::str_to_lower(s)

  if (is.na(x)) {
    lgl <- FALSE
  } else {
    if (stringr::str_detect(x, "[a-zA-Z]{1,2}[\\s\\p{Punct}]{0,}[0-9]{1,2}")) {

      x_clean <- stringr::str_extract(x, "([a-zA-Z]{1,2})([\\s\\p{Punct}]{0,})([0-9]{1,2})", group = c(1, 3)) %>%
        paste0(collapse = "")
      lgl <- purrr::map_lgl(stringr::regex(s),
                            stringr::str_detect,
                            string = stringr::str_to_lower(x_clean)
      ) %>%
        any(na.rm = TRUE)
    } else {
      lgl <- FALSE
    }
  }

  return(lgl)
}

#' given s, a vector of valid outcodes, extracts the first detected present
#' in x
#'
#' @param x string with a postcode district to be extracted
#' @param s vector of valid postcode districts
#' @param fil remove punctuation in x
#' @details it seems to be faster to use detect than extract rather than
#' attempting to extract on x
#' @returns character outcode (lower case)
#'
#' @examples
#' utils::data(pndf)
#' pcd <- pndf |>
#'   dplyr::filter(class == "pcd") |>
#'   dplyr::select(name) |>
#'   unlist(use.names = TRUE)
#' extract_outcode("the outcode is sw1a in this example", pcd)
#'
#' @export

extract_outcode <- function(x, s, fil = TRUE) {
  s <- stringr::str_to_lower(s)
  x <- stringr::str_to_lower(x)

  # this line probably fixes the issue of middle of postcode punctuation being a problem

  if (fil) {
    x <- x %>% stringr::str_replace_all("\\s?\\p{Punct}{1,}\\s?", "")

    # if an incode is present remove it

    if (stringr::str_detect(x, regex("[0-9][A-Za-z]{2}"))) {
      x <- stringr::str_replace_all(x, regex("[0-9]{1}[A-Za-z]{2}"), " ")
    }

    w <- purrr::map_lgl(
      regex(s),
      str_detect,
      string = x %>% stringr::str_replace("\\W|\\p{punct}", " ") %>% stringr::str_to_lower()
    ) %>%
      which()

    if (NROW(w) > 0) {
      possible_matches <- s[w]

      match <-
        stringr::str_extract(stringr::str_to_lower(x), possible_matches[which.max(nchar(possible_matches))]) %>%
        stringr::str_replace_all("[\\p{Punct}\\s]", "")
    } else {
      match <- NA_character_
    }
  } else {
    match <- NA_character_
  }

  return(match)
}

#' given an outcode extracts the whole or just the postcode sector from x
#' using an existing outcode seems to reduce the possibility of extracting
#' the alphanumeric typos often present in clincial notes
#'
#' @param x character containing a postcode
#' @param outcode character outcode
#' @param sector boolean look for a whole postcode or just postcode sector
#' @details outcode can be extracted from the pndf data frame
#' @returns character postcode (lower case)
#'
#' @examples
#' extract_whole_postcode("the postcode is sw1a 1aa in this example", "sw1a")
#'
#' @export

extract_whole_postcode <- function(x, outcode, sector = FALSE) {
  if (is.na(outcode)) {
    whole_postcode <- NA_character_
  } else {
    whole_postcode <- stringr::str_replace_all(x, "\\s", "") %>%
      stringr::str_to_upper()

    if (sector) {
      whole_postcode <- whole_postcode %>%
        stringr::str_extract(pattern = paste0(stringr::str_to_upper(outcode), "[0-9]"))
    } else {
      whole_postcode <- whole_postcode %>%
        stringr::str_extract(pattern = paste0(stringr::str_to_upper(outcode), "[0-9][A-Za-z]{2}"))
    }

    whole_postcode <- stringr::str_to_lower(whole_postcode)
  }

  return(whole_postcode)
}
