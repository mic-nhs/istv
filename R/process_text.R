#' checks whether a word is a non-dictionary word, where the definition of
#' non-dictionary includes the names of larger populated places, e.g.
#' Barking.
#'
#' @param x character to be checked
#' @param place_names vector of place names that are the same as common words
#'
#' @returns boolean whether x is a word
#'
#' @export

word_check <- function(x, place_names = NULL) {
  #
  #

  if (is.null(place_names)) {
    place_names <- istv::place_names
  }

  place_names <- place_names %>%
    stringr::str_to_lower()

  x <- x %>% stringr::str_to_lower()

  lgl <- (hunspell::hunspell_check(x) |
            hunspell::hunspell_check(x, dict = "en_GB")) &
    !(x %in% place_names)

  return(lgl)
}

#' checks if a string is duplicated internally
#'
#' @param x character which may have duplication
#' @details if a character contains something like "this word this word" will
#' de-duplicate and return "this word". Duplication seems to be quite a common
#' feature of
#'
#' @returns deduplicated character
#'
#' @export

is_str_dup <- function(x) {
  str_mid <- round(nchar(x) / 2)

  s1 <- stringr::str_sub(x, start = 1, end = str_mid) %>% stringr::str_squish()
  s2 <- stringr::str_sub(x, start = str_mid + 1, end = 1e6) %>% stringr::str_squish()

  s_out <- dplyr::if_else(s1 == s2, s1, x)

  return(s_out)
}

#' checks if x is a stopword
#'
#' @param x a character, preferably a single word
#' @param lexicon a character as documented in tidytext::stop_words
#'
#' @returns boolean
#'
#' @export

stopword_check <- function(x, lexicon = "onix") {
  x <- x %>%
    stringr::str_replace_all("[^[:alpha:]^\\s]", "") %>%
    stringr::str_squish()

  sw <- tidytext::stop_words %>%
    dplyr::filter(lexicon == lexicon) %>%
    dplyr::select(word) %>%
    unlist() %>%
    stringr::str_to_lower()

  check <- x %in% sw

  return(check)
}

#' replaces abbreviations using a table of regex and the replacement
#'
#' @param x character containing abbreviations to be replaced
#' @param abbreviations tibble with columns regex and replacement giving regex
#' for abbreviations and the relevant replacement, if none is supplied
#' utils::data((abbreviations) is used)
#'
#' @returns character containing x with abbreviations replaced
#'
#' @export

replace_abbreviations <- function(x, abbreviations = NULL) {
  if (is.null(abbreviations)) {
    abbreviations <- istv::abbreviations
  }

  for (i in 1:NROW(abbreviations)) {
    x <- x %>% stringr::str_replace_all(abbreviations$regex[i], abbreviations$replacement[i])
  }

  x <- x %>%
    stringr::str_replace_all("\\ss($|\\s)", "s\\1")

  return(x)
}

#' standard text cleaning
#'
#' @param d data frame containing text to be cleaned
#' @param col column name returning text to be cleaned
#' @details text cleaning removes common abbreviations, punctuation
#'
#' @returns d data frame with cleaned text in column ald_clean
#'
#' @export

clean_text <- function(d, col = assault_location_description) {
  # clean text
  # - replace common abbreviations
  # - remove punctuation
  #

  d <- d %>%
    dplyr::mutate(
      ald_clean = {{ col }} %>%
        # various abbreviations for patient retaining the terminal s (but
        # disregarding any possessive apostrophe, there are virtually no cases
        # where patient should be plural and it is often written so)
        #
        stringr::str_to_lower() %>%
        replace_abbreviations() %>%
        stringr::str_replace_all("[^[:alnum:]&&[^']]", " ") %>%
        # remove extraneous whitespace

        stringr::str_squish() # %>%

      # is_str_dup()
    )

  return(d)
}
