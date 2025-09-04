#' checks whether a word is a non-dictionary word, where the definition of
#' non-dictionary includes the names of larger populated places, e.g.
#' Barking.
#'
#' @param x character to be checked
#' @param place_names vector of place names that are the same as common words
#'
#' @details note that the definition of words will exclude proper nouns so most
#' place names (e.g. London) will not count as words. Some place names (e.g.
#' Barking) are polysemic which is why we need the place_names vector
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

#' checks if a string contains a phrase from a vector, returns the number of
#' words in that phrase
#'
#' useful to modify the number of dictionary words for names like "mile end"
#' (both of which are dictionary words but together indicate a place)
#'
#' @param x character to be checked
#' @param check_words character vector containing dictionary word place names
#'
#' @details add
#'
#' @returns integer count of the number of words
#'
#' @export

ngram_check <- function(x, check_words = istv::place_names) {

  words <- check_words[
    purrr::map_lgl(check_words, ~stringr::str_detect(x, .x))]

  if (NROW(words) == 0) {

    n <- 0

  } else {

    n <- max(stringr::str_count(words, " ") + 1)

  }

  return(n)

}

#' makes a tibble containing all unigrams and their phonetic mapping based on
#' refined soundex algorithm for any given string
#'
#' @param x character a string containing the phrase to be encoded
#' @param mcl integer maximum code length for refinedSoundex
#'
#' @details the returned tibble includes columns n (1, indicating unigram), nword
#' the number of dictionary words in the token and nstop the number of stopwords
#' in the token (see word_check() and stopword_check()) for the definitions
#'
#' @returns tibble with columns token, phon, n, nword, nstop
#'
#' @export


unigram_tibble <- function(x, mcl = 10) {
  unigram <- tibble::tibble(x = x) %>%
    tidytext::unnest_tokens(.data$token, x, "words") %>%
    dplyr::mutate(phon = phonics::refinedSoundex(.data$token,
                                                 mcl,
                                                 clean = FALSE
    )) %>%
    dplyr::mutate(
      n = 1,
      nword = as.numeric(word_check(.data$token)),
      nstop = as.numeric(stopword_check(.data$token))
    )

  return(unigram)
}

#' helper function for ngram_tibble
#'
#' @param x data frame the unigram tibble
#' @param ngram integer ngram, e.g. 2 for bigrams
#'
#' @details the returned tibble includes columns n the ngram of the token, nword
#' the number of dictionary words in the token and nstop the number of stopwords
#' in the token (see word_check() and stopword_check()) for the definitions
#'
#' @returns tibble with columns token, phon, n, nword, nstop
#'
#' @export

ngram_window <- function(x, ngram) {
  return(tibble::tibble(
    token = paste(x$token, collapse = " "),
    phon = paste(x$phon, collapse = " "),
    n = ngram,
    nword = sum(x$nword),
    nstop = sum(x$nstop)
  ))
}

#' a windowing function that sweeps through a unigram tibble and
#' extracts ngrams
#'
#' @param x data frame the unigram tibble
#' @param ngram integer ngram, e.g. 2 for bigrams
#'
#' @details the returned tibble includes columns n the ngram of the token, nword
#' the number of dictionary words in the token and nstop the number of stopwords
#' in the token (see word_check() and stopword_check()) for the definitions
#'
#' @returns tibble with columns token, phon, n, nword, nstop
#'
#' @export

ngram_tibble <- function(x, ngram = 2, modify_words = NULL) {
  ngram <- runner::runner(x, f = ngram_window, ngram = ngram, k = ngram) %>%
    t() %>%
    tibble::as_tibble(.name_repair = "unique_quiet") %>%
    dplyr::mutate(across(everything(), ~ unlist(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::filter(NROW(stringr::str_split_1(token, " ")) == ngram)

    if (!rlang::is_null(modify_words)) {

      ngram <- ngram %>%
        dplyr::mutate(n_sub = purrr::map_int(token, ~ngram_check(.x, modify_words))) %>%
        dplyr::mutate(nword = nword - n_sub,
               nstop = nstop - n_sub,
               dplyr::across(c(nword, nstop), ~dplyr::if_else(.x<0, 0, .x))) %>%
        dplyr::select(-n_sub)

    }

  return(ngram)
}

#' returns all unigrams to ngram_max tokens from a given phrase plus their
#' phonetic encoding and some statistics
#'
#' @param x character phrase to be encoded
#' @param ngram_max the longest ngram to be extracted
#'
#' @details for the phrase "the seagull is dead" with ngram_max = 2 the tokens
#' returned would be
#'  the
#'  seagull
#'  is
#'  dead
#'  the seagull
#'  seagull is
#'  is dead
#'
#' @returns tibble with columns token, phon, n, nword, nstop
#'
#' @export

allgrams_tibble <- function(x, ngram_max = 3, mcl = 10, modify_words = NULL) {
  ut <- unigram_tibble(x, mcl = mcl)

  ngrams <- purrr::map(2:ngram_max, ~ ngram_tibble(ut, ngram = .x, modify_words = modify_words)) %>%
    dplyr::bind_rows()

  ngrams <- dplyr::bind_rows(ut, ngrams)
ut
  return(ngrams)
}


#' checks if a string is duplicated internally and returns the deduplicated string
#'
#' @param x character which may have duplication
#' @details if a character contains something like "this word this word" will
#' de-duplicate and return "this word". Duplication seems to be quite a common
#' feature of
#'
#' @returns deduplicated character
#'
#' @export

str_dedup <- function(x) {

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

clean_text <- function(d, col = assault_location_description, abbreviations = NULL) {
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
        stringr::str_to_lower() %>%
        replace_abbreviations(abbreviations = abbreviations) %>%
        stringr::str_replace_all("[^[:alnum:]&&[^']]", " ") %>%
        # remove extraneous whitespace
        stringr::str_squish() %>%
        str_dedup()

    )

  return(d)
}
