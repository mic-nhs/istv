#' match x against the closest match in a place name data frame with columns
#' id and name
#'
#' @param x character a short sentence such as that recorded in the assault
#' location description field
#' @param pndf data frame a place name data frame such as that supplied by istv::pndf
#' @param mcl integer for phonetic encoding as documented in phonics::refinedSoundex
#'
#' @returns all possible candidate matches
#'
#' @export

match_place <- function(x, pndf, mcl = 10) {
  # create a unigram representation of the assault location description string
  # remove all words containing both strings and numbers or just numbers - if not
  # done this screws up soundex
  #
  # possibly consider MRA for better matching
  #

  # the following could be made much more efficient

  x <- tibble::tibble(x = x) %>%
    tidytext::unnest_tokens(.data$token, x, "words") %>%
    dplyr::filter(stringr::str_detect(.data$token, "\\d", negate = TRUE))

  n_string <- NROW(x)

  # the unigrams and their phonetic matches, as well as whether the original
  # token is a word or a stopword (similarly done for place_name_list)

  x_unigram_phon <- x %>%
    dplyr::mutate(phon = phonics::refinedSoundex(
      stringr::str_replace(.data$token, "([a-z]{2,})(s)$", "\\1"),
      mcl,
      clean = FALSE
    )) %>%
    dplyr::mutate(
      n = 1,
      isword = as.numeric(word_check(.data$token)),
      isstop = as.numeric(stopword_check(.data$token))
    )

  # the original phrase as a series of tokens and phonetic matches

  x_phon <- x_unigram_phon %>%
    dplyr::select(phon, token) %>%
    dplyr::summarize(phon = paste0(.data$phon, collapse = " "))

  # and now for efficiency with longer phrases (more important for the
  # place_name_list) remove duplicates

  x_unigram_phon <- x_unigram_phon %>%
    dplyr::distinct()

  # create table of uni-tri grams
  #

  x_bigram_phon <- x_phon %>%
    tidytext::unnest_tokens(.data$phon,
      .data$phon,
      "ngrams",
      n = 2,
      to_lower = FALSE
    ) %>%
    dplyr::mutate(col = stringr::str_split(.data$phon, " ")) %>%
    tidyr::unnest(col) %>%
    dplyr::left_join(x_unigram_phon, join_by(col == phon)) %>%
    dplyr::group_by(.data$phon) %>%
    dplyr::summarize(
      token = paste0(.data$token, collapse = " "),
      isword = sum(.data$isword),
      isstop = sum(.data$isstop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = 2)

  x_trigram_phon <- x_phon %>%
    tidytext::unnest_tokens(.data$phon, .data$phon, "ngrams", n = 3, to_lower = FALSE) %>%
    dplyr::mutate(col = stringr::str_split(.data$phon, " ")) %>%
    tidyr::unnest(col) %>%
    dplyr::left_join(x_unigram_phon, join_by(col == phon)) %>%
    dplyr::group_by(.data$phon) %>%
    dplyr::summarize(
      token = paste0(.data$token, collapse = " "),
      isword = sum(.data$isword),
      isstop = sum(.data$isstop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = 3)

  x_phon_ngrams <- dplyr::bind_rows(x_unigram_phon, x_bigram_phon, x_trigram_phon) %>%
    dplyr::filter(!is.na(.data$phon))

  # duplicate entries with street/saint abbreviations
  # if st at the end of an ngram duplicate it and replace with "street"
  # if st at the start of an ngram duplicate it and replace with "saint"
  # ignore unigrams
  #

  street_entries <- x_phon_ngrams %>%
    dplyr::filter(n > 1, stringr::str_detect(.data$phon, paste0(
      " ", phonics::refinedSoundex("st", mcl), "$"
    ))) %>%
    dplyr::mutate(phon = stringr::str_replace(
      .data$phon,
      paste0(" ", phonics::refinedSoundex("st", mcl), "$"),
      paste0(" ", phonics::refinedSoundex("street", mcl))
    ))

  saint_entries <- x_phon_ngrams %>%
    dplyr::filter(n > 1, stringr::str_detect(.data$phon, paste0(
      "^", phonics::refinedSoundex("st", mcl), " "
    ))) %>%
    dplyr::mutate(phon = stringr::str_replace(
      .data$phon,
      paste0("^", phonics::refinedSoundex("st", mcl), " "),
      paste0(" ", phonics::refinedSoundex("saint", mcl))
    ))

  x_phon_ngrams <- x_phon_ngrams %>%
    dplyr::bind_rows(street_entries, saint_entries)

  # create a map of all the token-phonetic match pairs in the filtered
  # version of the place_name_list

  pndf_phon_map <- pndf %>%
    dplyr::select(name) %>%
    tidytext::unnest_tokens(.data$token, .data$name, "words") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      isword = word_check(.data$token) %>% as.numeric(),
      isstop = stopword_check(.data$token) %>% as.numeric(),
      phon = phonics::refinedSoundex(
        stringr::str_replace(.data$token, "([a-z]{2,})(s)$", "\\1"),
        mcl,
        clean = FALSE
      )
    )

  # place_name_list unigrams

  pndf_unigram_phon <- pndf %>%
    tidytext::unnest_tokens(.data$token, .data$name, "words") %>%
    dplyr::left_join(pndf_phon_map, join_by(token)) %>%
    dplyr::mutate(
      n = 1,
      nword = as.numeric(word_check(.data$token)),
      nstop = as.numeric(stopword_check(.data$token))
    ) %>%
    dplyr::select(
      id,
      token,
      phon,
      n,
      nword,
      nstop
    )

  # place_name_list phonetic match strings

  pndf_phon <- pndf_unigram_phon %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarize(phon = paste0(.data$phon, collapse = " ")) %>%
    dplyr::ungroup()

  # create a table of uni-tri grams for the place_name_list

  pndf_bigram_token <- pndf %>%
    tidytext::unnest_tokens(token, name, "ngrams", n = 2, to_lower = TRUE) %>%
    dplyr::mutate(col = stringr::str_split(.data$token, " ")) %>%
    tidyr::unnest(col) %>%
    dplyr::left_join(pndf_phon_map, join_by(col == token)) %>%
    dplyr::group_by(.data$id, .data$token) %>%
    dplyr::filter(!is.na(.data$token)) %>%
    dplyr::summarize(
      phon = paste0(.data$phon, collapse = " "),
      nword = sum(.data$isword),
      nstop = sum(.data$isstop),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = 2)

  pndf_trigram_token <- pndf %>%
    tidytext::unnest_tokens(.data$token, .data$name, "ngrams", n = 3, to_lower = TRUE) %>%
    dplyr::mutate(col = stringr::str_split(token, " ")) %>%
    tidyr::unnest(col) %>%
    dplyr::left_join(pndf_phon_map, join_by(col == token)) %>%
    dplyr::group_by(.data$id, .data$token) %>%
    dplyr::filter(!is.na(.data$token)) %>%
    dplyr::summarize(
      phon = paste0(.data$phon, collapse = " "),
      nword = sum(.data$isword),
      nstop = sum(.data$isstop),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = 3)

  pndf_phon_ngrams <- dplyr::bind_rows(pndf_unigram_phon, pndf_bigram_token, pndf_trigram_token)


  candidates <- x_phon_ngrams %>%
    dplyr::inner_join(pndf_phon_ngrams, join_by(phon), suffix = c("_x", "_cand")) %>%
    dplyr::left_join(
      pndf %>% dplyr::select(id, name) %>% dplyr::rename(name_cand = name),
      join_by(id)
    )

  candidates <- candidates %>%
    dplyr::mutate(
      len_cand = purrr::map_dbl(
        .data$name_cand,
        ~ stringr::str_extract_all(.x, " ") %>%
          unlist() %>%
          NROW()
      )
    ) %>%
    dplyr::mutate(len_cand = .data$len_cand + 1) %>%
    dplyr::mutate(
      pc_match = .data$n_x / (.data$len_cand),
      token_match = .data$token_x == .data$token_cand,
      plural_match = stringr::str_replace(.data$token_x, "([a-z]{2,})(s)$", "\\1") == stringr::str_replace(.data$token_cand, "([a-z]{2,})(s)$", "\\1"),
      n_string = n_string
    ) %>%
    dplyr::filter(!((.data$n_cand == .data$nword) &
      .data$nword == 1)) %>%
    dplyr::filter(!(.data$nstop == 1 &
      .data$nword == .data$n_cand)) %>%
    dplyr::filter(!(.data$n_cand == .data$nstop)) %>%
    dplyr::filter(!is.na(.data$id))


  return(candidates)
}

#' selects the best possible candidate on a candidate data frame based on several criteria
#'
#' @param candidates data frame such as that returned by select_candidate
#' @param pc_m double
#' @param xy integer vector of length 2, xy coordinates of site attended on BNG
#' @param reject_pcd boolean whether or not to reject postcode matches
#'
#' @details some details to be filled in
#'
#' @returns all possible candidate matches
#'
#' @export

select_candidate <- function(candidates,
                             pc_m = .5,
                             xy = c(531370, 180220),
                             reject_pcd = TRUE) {
  # discard single word dictionary matches

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_x == 1 & .data$isword == 1))

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_x == 1 & !(.data$token_match | .data$plural_match)))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # discard very short entries that aren't token matches
  #

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_x == 1 &
      nchar(.data$token_x) < 3 & !.data$token_match))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # remove candidates substantially longer than the original text

  candidates <- candidates %>%
    dplyr::filter(!(.data$len_cand > (n_string + 2)))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # discard postcode matches if any

  if (reject_pcd) {
    candidates <- candidates %>%
      dplyr::filter(!(.data$class %in% c("pc", "pc2", "pcd")))
  }

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # discard any very short matches

  candidates <- candidates %>%
    dplyr::filter(.data$pc_match >= pc_m)

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  #

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_x == .data$isword & !.data$token_match))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # calculate perfect matches
  # - nstring == ncand
  # - token or plural match
  # - 100% match

  candidates <- candidates %>%
    dplyr::mutate(
      perfect_match = (n_string == .data$len_cand &
        .data$pc_match == 1) &
        (.data$token_match | .data$plural_match)
    )

  if (any(candidates$perfect_match)) {
    candidates <- candidates %>% dplyr::filter(.data$perfect_match)
  }

  # prioritze those with parent-child relationships

  parent_child <- candidates %>%
    dplyr::distinct(.data$id, .data$id_parent)

  candidates <- candidates %>%
    dplyr::mutate(
      parent_child = dplyr::case_when(
        .data$id %in% parent_child$id_parent ~ "parent",
        .data$id_parent %in% parent_child$id ~ "child",
        TRUE ~ "neither"
      )
    )

  # add pc_match of parent when present
  #

  parent_pc <- candidates %>%
    dplyr::filter(.data$parent_child == "parent") %>%
    dplyr::select(id, pc_match) %>%
    dplyr::rename(pc_match_parent = pc_match)

  candidates <- candidates %>%
    dplyr::left_join(parent_pc, join_by(id_parent == id)) %>%
    dplyr::mutate(pc_match_parent = replace_na(.data$pc_match_parent, 0)) %>%
    distinct()

  # keep those with a max pc match or a parent-child relationship

  candidates <- candidates %>%
    dplyr::filter(
      pc_match == max(.$pc_match) |
        parent_child %in% c("parent", "child") |
        pc_match == (
          candidates %>% dplyr::filter(.data$n_x > 1) %>% dplyr::select(pc_match) %>% max0()
        ) &
          .data$n_x > 1
    )

  # keep the longest and the token matches and plural matches

  candidates <- candidates %>%
    dplyr::filter(.data$len_cand == max(.$len_cand) |
      .data$token_match | .data$plural_match)

  # reject parents with a child present

  candidates <- candidates %>% dplyr::filter(.data$parent_child != "parent")

  # if there are candidates that are 100% matches at this stage keep them
  # or keep orgs that are left

  candidates <- candidates %>% dplyr::filter(.data$pc_match == max(.$pc_match) |
    .data$class %in% c("org", "street"))


  # if there are token matches then only keep those

  candidates <- candidates %>% dplyr::filter(
    .data$token_match == max(.$token_match) |
      .data$plural_match == max(.$plural_match)
  )

  # if there are streets and towns left prefer the streets
  #

  if (any(
    candidates$class %in% c(
      "lad",
      "populatedplace",
      "greenspace",
      "town",
      "landform",
      "country"
    )
  ) & any(candidates$class == "street")) {
    cand <- candidates %>% dplyr::filter(.data$class %in% c("street", "org"))

    if (NROW(cand) > 0) {
      candidates <- cand
    }
  }

  # proximity to site

  candidates <- candidates %>%
    dplyr::mutate(proximity = euclidean_dist(.data$easting, .data$northing, xy[1], xy[2]))

  # group
  #

  candidates <- candidates %>%
    group_by(id) %>%
    summarize(
      token_x = head(token_x, 1),
      phon = head(phon, 1),
      n_x = max(n_x),
      isword = max(isword),
      isstop = max(isstop),
      token_cand = head(token_cand, 1),
      n_cand = max(n_cand),
      nstop = max(nstop),
      name_cand = head(name_cand, 1),
      len_cand = max(len_cand, 1),
      pc_match = max(pc_match, 1),
      token_match = max(token_match) == 1,
      plural_match = max(plural_match) == 1,
      n_string = max(n_string),
      name = head(name, 1),
      easting = max(easting),
      northing = max(northing),
      class = head(class, 1),
      proximity = min(proximity)
    ) %>%
    ungroup()

  # if all candidates have the SAME NAME take the nearest
  #

  if (NROW(candidates %>% distinct(name)) == 1) {

    candidates <- candidates %>% slice_min(n = 1, order_by = proximity)

    candidates <- candidates %>%
      dplyr::slice_max(
        tibble::tibble(
          pc_match,
          desc(class),
          n_x,
          token_match,
          plural_match,
          desc(proximity)
        ),
        n = 2
      )

  }

  # if there is a pc_100 match take the longest

  if (all(candidates$pc_match == 1)) {
    candidates <- candidates %>% dplyr::filter(.data$n_x == max(.$n_x))
  }

  # area covered by candidates
  # if it's smaller than 5000m2 (roughly 70*70m, or 10*700m) then just pick the top candidate for the sake of resolving
  # missing coordingates are set to zero of bng - this makes the area naturally
  # big as the zero point is somewhere southwest of cornwall

  if (NROW(candidates) < 3) {
    candidates <- candidates %>%
      add_row(
        easting = candidates$easting[1] + 50,
        northing = candidates$northing[1] + 50
      ) %>%
      add_row(
        easting = candidates$easting[1] - 50,
        northing = candidates$northing[1] - 50
      )
  }

  area <- candidates %>%
    dplyr::mutate(
      easting = replace_na(.data$easting, 0),
      northing = replace_na(.data$northing, 0)
    ) %>%
    sf::st_as_sf(coords = c("easting", "northing")) %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_area()

  if (NROW(candidates) > 0) {
    if (area < 5e3) {
      candidates <- dplyr::slice_head(candidates, n = 1)
    } else if (area < 1e5) {
      cand <- dplyr::slice_head(candidates, n = 1) %>%
        dplyr::mutate(
          name = paste(candidates$name, collapse = ","),
          easting = mean(candidates$easting),
          northing = mean(candidates$northing),
          class = "locality",
          id = paste(candidates$id, collapse = "_")
        )

      candiates <- cand
    } else {
      candidates <- dplyr::slice_min(candidates,
        order_by = .data$proximity,
        n = 5
      )
    }
  }

  candidates <- filter(candidates, !is.na(id))

  return(candidates)
}

#' makes a minimum spec candidate data frame with zero rows
#'
#'
#' @returns zero row data frame with columns id, pc_match
#'
#' @export

make_candidate_df <- function() {
  candidates <- tibble::tibble(id = character(0), pc_match = double(0), proximity = double(0))

  return(candidates)
}

#' determines whether or not an assault location description should be coded
#' to the individuals home address
#'
#' @param txt data frame with a column containing the text to be assessed
#' @param col name of the column containing text to be assessed
#'
#' @returns vector of the same length as txt containing "home" or "other"
#'
#' @export

assign_home <- function(txt, col = ald_clean) {
  # w2v model
  #

  w2v <- fs::path_package("extdata/w2v_model_cbow_8_300.bin", package = "istv") %>%
    word2vec::read.word2vec()

  # document vectors and predictions based on those vectors

  txt <- txt %>% dplyr::mutate(eid = dplyr::row_number())

  doc_vecs <- txt %>%
    dplyr::select(eid, ald_clean) %>%
    dplyr::rename(doc_id = eid, text = ald_clean) %>%
    word2vec::doc2vec(object = w2v) %>%
    tibble::as_tibble(rownames = "eid", .name_repair == "universal") %>%
    dplyr::filter(!is.na(.data$V1), .data$V1 != "")

  prediction_home <-
    tibble::tibble(
      id = doc_vecs$eid,
      home_flag = predict(istv::docs_fit, new_data = doc_vecs) %>% unlist()
    ) %>%
    dplyr::mutate(id = as.numeric(.data$id))

  prediction_home <- txt %>%
    dplyr::left_join(prediction_home, join_by(eid == id)) %>%
    dplyr::select(home_flag) %>%
    unlist()

  return(prediction_home)
}

#' geocodes a data frame of text
#'
#' @param txt data frame containing columns site_code, assault_location_description plus anything else that is useful
#' @param pndf data frame with columns id, name, easting, northing, class, fulladdress, LSOA21CD, if NULL uses the package default
#' @param site_xy data frame with site_code, easting northing of the hospital sites, if NULL uses the package default
#' @param debug boolean, if TRUE the output is much more voluminous and can be used to troubleshoot matching issues
#' @param home_fn function to be used to assign home, returns vector of "home"/"other" values
#'
#' @details extracts all the postcode variants (full, sector, district), flags if home
#'
#' @returns txt with additional columns ...
#'
#' @examples
#' # pndf = NULL
#' # site_xy = NULL
#' # txt <- tibble::tibble(site_code = c("RJ122", "RJ122", "RVR50", "R1H12", "RJ701", "RVR50", "R1H12", "R1H12", "RAL01", "RAL01"),
#' #                       mechanism = "blunt injury",
#' #                       injury_datetime = as_datetime("1999-12-31 23:59:59"),
#' #                       assault_location_description = c("- -", "London Eye, SE1", "Nandos, Epsom, KT19 8AS", "Whitechapel, E1", "sainsbury on wandsworth road sw8", "nandos in epsom town centre near the station, KT19", "on holiday in spain", "na", "on the weekend at home", "ASDA in roehampton"))
#' #
#' # g <- geocode_text(txt)
#'
#' @export

geocode_text <- function(txt,
                         pndf = NULL,
                         site_xy = NULL,
                         home_fn = assign_home,
                         debug = FALSE) {
  if (is_null(site_xy)) {
    site_xy <- istv::site_xy %>% dplyr::select(site_code, easting, northing)
  }

  if (is_null(pndf)) {
    pndf <- istv::pndf
  }

  message("Adding site xy")

  txt <- txt %>%
    dplyr::left_join(site_xy, join_by(site_code))

  # cleaned text

  message("Cleaning text")

  txt <- clean_text(txt)

  # assign home

  txt <- txt %>%
    dplyr::mutate(home_flag = home_fn(txt))

  # now extract postcodes
  #

  # detect a valid outcode: this helps filter the candidates down to speed things
  # up
  #

  message("Detecting outcode")

  outcodes <- pndf %>%
    dplyr::filter(.data$class == "pcd") %>%
    dplyr::distinct(.data$name) %>%
    unlist(use.names = FALSE)

  outcodes <-
    paste0("(^|\\s|\\p{Punct})+(", outcodes, ")(\\s|$||\\p{Punct})+")

  txt <- txt %>%
    dplyr::mutate(contains_outcode = purrr::map_lgl(
      .data$ald_clean,
      ~ detect_outcode(.x,
        s =
          outcodes
      ),
      .progress = FALSE
    ))

  # if an outcode is detected try to extract the outcode and the postcode
  # this requires a valid (i.e. used currently or historically) outcode or
  # postcode - it doesn't just match the regex. It also does quite a lot of work
  # to deal with funky ways of writing postcodes - embedded in words, running
  # into punctuation or a mix of additional whitespace - without confusing with
  # (e.g.) "the incident happened at 10 past 5" pulling t10
  #

  message("Extracting outcode")

  txt <- txt %>%
    dplyr::mutate(
      outcode = map2_chr(
        .data$ald_clean,
        .data$contains_outcode,
        ~ extract_outcode(.x, outcodes, .y),
        .progress = FALSE
      )
    )

  message("Extracting whole postcode")

  txt <- txt %>%
    dplyr::mutate(
      postcode = map2_chr(
        .data$ald_clean,
        .data$outcode,
        ~ extract_whole_postcode(.x, .y),
        .progress = FALSE
      )
    )

  txt <- txt %>%
    dplyr::mutate(pc2 = map2_chr(
      .data$ald_clean,
      .data$outcode,
      ~ extract_whole_postcode(.x, .y, sector = TRUE),
      .progress = FALSE
    ))

  message("Matching placenames")

  txt <- txt %>%
    dplyr::mutate(candidates = list(make_candidate_df()))

  # look for candidates close to the
  #



  pb <- utils::txtProgressBar(
    min = 0,
    max = NROW(txt),
    style = 3,
    width = 20
  )

  for (i in 1:NROW(txt)) {
    if (debug) message("line ", i, " of ", NROW(txt), "\n")

    candidates <- make_candidate_df()

    if (!is.na(txt$outcode[i])) {
      pf <- pndf %>% dplyr::filter(.data$pcd == txt$outcode[i])

      cand <- match_place(txt$ald_clean[i], pf)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
    }

    if (NROW(candidates) == 0) {
      pf <- filter_pndf(txt$easting[i], txt$northing[i], r = 10e3, pndf)
      cand <- match_place(txt$ald_clean[i], pf)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
      # go wider, local at local villages & town centres nearby (within 50km)

      pf <- filter_pndf(txt$easting[i], txt$northing[i], r = 50e3, pndf) %>%
        dplyr::filter(.data$class %in% c("locality", "populatedplace"))

      cand <- match_place(txt$ald_clean[i], pf)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
      # go national, look at towns anywhere in the UK

      pf <- pndf %>%
        dplyr::filter(.data$class %in% c("town", "country", "lad"))

      cand <- match_place(txt$ald_clean[i], pf)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
    }

    if (NROW(candidates) > 0) {
      candidates <- candidates %>%
        dplyr::left_join(istv::pndf, by = join_by(id)) %>%
        dplyr::distinct() %>%
        select_candidate(xy = c(txt$easting[i], txt$northing[i]))
    }

    if (NROW(candidates) == 0) {
      candidates <- make_candidate_df() %>% add_row()
    }

    txt$candidates[[i]] <- candidates
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  # final selection
  # home >
  # postcode >
  # postcode sector >
  # unambiguous match >
  # postcode district >
  #
  #
  #

  txt <- txt %>%
    dplyr::mutate(
      n_cand = purrr::map_int(.data$candidates, ~ .x %>%
        dplyr::filter(!is.na(.data$id)) %>%
        NROW()),
      top_cand = purrr::map_chr(
        .data$candidates,
        ~ .x %>%
          dplyr::select(id) %>%
          dplyr::slice_head(n = 1) %>%
          unlist(use.names = FALSE)
      ),
      match = purrr::map_dbl(
        .data$candidates,
        ~ .x %>%
          dplyr::select(pc_match) %>%
          dplyr::slice_head(n = 1) %>%
          unlist(use.names = FALSE)
      ),
      selected_id = dplyr::case_when(
        home_flag == "home" ~ "home",
        !is.na(.data$postcode) ~ stringr::str_to_upper(.data$postcode),
        !is.na(.data$pc2) ~ paste0("pc2_", stringr::str_to_upper(.data$pc2)),
        n_cand == 1 ~ .data$top_cand,
        match == 1 ~ .data$top_cand,
        !is.na(.data$outcode) ~ paste0("pcd_", stringr::str_to_upper(.data$outcode)),
        n_cand > 1 ~ "ambiguous",
        TRUE ~ "no match"
      )
    ) %>%
    dplyr::left_join(
      istv::pndf %>% dplyr::select(
        id,
        name,
        fulladdress,
        class,
        LSOA21CD
      ),
      join_by(selected_id == id)
    ) %>%
    mutate(LSOA21CD = case_when(selected_id == "home" ~ "home",
                                class == "country" ~ name,
                                is.na(LSOA21CD) ~ NA_character_,
                                TRUE ~ LSOA21CD))

  if (!debug) {
    txt <- txt %>%
      dplyr::select(
        -easting, -northing, -ald_clean, -home_flag,
        -contains_outcode, -match, -outcode, -postcode, -pc2, -n_cand, -top_cand, -candidates
      )
  }

  return(txt)
}
