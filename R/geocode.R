#' match x against the closest match in a place name data frame with columns
#' id and name
#'
#' @param x character a short sentence such as that recorded in the assault
#' location description field
#' @param pndf data frame a place name data frame such as that supplied by istv::pndf
#' @param mcl integer for phonetic encoding as documented in phonics::refinedSoundex
#' @param precalc_tokens data frame if supplied contains previously calculated tokens
#' and phonetic encoding for pndf. If NULL will calculate.
#'
#' @details supplying precalc_tokens will make this quite a bit quicker
#'
#' @returns all possible candidate matches
#'
#' @export

match_place <- function(x, pndf, mcl = 10, precalc_tokens = NULL, ngram_max = 3, modify_words = NULL) {
  # create a unigram representation of the assault location description string
  # remove all words containing both strings and numbers or just numbers - if not
  # done this screws up soundex
  #
  # possibly consider MRA for better matching
  #

  # len_target         number of words in target string
  # len_cand           number of words in candidate string
  # n                  indicates unigram, bigram, trigram
  # nword              indicates the number of dictionary words in the ngram
  # nstop              indictates the number of stop words in the ngram
  # _cand suffix       indicates candidate (i.e. pulled from place name list)
  # _target suffix     indicates target (i.e. pulled from the string to be matched)
  # token_match        percentage of the words in the target that are token rather than phon
  #                    matches for the candidate
  # plural_match       indicates that the target and candidate match on token plural - especially
  #                    useful for thinks like Sainsbury(s)
  # pc_match_cand      the length of the target ngram over the total length of the candidate
  # pc_match_target    the length of the candidate ngram over the total length of the target, i.e. if this exceeds 1 then the candidate is longer than the target
  #

  # deal with zero length or all whitespace strings

  zerolen <- stringr::str_replace_all(x, "\\s", "")

  if (zerolen == "") {return(make_candidate_df())}

  # the following could be made much more efficient

  target <- allgrams_tibble(x, mcl = mcl, ngram_max = ngram_max, modify_words = modify_words) %>%
    dplyr::mutate(len_target = dplyr::if_else(n == 1, 1, 0) %>% sum())

  # duplicate entries with street/saint abbreviations
  # if st at the end of an ngram duplicate it and replace with "street"
  # if st at the start of an ngram duplicate it and replace with "saint"
  # ignore unigrams
  #

  street_entries <- target %>%
    dplyr::filter(n > 1, stringr::str_detect(.data$phon, paste0(
      " ", phonics::refinedSoundex("st", mcl), "$"
    ))) %>%
    dplyr::mutate(phon = stringr::str_replace(
      .data$phon,
      paste0(" ", phonics::refinedSoundex("st", mcl), "$"),
      paste0(" ", phonics::refinedSoundex("street", mcl))
    ))

  saint_entries <- target %>%
    dplyr::filter(n > 1, stringr::str_detect(.data$phon, paste0(
      "^", phonics::refinedSoundex("st", mcl), " "
    ))) %>%
    dplyr::mutate(phon = stringr::str_replace(
      .data$phon,
      paste0("^", phonics::refinedSoundex("st", mcl), " "),
      paste0(" ", phonics::refinedSoundex("saint", mcl))
    ))

  target <- target %>%
    dplyr::bind_rows(street_entries, saint_entries)

  # create a map of all the token-phonetic match pairs in the filtered
  # version of the place_name_list

  if (is.null(precalc_tokens)) {
    cands <- pndf %>%
      precalculate_pndf_tokens(ngram_max = ngram_max)
  } else {
    cands <- pndf %>% dplyr::left_join(precalc_tokens, dplyr::join_by(id),
                                relationship = "many-to-many")
  }

  if (NROW(cands) == 0) {
    return(make_candidate_df())
  }

  candidates <- target %>%
    dplyr::inner_join(cands, dplyr::join_by(phon),
      suffix = c("_target", "_cand"),
      relationship = "many-to-many"
    )

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  candidates <- candidates %>%
    dplyr::mutate(
      len_cand = purrr::map_dbl(
        .data$name,
        ~ stringr::str_extract_all(.x, " ") %>%
          unlist() %>%
          NROW()
      )
    ) %>%
    dplyr::mutate(len_cand = .data$len_cand + 1) %>%
    dplyr::mutate(
      pc_match_cand = .data$n_target / (.data$len_cand),
      # token_match = purrr::map2_int(.data$token_target, .data$token_cand,
      #   ~sum(str_split(.x, " ", simplify = TRUE) == str_split(.x, " ", simplify = TRUE))),
      # token_match = .data$token_match / n_cand,
      plural_match = stringr::str_replace(.data$token_target, "([a-z]{2,})(s)$", "\\1") == stringr::str_replace(.data$token_cand, "([a-z]{2,})(s)$", "\\1"),
      pc_match_target = .data$len_cand / .data$len_target,
      tmp1 = token_target %>% stringr::str_replace_all("\\d", " ") %>% stringr::str_squish() %>% stringr::str_split(pattern = " "),
      tmp2 = token_cand %>% stringr::str_replace_all("\\d", " ") %>% stringr::str_squish() %>% stringr::str_split(pattern = " "),
      token_match = purrr::map2_int(tmp1, tmp2, ~ NROW(which((.x) == (.y)))) / n_cand
    ) %>%
    #select(-contains("tmp")) %>%
    dplyr::filter(!((.data$n_cand == .data$nword_target) &
      .data$nword_target == 1)) %>%
    dplyr::filter(!(.data$nstop_target == 1 &
      .data$nword_target == .data$n_cand)) %>%
    dplyr::filter(!(.data$n_cand == .data$nstop_cand)) %>%
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
  # discard any candidates substantially longer than the target
  #

  candidates <- candidates %>%
    dplyr::filter(!(.data$pc_match_target > 1.2))

  # discard single word dictionary matches

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_target == 1 & .data$nword_target == 1))

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_target == 1 & !(.data$token_match | .data$plural_match)))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # discard very short entries that aren't token matches
  #

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_target == 1 &
      nchar(.data$token_target) < 3 & !.data$token_match))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # remove candidates substantially longer than the original text

  candidates <- candidates %>%
    dplyr::filter(!(.data$len_cand > (len_target + 2)))

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
    dplyr::filter(.data$pc_match_cand >= pc_m)

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  #

  candidates <- candidates %>%
    dplyr::filter(!(.data$n_target == .data$nword_target & !.data$token_match))

  if (NROW(candidates) == 0) {
    return(make_candidate_df())
  }

  # calculate perfect matches
  # - nstring == ncand
  # - token or plural match
  # - 100% match

  candidates <- candidates %>%
    dplyr::mutate(
      perfect_match = (len_target == .data$len_cand &
        .data$pc_match_cand == 1) &
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
    dplyr::select(id, pc_match_cand) %>%
    dplyr::rename(pc_match_parent = pc_match_cand)

  candidates <- candidates %>%
    dplyr::left_join(parent_pc, dplyr::join_by(id_parent == id)) %>%
    dplyr::mutate(pc_match_parent = tidyr::replace_na(.data$pc_match_parent, 0)) %>%
    dplyr::distinct()

  # keep those with a max pc match or a parent-child relationship
  #

  candidates <- candidates %>%
    dplyr::filter(
      pc_match_cand == max(.$pc_match_cand) |
        pc_match_target == max(.$pc_match_target) |
        parent_child %in% c("parent", "child") |
        pc_match_cand == (
          candidates %>% dplyr::filter(.data$n_target > 1) %>% dplyr::select(pc_match_cand) %>% max0()
        ) &
          .data$n_target > 1
    )

  # discard those where stopwords more than 50% of a match
  #

  candidates <- candidates %>%
    dplyr::filter(!(nstop_target > (0.5 * n_target)))

  # prefer the lowest frequency matches
  #

  #candidates <- candidates %>% dplyr::slice_min(order_by = f_min, n=3)

  candidates %>% dplyr::arrange(token_match, dplyr::desc(f_min)) %>% dplyr::slice_head(n = 5)

  # keep the longest and the token matches and plural matches

  candidates <- candidates %>%
    dplyr::filter(.data$len_cand == max(.$len_cand) |
      .data$token_match | .data$plural_match)

  # reject parents with a child present
  # THIS IS TOO SIMPLISTIC

  #candidates <- candidates %>% dplyr::filter(.data$parent_child != "parent")

  # if there are candidates that are 100% matches at this stage keep them
  # or keep orgs that are left

  candidates <- candidates %>% dplyr::filter(.data$pc_match_cand == max(.$pc_match_cand) |
    .data$class %in% c("org", "street"))

  # if there are token matches then only keep those

  candidates <- candidates %>% dplyr::filter(
    .data$token_match == max(.$token_match) |
      .data$plural_match == max(.$plural_match)
  )

  # if there are streets and towns left prefer the streets providing they have as
  # high a match as any towns
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

    max_pc_ladplus <- candidates %>% dplyr::filter(!(.data$class %in% c("street", "org"))) %>%
      dplyr::select(pc_match_cand) %>% max() %>% unlist()

    cand <- candidates %>% dplyr::filter(.data$class %in% c("street", "org"), pc_match_cand == max_pc_ladplus)

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
    dplyr::group_by(id) %>%
    dplyr::summarize(
      token_target = head(token_target, 1),
      phon = head(phon, 1),
      n_target = max(n_target),
      nword_target = max(nword_target),
      nstop_target = max(nstop_target),
      token_cand = head(token_cand, 1),
      n_cand = max(n_cand),
      nword_cand = max(nword_cand),
      nstop_cand = max(nstop_cand),
      name = head(name, 1),
      len_cand = max(len_cand, 1),
      pc_match_cand = max(pc_match_cand, 1),
      pc_match_target = max(pc_match_target, 1),
      token_match = max(token_match) == 1,
      plural_match = max(plural_match) == 1,
      len_target = max(len_target),
      name = head(name, 1),
      easting = max(easting),
      northing = max(northing),
      class = head(class, 1),
      proximity = min(proximity),
      f_min = min(f_min)
    ) %>%
    dplyr::ungroup()

  # if all candidates have the SAME NAME take the nearest
  #

  if (NROW(candidates %>% dplyr::distinct(name)) == 1) {
    candidates <- candidates %>% dplyr::slice_min(n = 1, order_by = proximity)

    candidates <- candidates %>%
      dplyr::slice_max(
        tibble::tibble(
          pc_match_cand,
          dplyr::desc(class),
          n_target,
          dplyr::desc(token_match),
          plural_match,
          dplyr::desc(proximity)
        ),
        n = 2
      )
  }

  # if there is n_cand == 1 and n_target == 1 but len_cand > 1 then remove >1

  if (all(candidates$n_cand == 1) & all(candidates$n_target == 1)) {
    candidates <- candidates %>% dplyr::filter(.data$len_cand == 1)
  }

  # if there is a pc_100 match take the longest

  if (all(candidates$pc_match_cand == 1)) {
    candidates <- candidates %>% dplyr::filter(.data$n_target == max(.$n_target))
  }

  # area covered by candidates
  # if it's smaller than 5000m2 (roughly 70*70m, or 10*700m) then just pick the top candidate for the sake of resolving
  # missing coordingates are set to zero of bng - this makes the area naturally
  # big as the zero point is somewhere southwest of cornwall

  if (NROW(candidates) < 3) {
    candidates <- candidates %>%
      tibble::add_row(
        easting = candidates$easting[1] + 50,
        northing = candidates$northing[1] + 50
      ) %>%
      tibble::add_row(
        easting = candidates$easting[1] - 50,
        northing = candidates$northing[1] - 50
      )
  }

  area <- candidates %>%
    dplyr::mutate(
      easting = tidyr::replace_na(.data$easting, 0),
      northing = tidyr::replace_na(.data$northing, 0)
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

  candidates <- dplyr::filter(candidates, !is.na(id))

  return(candidates)
}

#' makes a minimum spec candidate data frame with zero rows
#'
#'
#' @returns zero row data frame with columns id, pc_match
#'
#' @export

make_candidate_df <- function() {
  candidates <- tibble::tibble(id = character(0), pc_match_cand = double(0), proximity = double(0))

  return(candidates)
}

#' determines whether or not an assault location description should be coded
#' to the individuals home address - uses a very simple process of matching a
#' few key phrases without relying on any externally trained model
#'
#' @param txt data frame with a column containing the text to be assessed
#' @param col name of the column containing text to be assessed
#'
#' @returns vector of the same length as txt containing "home" or "other"
#'
#' @export

assign_home_simple <- function(txt, col = ald_clean) {

  home_phrases <- c("^home$",
                    "^.{0,10}[^\\s]home[$\\s].{0,10}$",
                    "(patient['s]{1,2}\\s*|own)\\s(home|house|residence|flat|appartment|address)(\\s|$)",
                    "home address",
                    "at home")

  txt <- txt %>%
    dplyr::rowwise() %>%
    dplyr::mutate(home_flag = purrr::map_lgl(home_phrases,
                                      ~stringr::str_detect({{col}}, .x)) %>%
                                        any(na.rm = TRUE) %>%
                    dplyr::if_else("home", "other"))

  return(txt)

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

assign_home <- function(txt, col = ald_clean,
                        w2v_file = "w2v_model_cbow_8_300.bin") {

  # w2v model
  #

  w2v <- w2v_file %>%
    word2vec::read.word2vec()

  # document vectors and predictions based on those vectors

  doc_vecs <- txt %>%
    dplyr::mutate(doc_id = dplyr::row_number(),
                  ald_clean = dplyr::if_else(ald_clean == "" | is.na(ald_clean), "no text", ald_clean)) %>%
    dplyr::rename(text = ald_clean) %>%
    dplyr::select(doc_id, text) %>%
    word2vec::doc2vec(object = w2v) %>%
    tibble::as_tibble(rownames = "doc_id", .name_repair == "check_unique") %>%
    dplyr::rename(eid = doc_id) %>%
    dplyr::mutate(across(where(is.numeric), ~as.numeric(.x))) %>%
    dplyr::mutate(across(where(is.numeric), ~dplyr::if_else(is.na(.x), 0, .x)))

  prediction_home <-
     tibble::tibble(
       id = rownames(doc_vecs),
       home_flag = brulee:::predict.brulee_logistic_reg(docs_fit, new_data = doc_vecs) %>% unlist()
     ) %>%
     dplyr::mutate(id = as.numeric(.data$id)) %>%
    dplyr::mutate(home_flag = tidyr::replace_na(home_flag, "other")) %>%
    dplyr::select(home_flag) %>%
    unlist()


  return(prediction_home)
}

#' geocodes a data frame of text
#'
#' @param txt data frame containing columns site_code, assault_location_description plus anything else that is useful
#' @param pndf data frame with columns id, name, easting, northing, class, fulladdress, LSOA21CD, if NULL uses the package default
#' @param precalc_tokens data frame with precalculated tokens and phonetic encouding for the pndf
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
                         precalc_tokens = NULL,
                         site_xy = NULL,
                         home_fn = NULL,
                         modify_words = NULL,
                         debug = FALSE,
                         ... #named arguments to be passed to home_fn
                         ) {
  if (rlang::is_null(site_xy)) {
    site_xy <- istv::site_xy %>% dplyr::select(site_code, easting, northing)
  }

  if (rlang::is_null(pndf)) {
    pndf <- istv::pndf
  }

  if (rlang::is_null(precalc_tokens)) {
    precalc_tokens <- istv::precalc_tokens
  }

  if (rlang::is_null(home_fn)) {
    home_fn <- istv::assign_home
  }

  if (rlang::is_null(modify_words)) {
    modify_words <- istv::place_names
  }

  message("Adding site xy")

  txt <- txt %>%
    dplyr::left_join(site_xy, dplyr::join_by(site_code))

  # cleaned text

  message("Cleaning text")

  txt <- clean_text(txt) %>%
    mutate(ald_clean = replace_na(ald_clean, "ZZZ no text"))

  # assign home

  txt <- txt %>%
    dplyr::mutate(home_flag = home_fn(txt, ...))

  # now extract postcodes
  #

  # detect a valid outcode: this helps filter the candidates down to speed things
  # up
  #

  message("Detecting outcode")

  outcodes <- pndf %>%
    dplyr::distinct(.data$pcd) %>%
    unlist(use.names = FALSE)

  # outcodes <-
  #   paste0("(^|\\s|\\p{Punct})+(", outcodes, ")(\\s|$||\\p{Punct})+")

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


  txt <- txt %>%
    dplyr::mutate(
      outcode = purrr::map2_chr(
        .data$ald_clean,
        .data$contains_outcode,
        ~ extract_outcode(.x, outcodes, .y),
        .progress = FALSE
      )
    )

  message("Extracting whole postcode")

  txt <- txt %>%
    dplyr::mutate(
      postcode = purrr::map2_chr(
        .data$ald_clean,
        .data$outcode,
        ~ extract_whole_postcode(.x, .y),
        .progress = FALSE
      )
    )

  txt <- txt %>%
    dplyr::mutate(pc2 = purrr::map2_chr(
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

    candidates <- cand <- town_candidates <- make_candidate_df()


    if (!is.na(txt$outcode[i])) {

      pf <- pndf %>% dplyr::filter(.data$pcd == txt$outcode[i])

      cand <- match_place(txt$ald_clean[i], pf, precalc_tokens = precalc_tokens, modify_words = modify_words)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
    }

    if (NROW(candidates) == 0) {

      # firstly find out if there is a location name in the string
      #

      town_candidates <- make_candidate_df()

      pf_town <- istv::pndf %>%
        dplyr::filter(class %in% c("town", "LAD")) %>%
        filter_pndf(x = txt$easting[i], y=txt$northing[i], r=25e3)

      if (NROW(pf_town) > 0) {

      placename <- match_place(txt$ald_clean[i],
                               pndf = pf_town,
                               precalc_tokens = istv::precalc_tokens,
                               modify_words = modify_words)

        if (NROW(placename) > 0) {

        pf <- purrr::pmap(placename %>% dplyr::select(easting, northing), function(easting, northing) {filter_pndf(easting, northing, r = 2e3)}) %>%
          dplyr::bind_rows() %>%
          dplyr::filter(!(name %in% placename$name))

        cand <- match_place(txt$ald_clean[i], pf, precalc_tokens = precalc_tokens,
                            modify_words = modify_words)

          if (NROW(cand) > 0) {
            cand <- cand %>%
              dplyr::distinct() %>%
              select_candidate(xy = c(placename$easting[1], placename$northing[1], r = 10e3), reject_pcd = TRUE)
            town_candidates <- dplyr::bind_rows(town_candidates, cand)
          }

        }

      }

      pf <- filter_pndf(txt$easting[i], txt$northing[i], r = 10e3, pndf)

      cand <- match_place(txt$ald_clean[i], pf, precalc_tokens = precalc_tokens,
                          modify_words = modify_words)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
      # go wider, local at local villages & town centres nearby (within 100km)

      pf <- filter_pndf(txt$easting[i], txt$northing[i], r = 100e3, pndf) %>%
        dplyr::filter(.data$class %in% c("locality", "populatedplace"))

      cand <- match_place(txt$ald_clean[i], pf, precalc_tokens = precalc_tokens,
                          modify_words = modify_words)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
      # go national, look at towns anywhere in the UK

      pf <- pndf %>%
        dplyr::filter(.data$class %in% c("town", "country", "lad"))

      cand <- match_place(txt$ald_clean[i], pf, precalc_tokens = precalc_tokens,
                          modify_words = modify_words)

      if (NROW(cand) > 0) {
        candidates <- dplyr::bind_rows(candidates, cand)
      }
    }

    if (NROW(candidates) > 0) {
      candidates <- candidates %>%
        dplyr::distinct() %>%
        select_candidate(xy = c(txt$easting[i], txt$northing[i]))
    }

    # if the pc_match on the town candidate is the same as or greater than
    # pc match on the proximal candidate then use town candidates

    if (exists("town_candidates") & NROW(candidates) > 0) {

    if (max0(town_candidates$pc_match_cand) >= max0(candidates$pc_match_cand)) {
      candidates <- town_candidates

    }}

    if (NROW(candidates) == 0) {
      candidates <- make_candidate_df() %>% tibble::add_row()
    }

    txt$candidates[[i]] <- candidates
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  # final selection
  # if home & postcode >
  # if home & postcode sector >
  # home >
  # if not home & postcode
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
        ~ .x  %>%
          dplyr::slice_max(n = 1, order_by = pc_match_cand) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::select(id) %>%
          unlist(use.names = FALSE)
      ),
      match = purrr::map_dbl(
        .data$candidates,
        ~ .x %>%
          dplyr::slice_max(n = 1, order_by = pc_match_cand) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::select(pc_match_cand) %>%
          unlist(use.names = FALSE)
      ),
      selected_id = dplyr::case_when(
        home_flag == "home" & !is.na(.data$postcode) ~ stringr::str_to_upper(.data$postcode),
        home_flag == "home" &!is.na(.data$pc2) ~ paste0("pc2_", stringr::str_to_upper(.data$pc2)),
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
      dplyr::join_by(selected_id == id)
    ) %>%
    dplyr::mutate(LSOA21CD = dplyr::case_when(
      selected_id == "home" ~ "home",
      class == "country" ~ name,
      is.na(LSOA21CD) ~ NA_character_,
      TRUE ~ LSOA21CD
    ))

  if (!debug) {
    txt <- txt %>%
      dplyr::select(
        -easting, -northing, -ald_clean, -home_flag,
        -contains_outcode, -match, -outcode, -postcode, -pc2, -n_cand, -top_cand, -candidates
      )
  }

  return(txt)
}
