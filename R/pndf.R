#' Creates a gazetteer of place names from several sources.
#'
#' This function will likely take several minutes to run
#'
#' @param paf_file the build environment file from PAF (may be subset)
#' @param openname_dir a directory containing OS open name files
#' @param openname_headers headers for the columns, defaults to the values current
#' when this file was created in March 2025
#' @param openname_types types to extract from the openname file - useful if
#' access to paf file is limited and you want to include
#' @param include_london_sites boolean whether or not to roll the London UEC
#' sites in
#' @param greenspace_files a vector of greenspace files
#' @param lad_file csv file containing LAD names and codes (LAD24CD, LAD24NM)
#' @param postcode_poly_dir directory containing postcode polygons
#' @param lsoa_file file containing LSOA polygons (preferably full resolution)
#' @param add_full_address boolean, add a fulladdress column to the pndf output
#' @param write_package_data if true write data to package data folder
#' @param pndf_levels vector
#' @details a place name data frame can be created using one or more of these
#' inputs.
#'
#' Sources:
#'
#' - postcode address file - detailed and contains venue and business names
#' Obtained from Ordnance Survey via the public sector geospatial agreement
#' recommend restricting to the most relevant area (in the case of the pndf
#' file supplied in the package it is a 10km buffer around London). There are
#' several files in the extract, and the relevant one is add_gb_builtaddress.csv
#'
#' - OS Open Name - higher level but even if PAF is in use, good to add in
#' towns outside of the area covered by your PAF extract. May be a substitute
#' for PAF if that is not available. Currently at
#' https://www.ordnancesurvey.co.uk/products/os-open-names
#'
#' - OS Greenspace - contains names of open spaces. Currently at
#' https://www.ordnancesurvey.co.uk/products/os-open-greenspace
#'
#' - LADs contains names of local authority districts - in some cases these
#' aren't the same as names of populated places but often used as reference.
#' Obtained from https://geoportal.statistics.gov.uk/, and the file to use
#' is the 2024 version (or field names will need to be changed, see code
#' below for details)
#'
#' - FCDO country names - to cover places outside the UK (this is present as a dataset in the package)
#' FCDO country names https://www.gov.uk/government/publications/geographical-names-and-information
#' Created by countries <- readr::read_csv("file") %>%
#' dplyr::mutate(id = paste0("fcdo_", `Country code`), id_parent = NA, class = "country", easting = NA, northing = NA) %>%
#'   dplyr::rename(name = Name) %>%
#'   dplyr::select(id, name, easting, northing, id_parent, class)
#'
#' - postcode polygons - to assign postcode districts to points where that has
#' not already been done in the source. Obtained from OS via PSGA, it comes as
#' as series of shapefiles for each postcode area
#'
#' - lsoa_file - to assign all points to an LSOA for later processing
#'
#' @returns a dataframe with columns
#'            id
#'            name
#'            id_parent (may be NA)
#'            easting on the BNG
#'            northing
#'            class
#'            fulladdress
#'            LSOA21CD
#'            pcd
#'
#' @export

make_pndf <- function(paf_file = NULL,
                      openname_dir = NULL,
                      openname_headers = NULL,
                      openname_types = NULL,
                      greenspace_files = NULL,
                      include_london_sites = TRUE,
                      lad_file = NULL,
                      postcode_poly_dir = NULL,
                      lsoa_file = NULL,
                      add_full_address = TRUE,
                      write_package_data = FALSE,
                      pndf_levels = c(
                        "pc",
                        "org",
                        "pc2",
                        "street",
                        "locality",
                        "pcd",
                        "lad",
                        "populatedplace",
                        "greenspace",
                        "town",
                        "landform",
                        "country"
                      )) {
  options("dplyr.summarise.inform" = FALSE)

  # check inputs present

  if (is.null(paf_file) &
      is.null(openname_dir) &
      is.null(greenspace_files) &
      is.null(lad_file)) {
    stop("No data supplied")
  }

  # read in country names as a start

  pndf <- istv::countries

  # process the PAF file

  if (!is.null(paf_file)) {
    # read in the PAF, select relevant fields, extract postcode districts
    # and postcode sectors (often found as a result of anonymization practices)
    #

    message("reading paf")

    paf <- readr::read_csv(
      paf_file,
      col_select = c(
        .data$uprn,
        .data$organisationname,
        .data$subname,
        .data$name,
        .data$fulladdress,
        .data$streetname,
        .data$locality,
        .data$townname,
        .data$postcode,
        .data$primaryclassificationdescription,
        .data$secondaryclassificationdescription,
        .data$tertiaryclassificationdescription,
        .data$easting,
        .data$northing,
        .data$positionalaccuracy
      ),
      show_col_types = FALSE,
      progress = FALSE
    ) %>%
      dplyr::mutate(
        pcd = stringr::str_extract(.data$postcode, "(.*?)\\s(.*?)", group = 1),
        pc2 = stringr::str_sub(.data$postcode, end = -3)
      )

    # "underground station" is often written as "station" in the notes - ditto
    # railway station and overground railway station
    # and "clapham south tube" is common as well
    # create entries that reflect this
    #

    transport <- paf %>%
      dplyr::filter(
        stringr::str_detect(
          .data$name,
          "(((OVERGROUND |UNDERGROUND )(RAILWAY )?)STATION)|(RAILWAY STATION)"
        ),
        .data$secondaryclassificationdescription == "Transport"
      ) %>%
      dplyr::mutate(
        id = .data$uprn,
        class = "org",
        id_parent = .data$uprn %>% as.character()
      ) %>%
      dplyr::select(
        id,
        name,
        easting,
        northing,
        class,
        id_parent,
        fulladdress
      )

    stations <- transport %>%
      dplyr::mutate(
        name = stringr::str_replace_all(.data$name, "OVERGROUND", "") %>%
          stringr::str_replace_all("UNDERGROUND", "") %>%
          stringr::str_replace_all("RAILWAY", "") %>%
          stringr::str_squish(),
        id = paste0("rail_", .data$id)
      )

    tube <- transport %>%
      dplyr::filter(stringr::str_detect(.data$name, "UNDERGROUND")) %>%
      dplyr::mutate(
        name = stringr::str_replace_all(.data$name, "UNDERGROUND", "") %>%
          stringr::str_replace_all("RAILWAY", "") %>%
          stringr::str_replace_all("STATION", "TUBE") %>%
          stringr::str_squish(),
        id = paste0("tube_", .data$id)
      )

    # station names are sometimes used without the suffix station - create
    # a list of these that can be checked against locality/populated place/town
    # names
    #

    raw_stations <- transport %>%
      dplyr::mutate(
        name = stringr::str_replace_all(.data$name, "(RAILWAY|STATION|UNDERGROUND)", "") %>%
          stringr::str_squish()
      ) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::summarize(
        id = paste0("raw_", utils::head(id, 1)),
        easting = mean(.data$easting),
        northing = mean(.data$northing),
        class = utils::head(.data$class, 1),
        id_parent = head(.data$id_parent, 1),
        fulladdress = head(.data$fulladdress, 1)
      ) %>%
      dplyr::filter(.data$name != "")

    # prisons are either HMP x or x prison and common assault locations
    #

    prisons <- paf %>%
      dplyr::filter(.data$tertiaryclassificationdescription == "Prison") %>%
      dplyr::mutate(
        id = .data$uprn,
        class = "org",
        id_parent = .data$uprn %>% as.character()
      ) %>%
      dplyr::select(
        id,
        name,
        easting,
        northing,
        class,
        id_parent,
        fulladdress
      )

    prisons_hmp <- prisons %>%
      dplyr::mutate(
        name = stringr::str_replace(.data$name, "H\\s?M\\sP(RISON)? ", "HMP "),
        id = paste0("prison_hmp_", .data$id)
      )

    prisons_prison <- prisons_hmp %>%
      dplyr::mutate(
        name = stringr::str_replace(.data$name, pattern = "HMP (.*)", replacement = "\\1 PRISON"),
        id = stringr::str_replace(.data$id, "hmp", "prison")
      )

    # create a data frame with all the postcodes

    pcs <- paf %>%
      dplyr::filter(!is.na(.data$postcode)) %>%
      dplyr::group_by(.data$postcode, .data$pcd) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(id_parent = .data$pcd, name = .data$postcode) %>%
      dplyr::mutate(
        class = "pc",
        id = stringr::str_replace(.data$name, "\\s", "")
      )

    # ditto postcode sectors

    pc2 <- paf %>%
      dplyr::filter(!is.na(.data$postcode)) %>%
      dplyr::group_by(.data$pc2, .data$pcd) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(id_parent = .data$pcd, name = .data$pc2) %>%
      dplyr::mutate(
        class = "pc2",
        id = paste0("pc2_", stringr::str_replace(.data$name, "\\s", ""))
      )

    # ditto postcode districts

    pcds <- paf %>%
      dplyr::filter(!is.na(.data$pcd)) %>%
      dplyr::group_by(.data$pcd) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = .data$pcd) %>%
      dplyr::mutate(
        class = "pcd",
        id_parent = NA,
        id = paste0("pcd_", .data$name)
      )

    # ditto towns

    towns <- paf %>%
      dplyr::filter(!is.na(.data$townname)) %>%
      dplyr::group_by(.data$townname) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = .data$townname) %>%
      dplyr::mutate(
        id = paste0(
          "t",
          dplyr::row_number() %>%
            stringr::str_pad(
              width = 8,
              side = "left",
              pad = "0"
            )
        ),
        class = "town",
        id_parent = NA
      )

    # ditto localities

    localities <- paf %>%
      dplyr::group_by(.data$locality, .data$townname) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::filter(!is.na(.data$locality)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = .data$locality) %>%
      dplyr::mutate(
        id = paste0(
          "l",
          dplyr::row_number() %>%
            stringr::str_pad(
              width = 8,
              side = "left",
              pad = "0"
            )
        ),
        class = "locality"
      ) %>%
      dplyr::left_join(
        towns %>% dplyr::select(name, id),
        join_by(townname == name),
        suffix = c("", "_parent")
      ) %>%
      dplyr::select(-townname)

    # ditto streets

    streets <- paf %>%
      dplyr::group_by(.data$streetname, .data$pcd) %>%
      dplyr::filter(!is.na(.data$streetname)) %>%
      dplyr::summarize(
        easting = mean(.data$easting),
        northing = mean(.data$northing)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = .data$streetname) %>%
      dplyr::mutate(
        id = paste0(
          "s",
          dplyr::row_number() %>%
            stringr::str_pad(
              width = 8,
              side = "left",
              pad = "0"
            )
        ),
        class = "street"
      ) %>%
      dplyr::rename(id_parent = .data$pcd)

    # now check the station list against towns, localities, streets
    #

    raw_stations <- raw_stations %>%
      dplyr::anti_join(
        towns %>% dplyr::distinct(.data$name),
        join_by(name)
      ) %>%
      dplyr::anti_join(
        localities %>% dplyr::distinct(.data$name),
        join_by(name)
      ) %>%
      dplyr::anti_join(
        streets %>% dplyr::distinct(.data$name),
        join_by(name)
      )

    # ditto organizations

    orgs <- paf %>%
      dplyr::filter(!is.na(.data$organisationname)) %>%
      dplyr::group_by(
        .data$organisationname,
        .data$streetname,
        .data$postcode,
        .data$pcd
      ) %>%
      dplyr::summarize(
        id = min(.data$uprn),
        easting = mean(.data$easting),
        northing = mean(.data$northing),
        postcode = utils::head(.data$postcode, 1),
        fulladdress = utils::head(.data$fulladdress, 1)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = paste0("o_", as.character(.data$id))) %>%
      dplyr::mutate(class = "org") %>%
      dplyr::left_join(
        streets %>% dplyr::select(name, id_parent, id),
        join_by(
          .data$streetname == .data$name,
          .data$pcd == .data$id_parent
        ),
        suffix = c("", "_parent")
      ) %>%
      dplyr::rename(
        id_parent = .data$id_parent_parent,
        name = .data$organisationname
      ) %>%
      dplyr::select(-pcd, -postcode, -streetname)

    # ditto names

    names <- paf %>%
      dplyr::group_by(.data$name, .data$streetname, .data$postcode, .data$pcd) %>%
      dplyr::filter(!is.na(.data$name)) %>%
      dplyr::summarize(
        id = min(.data$uprn), ,
        easting = mean(.data$easting),
        northing = mean(.data$northing),
        postcode = utils::head(.data$postcode, 1),
        fulladdress = utils::head(.data$fulladdress, 1)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(class = "org", id = paste0("n_", as.character(.data$id))) %>%
      dplyr::left_join(
        streets %>% dplyr::select(name, id_parent, id),
        join_by(
          .data$streetname == .data$name,
          .data$pcd == .data$id_parent
        ),
        suffix = c("", "_parent")
      ) %>%
      dplyr::rename(id_parent = .data$id_parent_parent) %>%
      dplyr::select(-pcd, -postcode, -streetname)

    pndf <- dplyr::bind_rows(
      stations,
      tube,
      prisons_prison,
      prisons_hmp,
      names,
      orgs,
      pcs,
      pc2,
      pcds,
      streets,
      localities,
      towns,
      pndf
    )
  }

  # Add OS Openname data if required

  if (!is.null(openname_dir)) {
    message("reading opennames")

    if (is.null(openname_headers)) {
      openname_headers <- istv::openname_headers
    }

    if (is.null(openname_types)) {
      openname_types <- c("populatedPlace", "landform")
    }

    openname <- openname_dir %>%
      dir(full.names = TRUE) %>%
      purrr::map(
        ~ readr::read_csv(.x, col_names = openname_headers, show_col_types = FALSE),
        .progress = FALSE
      ) %>%
      dplyr::bind_rows()

    openname <- openname %>%
      dplyr::filter(.data$TYPE %in% openname_types, .data$COUNTRY == "England") %>%
      dplyr::select(
        ID,
        NAME1,
        TYPE,
        GEOMETRY_X,
        GEOMETRY_Y,
        POSTCODE_DISTRICT
      ) %>%
      dplyr::rename(
        id_parent = .data$POSTCODE_DISTRICT,
        id = .data$ID,
        name = .data$NAME1,
        class = .data$TYPE,
        easting = .data$GEOMETRY_X,
        northing = .data$GEOMETRY_Y
      ) %>%
      dplyr::mutate(class = stringr::str_to_lower(.data$class)) %>%
      dplyr::select(
        id,
        name,
        easting,
        northing,
        id_parent,
        class
      ) %>%
      add_row(
        id = c("northlondon", "southlondon", "westlondon", "southlondon"),
        name = c("north london", "south london", "west london", "east london"),
        easting = NA,
        northing = NA,
        id_parent = NA,
        class = "populatedplace"
      )

    # if we have station names from the PAF check against the populated place
    # list
    #

    if (exists("raw_stations")) {
      raw_stations <- raw_stations %>%
        dplyr::anti_join(
          openname %>%
            dplyr::distinct(.data$name) %>%
            dplyr::mutate(name = stringr::str_to_upper(.data$name)),
          join_by(name)
        )
    }

    pndf <- dplyr::bind_rows(openname, pndf)
  }

  # add greenspace data if necessary

  if (!is.null(greenspace_files)) {
    message("reading greenspace")

    greenspace <- greenspace_files %>%
      purrr::map(~ read_sf(.x, quiet = TRUE)) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!is.na(.data$distName1)) %>%
      sf::st_centroid() %>%
      dplyr::mutate(
        easting = sf::st_coordinates(.data)[, 1],
        northing = sf::st_coordinates(.data)[, 2],
        class = "greenspace"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(name = .data$distName1) %>%
      dplyr::select(
        id,
        name,
        easting,
        northing,
        class
      ) %>%
      dplyr::mutate(id_parent = NA)

    pndf <- dplyr::bind_rows(greenspace, pndf)
  }

  # add NHS london sites from package data

  if (include_london_sites) {
    message("adding london NHS sites")

    sites <- istv::site_xy %>%
      dplyr::select(
        site_code,
        site,
        easting,
        northing
      ) %>%
      dplyr::mutate(class = "org", id_parent = NA) %>%
      dplyr::rename(id = .data$site_code, name = .data$site)

    pndf <- dplyr::bind_rows(sites, pndf)
  }

  # add lad data if necessary

  if (!is.null(lad_file)) {
    message("reading local authority districts")

    lad <- readr::read_csv(lad_file, show_col_types = FALSE) %>%
      dplyr::rename(
        id = .data$LAD24CD,
        name = .data$LAD24NM,
        easting = .data$BNG_E,
        northing = .data$BNG_N
      ) %>%
      dplyr::select(id, name, easting, northing) %>%
      dplyr::mutate(class = "lad", id_parent = NA)

    pndf <- dplyr::bind_rows(lad, pndf)
  }

  # finally bind raw_stations if it exists
  #

  if (exists("raw_stations")) {
    pndf <- dplyr::bind_rows(pndf, raw_stations)
  }

  # everything from here is about assigning lookups - primarily
  # lsoa and postcode districts (of the centroid)

  # assign postcode districts where possible

  if (!is.null(postcode_poly_dir)) {
    message("assigning postcode districts")

    pcd_sf <- dir(postcode_poly_dir,
                  pattern = "*.shp",
                  full.names = TRUE
    ) %>%
      purrr::map(~ sf::st_read(.x, quiet = TRUE)) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(pcd = stringr::str_extract(.data$POSTCODE, "([:alnum:]*)(\\s[:alnum:]*)", 1)) %>%
      dplyr::group_by(.data$pcd) %>%
      dplyr::summarize(geometry = sf::st_union(.data$geometry)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$pcd))

    pndf_sf <- sf::st_as_sf(pndf %>% dplyr::filter(!is.na(.data$easting)),
                            coords = c("easting", "northing")
    )
    sf::st_crs(pndf_sf) <- sf::st_crs(pcd_sf)

    pcd_lookup <- sf::st_join(pndf_sf, pcd_sf) %>%
      tibble::as_tibble() %>%
      dplyr::select(id, pcd) %>%
      dplyr::mutate(pcd = stringr::str_to_lower(.data$pcd))

    pndf <- pndf %>%
      dplyr::left_join(pcd_lookup, join_by(id))
  }

  # assign LSOAs - this only works for locations in England and Wales obvs
  # currently filtering out greenspace and landforms in Scotland

  if (!is.null(lsoa_file)) {
    message("assigning lsoa")

    lsoa21 <- sf::st_read(lsoa_file, quiet = TRUE)

    lsoa_lookup <- sf::st_join(pndf_sf, lsoa21, largest = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::select(id, LSOA21CD) %>%
      dplyr::filter(!is.na(.data$LSOA21CD))

    pndf <- pndf %>%
      dplyr::left_join(lsoa_lookup, join_by(id)) %>%
      dplyr::filter(!(
        .data$class %in% c("greenspace", "landform") &
          is.na(.data$LSOA21CD)
      ))
  }

  # useful to add full address in sometimes
  #

  message("finalizing")

  if (!is.null(paf_file) & add_full_address) {
    pndf <- pndf %>%
      dplyr::mutate(fulladdress = dplyr::if_else(is.na(.data$fulladdress), .data$name, .data$fulladdress)) %>%
      dplyr::distinct()
  }

  # clean the text strings using the clean_text function for ease of comparison
  #

  pndf <- pndf %>%
    clean_text(col = .data$name) %>%
    dplyr::mutate(name = dplyr::if_else(
      class %in% c("pc2", "pcd", "pc"),
      stringr::str_to_lower(.data$name),
      .data$ald_clean
    )) %>%
    dplyr::select(-ald_clean)

  # make class a factor from most to least granular features (roughly)
  #

  if (!is.null(pndf_levels)) {
    pndf <- pndf %>%
      dplyr::mutate(class = factor(.data$class, pndf_levels, ordered = TRUE))
  }

  if (write_package_data) {
    message("writing to package")

    usethis::use_data(pndf, overwrite = TRUE)
  }

  return(pndf)
}

#' filter a place name file to be within a certain distance of a point
#'
#' @param x numeric, x coordinate of center (probably best on BNG)
#' @param y numeric, y coordinate of center (probably best on BNG)
#' @param r distance of bounding box from center
#' @param pndf tibble with columns at least easting and northing containing
#' coordinates of locations to be filtered, if null the data supplied with the
#' package is used
#
#' @details creates a bounding box from x-r to x+r and y-r to y+r and filters
#' locations supplied in pndf
#'
#' @returns data frame pndf filtered to area
#'
#' @export

filter_pndf <- function(x = 531370,
                        y = 180220,
                        r = 500,
                        pndf = NULL) {
  if (is.null(pndf)) {
    pndf <- istv::pndf
  }

  xmin <- x - r
  xmax <- x + r
  ymin <- y - r
  ymax <- y + r

  pndf_filtered <- pndf %>%
    dplyr::filter(
      .data$easting < xmax,
      .data$easting > xmin,
      .data$northing < ymax,
      .data$northing > ymin
    )
}
