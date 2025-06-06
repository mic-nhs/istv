#' London Type 1 (consultant led) Emergency Department site locations
#'
#' Coordinates of London Type 1 Emergency Departments, their ODS codes and
#' associated trust and ICB. EPSG:27700 (British National Grid)
#'
#' @format ## `sites_xy`
#' A data frame with 28 rows and 7 columns:
#' \describe{
#'   \item{icb_name, trust_name, site_name}{human readable names for icb, trust and site}
#'   \item{icb, trust, site}{ODS codes for icb, trust and site}
#'   \item{geometry}{Coordinates of the site on BNG}
#' }
#' @source <https://www.odsdatasearchandexport.nhs.uk/>
"site_xy"

#' place name dataframe
#'
#' @format ## `pndf`
#' A data frame with ... rows and 6 columns:
#' \describe{
#'   \item{name}{name}
#'   \item{easting, northing}{BNG coordinates of location}
#'   \item{id}{unique id}
#'   \item{class}{class - e.g. town or street}
#'   \item{id_parent}{id of parent entity if applicable}
#' }
#' @source <https://www.odsdatasearchandexport.nhs.uk/>
"pndf"

#' Common abbreviations to be substituted
#'
#' @format ## `abbreviations`
#' A data frame with 2 columns:
#' \describe{
#'   \item{regex}{regex to be replaced}
#'   \item{replacement}{replacement}
#' }
"abbreviations"

#' countries with values to be appended to a place name data frame
#'
#' @format ## `countries`
#' A data frame with ... rows and 6 columns:
#' \describe{
#'   \item{name}{name of country}
#'   \item{easting, northing}{position on BNG, NA in this case}
#'   \item{id}{unique ID}
#'   \item{class}{country in this case}
#' }
#' @source <https://www.data.gov.uk/dataset/0b412b57-6934-4f28-b495-640cdc7e8f7f/foreign-commonwealth-and-development-office-geographical-names-index/datafile/f9835358-6435-41da-803c-f7917db7fdad/preview>
"countries"

#' model for assigning home locations
#'
#' @format ## `docs_fit`
#' A data frame with ... rows and 6 columns:
"docs_fit"

#' headers of openname file from ODS
#'
#' @format ## `openname_headers`
#' Character vector of openname column headers
#'
#' @source Ordnance Survey documentation
"openname_headers"


#' place names that are also dictionary words, e.g. derby
#'
#' @format ## `place_names`
#' Character vector of place names
#'
#' @source Ordnance Survey documentation
"place_names"
