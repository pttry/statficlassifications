#' Get map
#'
#' @param region_level character, region level of the map
#' @param year double, year of the map
#'
#' @return sf object
#' @export
#'
#' @examples
get_map <- function(region_level, year) {

  capabilities <- xml2::read_xml("https://geo.stat.fi/geoserver/tilastointialueet/wfs?service=WFS&version=2.0.0&request=GetCapabilities")
  map_names <- capabilities %>%
    xml2::xml_find_all("//wfs:FeatureType/wfs:Name") %>%
    xml2::xml_text()

  # Filter the required map from the list of all maps
  file <- tail(grep(paste0("tilastointialueet:", tolower(region_level)),
                    grep(as.character(year), map_names, value = TRUE),
                    value = TRUE),
               n = 1)

  # Test if the search was successful, return error if not.
  if(length(file) == 0) {stop("Map not found!")}

  url <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
  url$query <- list(service ="WFS",
                    version ="2.0.0",
                    request ="GetFeature",
                    typename = file,
                    outputFormat ="application/json")

  map <- sf::st_read(httr::build_url(url), quiet = TRUE)
  map[[region_level]] <- set_region_codes(map[[region_level]], region_level = region_level, year = year)
  map

}


#' Merge map
#'
#' @param map sf
#' @param by character, grouping variable
#'
#' @return
#' @export
#'
#' @examples
merge_map <- function(map, by = NULL) {
  map |> group_by_at(by) |> summarize(geometry = sf::st_union(geometry))

}
