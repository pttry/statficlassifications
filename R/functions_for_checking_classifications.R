
#' Check if a region variable correspond to classification of regionkey
#'
#' @param data
#' @param region_var
#' @param offline
#'
#' @return
#' @export
#'
#' @examples
check_region_var_classification <- function(data, region_var, offline = TRUE) {

  regionkey <- get_regionkey(offline = offline)

  logical <- logical(length(names(regionkey)))
  names(logical) <- names(regionkey)
  for(var in names(regionkey)) {
    if(all(data[[region_var]] %in% regionkey[[var]])) {
      logical[var] <- TRUE
    }
  }
  any(logical)
}

#' Check if region names and codes correspond as in regionkey.
#'
#' @param data
#' @param region_name_var
#' @param region_code_var
#' @param offline
#'
#' @return
#' @export
#'
#' @examples
check_region_var_name_code_correspondence <- function(data,
                                                      region_name_var, region_code_var,
                                                      offline = TRUE) {

  regions <- c("kunta", "seutukunta", "maakunta", "suuralue", "ely")

  regionkey <- get_regionkey(offline = offline)
  regionkey <- purrr::map(regions, ~tidyr::unite(regionkey, !!.x, paste(.x, c("name", "code"), sep = "_"))) %>%
    purrr::flatten() %>%
    as.data.frame() %>%
    dplyr::select(regions)

  data <- tidyr::unite(data, region_var, region_name_var, region_code_var)

  logical <- logical(length(names(regionkey)))
  names(logical) <- names(regionkey)
  for(var in names(regionkey)) {
    if(all(data$region_var %in% regionkey[[var]])) {
      logical[var] <- TRUE
    }
  }
  any(logical)
}

