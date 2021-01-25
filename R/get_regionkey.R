#' Import region classification key
#'
#' Imports NUTS-region classification keys from Statistics Finland API. Use together with 'dplyr::left_join'
#' to add regions to data. A wrapper for the \code{get_key} function that it calls under the hood.
#'
#' The classification keys e.g. from seutukunta to maakunta or maakunta to suuralue are not very well
#' available. Thus, the logic of the function is to first get all keys from kunta to all other regions,
#' construct a table that contains all municipalities with their corresponding larger regions and
#' then apply the selection implied by the arguments set by the used possible removing duplicates.
#'
#' @param region character, the smallest region desired in the resulting classification key.
#' @param only_codes logical, whether the key should contain only the region codes. Defaults to FALSE.
#' @param only_names logical, whether the key should contain only the region names. Defaults to FALSE.
#' @param year character or numerical, the year of the desired classification key.
#' @return data.frame Returns a classification key as a data.frame.
#' @export
#' @examples
#'
#' regionkey <- get_regionkey()
#'

get_regionkey <- function(source = "kunta", targets = NULL, year = NULL,
                          only_codes = FALSE, only_names = FALSE, offline = TRUE) {

  latest_year <- 2020 #get_latest_year()

  if(is.null(year)) {
    year <- latest_year
  } else if((year != latest_year & offline)) {
    offline <- FALSE
    message("Overriding default option for offline regionkey for years other than the latest year.")
  }

  target_regions <- c("seutukunta", "maakunta", "suuralue", "ely")
  region_code_prefixes <- c("SK", "MK", "SA", "ELY")
  missed_targets <- logical(length(target_regions))
  names(missed_targets) <- target_regions

  if(offline) {
    data(regionkey, package = "statficlassifications")
  } else {


  # Build a complete region key

  regionkey <- NULL


  for(target in target_regions) {

    # Create local ID and get key
    localID <- create_localID_name("kunta", target, year)
    key <- get_key(localID, print_key_name = FALSE)

    if(length(key) == 0) {
      missed_targets[target] <- TRUE
      next}

    # The codes in classification tables have only the numbers, not the region marker (e.g. MK, SK). Add
    # these region markers.

      key$source_code <- paste0("KU", key$source_code)
      key$target_code <- paste0(region_code_prefixes[which(target_regions == target)], key$target_code)

    # set the variable names, codes get prefix '_code' and names get prefix '_name'. e.g. '(maa)kunta_name'
    # and '(maa)kunta_code'.

    names(key) <- c("kunta_code", "kunta_name", paste(target, c("code", "name"), sep = "_"))

    if(is.null(regionkey)) {
      regionkey <- key
    } else {
      regionkey <- dplyr::left_join(regionkey, key, by = c("kunta_code", "kunta_name"))
    }

  }

  }

  # Apply potential user selection

    if(is.null(targets)) {
         targets <- target_regions[!missed_targets]
    } else {
         targets <- tolower(targets)
         if(any(!(targets %in% c("kunta", target_regions)))) {
             stop(paste0("This function only produces keys between ",
                    paste(target_regions, collapse = ", "),
                    " and kunta."))
         }
    }
    regionkey <- dplyr::select(regionkey, c(paste(c(source, targets), "name", sep = "_"),
                                          paste(c(source, targets), "code", sep = "_")))

  # Apply potential user selection regarding names and codes

  if(only_codes & only_names) {
    stop("Can't give you a key that has only codes but also only names!")
  }

  if(only_codes) {
    regionkey <- dplyr::select(regionkey, contains("code"))
  } else if(only_names) {
    regionkey <- dplyr::select(regionkey, contains("name"))
  }

  regionkey <- regionkey[!duplicated(regionkey),]
  dplyr::mutate_all(regionkey, as.factor)

}



#' Get region name-code keys.
#'
#' @param region character (vector) region(s) of required keys.
#' @param year
#' @param offline
#'
#' @return data.frame key
#' @export
#'
#' @examples
#'
#'   get_region_code_name_key("seutukunta")
#'
get_region_code_name_key <- function(region,
                                     year = NULL, offline = TRUE) {

  get_regionkey(source = region, targets = region, year = year, offline = offline)
}

