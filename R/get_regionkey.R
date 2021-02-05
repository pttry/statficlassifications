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
#' @import dplyr
#' @export
#' @examples
#'
#' regionkey <- get_regionkey()
#'

get_regionkey <- function(source = "kunta", targets = NULL, year = NULL,
                          only_codes = FALSE, only_names = FALSE, offline = TRUE) {

  latest_year <- get_latest_year(offline = offline)
  source <- tolower(source)
  targets <- tolower(targets)

  if(is.null(year)) {
    year <- latest_year
  } else if((year != latest_year & offline)) {
    offline <- FALSE
    message("Overriding default option for offline regionkey for years other than the latest year.")
  }

  target_regions <- prefix_name_key$name[-(1:2)]
  region_code_prefixes <- prefix_name_key$prefix[-(1:2)]
  missed_targets <- logical(length(target_regions))
  names(missed_targets) <- target_regions

  if(offline) {
    regionkey <- statficlassifications::regionkey
  } else {


  # Build a complete region key

  regionkey <- NULL

  for(target in target_regions) {

    # Create local ID and get key
    localId <- create_localId_name("kunta", target, year)
    key <- get_key(localId, print_key_name = FALSE)

    if(length(key) == 0) {
      missed_targets[target] <- TRUE
      next
    }

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

  # Apply potential user selection regarding regions

    if(is.null(targets)) {
         targets <- target_regions[!missed_targets]
    } else if(any(!(targets %in% c("kunta", target_regions)))) {
             return(message(paste0("This function only produces keys between ",
                    paste(target_regions, collapse = ", "),
                    " and kunta.")))
    } else if (any(targets %in% target_regions[missed_targets])) {
             return(message(paste0("There is no key from ",
                                  source, " to ",
                                  paste(target_regions[missed_targets], collapse = ", "),
                                  " for year ", year)))
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

#' Get region code-name keys
#'
#' @param ... character(s), (vector), region(s) of required keys.
#' @param year character/numeric, year of the required keys. If NULL uses the latest year.
#' @param offline logical, whether uses the key in the package data. Defaults TRUE.
#' @param as_named_vector logical, whether returns the key as a named vector rather than a
#'    data.frame. Defaults FALSE.
#'
#' @return a data.frame or a named vector. A region code-name key.
#' @export
#'
#' @examples
#'
#' get_region_code_name_key("seutukunta")
#' get_region_code_name_key("seutukunta", as_named_vector = TRUE)
#' get_region_code_name_key("seutukunta", "maakunta")
#'
get_region_code_name_key <- function(..., year = NULL, offline = TRUE, as_named_vector = FALSE) {

  latest_year <- get_latest_year(offline = offline)

  if(is.null(year)) {
    year <- latest_year
  } else if((year != latest_year & offline)) {
    offline <- FALSE
    message("Overriding default option for offline regionkey for years other than the latest year.")
  }

  regions <- tolower(unlist(list(...)))
  key <- data.frame()

  for(region in regions) {
    # localId <- grep(region,
    #                 search_classifications(region, as_localId = TRUE),
    #                 value = TRUE) %>%
    #            grep(pattern = year, value = TRUE)

    localId <- paste0(region, "_1_", year, "0101")
    key_temp <- get_classification(localId, print_series_name = FALSE)
    if(length(key_temp) == 0) {
      message(paste0("No region name-code key found for ", region, " for year ", year))
      next
     }
    key_temp$code <- paste0(name_to_prefix(region), key_temp$code)
    key <- rbind(key, key_temp)
  }

  if(length(key) == 0){
    return(message("No keys found!"))
  }

  if(length(regions) == 1){
    names(key) <- c(paste0(regions, "_code"), paste0(regions, "_name"))
  } else {
    names(key) <- c("alue_code", "alue_name")
  }
  output <- key
  if(as_named_vector) {
    output <- as.vector(unlist(key[paste0(region, "_name")],))
    names(output) <- as.vector(unlist(key[paste0(region, "_code")],))
  }
  output
}

