#' Get region classification key
#'
#' Imports region classification keys from Statistics Finland API. Use together with
#' \code{dplyr::left_join} to add regions to data. A wrapper for the
#' \code{get_key}-function that it calls under the hood.
#'
#' The classification keys e.g. from seutukunta to maakunta or maakunta to suuralue are not very well
#' available. Thus, the logic of the function is to first get all keys from kunta to all other regions,
#' construct a table that contains all municipalities with their corresponding larger regions and
#' then apply the selection implied by the arguments set by the user possible removing duplicates.
#'
#' @param from character, the smallest region desired in the resulting classification key.
#' @param ... character(s), the regions to include in the key other than \code{from}.
#' @param year character or numerical, the year of the desired classification key.
#' @param lang \code{"fi"}, \code{"en"}, or \code{"sv"}, language of the key required.
#'    Defaults to \code{"fi"}.
#' @param only_codes logical, whether the key should contain only the region codes.
#'    Defaults to \code{FALSE}.
#' @param only_names logical, whether the key should contain only the region names.
#'    Defaults to \code{FALSE}.
#' @param offline logical, whether uses the key in the package data. Defaults \code{TRUE}.
#' @return Region classification key as a data.frame.
#' @import dplyr
#' @export
#' @examples
#'
#' get_regionkey()
#' get_regionkey("kunta", "seutukunta", only_codes = TRUE)
#'

get_regionkey <- function(from = "kunta", ..., year = NULL, lang = "fi",
                          only_codes = FALSE, only_names = FALSE, offline = TRUE) {

  latest_year <- get_latest_year(offline = offline)
  source <- tolower(from)
  targets <- unlist(list(...))
  if(is.null(targets)) {
    targets <- c("seutukunta", "maakunta")
  } else {
    targets <- tolower(targets)
  }

  if(is.null(year)) {
    year <- latest_year
  } else if((year != latest_year & offline)) {
    offline <- FALSE
    message("Overriding default option for offline regionkey for years other than the latest year.")
  }

  if(lang != "fi") {
    offline <- FALSE
    message("Overriding default option for offline for language other than Finnish.")
  }

  # target_regions <- prefix_name_key$name[-(1:2)]
  region_code_prefixes <- name_to_prefix(targets, pass_unknown = TRUE)
  # missed_targets <- logical(length(target_regions))
  # names(missed_targets) <- target_regions

  if(offline) {
    regionkey <- statficlassifications::regionkey

    if(!any(paste0(targets, "_code") %in% names(regionkey))){
      stop("Not all classifications: ", targets, " in offline classification. Try offline = FALSE")
    }

    regionkey <- dplyr::select(regionkey, c(paste(c(source, targets), "name", sep = "_"),
                                          paste(c(source, targets), "code", sep = "_")))

  } else {


  # Build a complete region key

  regionkey <- NULL

  for(target in targets) {

    # Create local ID and get key
    localId <- create_localId_name("kunta", target, year)
    key <- get_key(localId, lang = lang, print_key_name = FALSE)

    if(length(key) == 0) {
      missed_targets[target] <- TRUE
      next
    }

    # The codes in classification tables have only the numbers, not the region marker (e.g. MK, SK). Add
    # these region markers.

      key$source_code <- paste0("KU", key$source_code)
      key$target_code <- paste0(region_code_prefixes[which(targets == target)], key$target_code)

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

    # if(is.null(targets)) {
    #      targets <- target_regions[!missed_targets]
    # } else if(any(!(targets %in% c("kunta", target_regions)))) {
    #          return(message(paste0("This function only produces keys between ",
    #                 paste(target_regions, collapse = ", "),
    #                 " and kunta.")))
    # } else if (any(targets %in% target_regions[missed_targets])) {
    #          return(message(paste0("There is no key from ",
    #                               source, " to ",
    #                               paste(target_regions[missed_targets], collapse = ", "),
    #                               " for year ", year)))
    # }



  # Apply potential user selection regarding names and codes
    if(only_codes & only_names) {
      stop("Can't give you a key that has only codes but also only names!")
    }
    if(only_codes) {regionkey <- dplyr::select(regionkey, contains("code"))}
    if(only_names) {regionkey <- dplyr::select(regionkey, contains("name"))}

  # Remove potential duplicate rows. Since regionkey is constructed using municipalities
  # no matter what the source argument is, using source argument other than municipality
  # generated duplicate rows.
    regionkey <- regionkey[!duplicated(regionkey),]

  # All columns to factors
    regionkey <- dplyr::mutate(regionkey, across(.fns = as.factor))

  # Return
    regionkey
}

#' Get region classifications / code-name keys
#'
#' @param ... character(s), (vector), region(s) of required keys.
#' @param year character/numeric, year of the required keys. If NULL uses the latest year.
#' @param lang \code{"fi"}, \code{"en"}, or \code{"sv"}, language of the classification required.
#'    Defaults to \code{"fi"}.
#' @param as_named_vector logical, whether returns the key as a named vector rather than a
#'    data.frame. Defaults \code{FALSE}.
#' @param suppress_message logical, whether to suppress any messages the function might produce.
#' @param only_codes logical, whether to return only the codes in the classification.
#'     Defaults to \code{FALSE}.
#' @param only_names logical, whether to return only the names in the classification.
#'     Defaults to \code{FALSE}.
#' @param offline logical, whether uses the key in the package data. Defaults \code{TRUE}.
#'
#'
#' @return a data.frame or a named vector. A region code-name key.
#' @export
#'
#' @examples
#'
#' get_regionclassification("seutukunta")
#' get_regionclassification("seutukunta", as_named_vector = TRUE)
#' get_regionclassification("seutukunta", "maakunta")
#'
get_regionclassification <- function(...,
                                     year = NULL,
                                     offline = TRUE,
                                     as_named_vector = FALSE,
                                     suppress_message = FALSE,
                                     only_names = FALSE,
                                     only_codes = FALSE,
                                     lang = "fi") {

  latest_year <- get_latest_year(offline = offline)

  if(is.null(year)) {
    year <- latest_year
  } else {
    offline <- FALSE
    if(!suppress_message) {
    message("Overriding default option for offline when specific year is required.")
    }
  }

  if(lang != "fi") {
    offline <- FALSE
    if(!suppress_message) {
      message("Overriding default option for offline for language other than Finnish.")
    }
  }

  regions <- unlist(list(...))
  if(length(regions) == 0) { regions <- prefix_name_key$name}
  key <- data.frame()

  if(offline) {
    key <- statficlassifications::region_code_to_name_key
    filter_regexp <- paste(name_to_prefix(regions), collapse = "|")
    key <- key[grepl(pattern = filter_regexp, x = key$alue_code),]
    message("Without year-argument a general region classification including abolished municipalities got.")
  } else {

  for(region in regions) {

    if(region == "KOKO MAA") {
      key_temp <- data.frame(code = "SSS", name = "KOKO MAA")
      key <- rbind(key, key_temp)
      next
    }

    localId <- paste0(region, "_1_", year, "0101")
    key_temp <- get_classification(localId, lang = lang, print_series_name = FALSE)
    if(length(key_temp) == 0) {
      if(!suppress_message) {
         message(paste0("No region name-code key found for ", region, " for year ", year))
      }
      next
     }
    key_temp$code <- paste0(name_to_prefix(region), key_temp$code)
    key <- rbind(key, key_temp)
  }
  }

  # Check if any keys were found, if not, return an error.
  if(length(key) == 0){
    return(message("No keys found!"))
  }


  # Set column names
    if(length(regions) == 1){
      names(key) <- c(paste0(regions, "_code"), paste0(regions, "_name"))
    } else {
      names(key) <- c("alue_code", "alue_name")
    }

  # Tranform to named vector if required
    if(as_named_vector & (only_names | only_codes)) {
      stop("There is no named vector with either only codes or names.")
    }
    output <- key
    if(as_named_vector) {
      output <- as.vector(unlist(key[paste0(regions, "_name")],))
      names(output) <- as.vector(unlist(key[paste0(regions, "_code")],))
    }

  # Apply potential user selection regarding names and codes
    if(only_codes & only_names) {
      stop("Cannot give you a key that has only codes but also only names!")
    }
    if(only_codes) {output <- dplyr::select(key, contains("code"))[,1]}
    if(only_names) {output <- dplyr::select(key, contains("name"))[,1]}

  # Return
    rownames(output) <- NULL
    output
}

