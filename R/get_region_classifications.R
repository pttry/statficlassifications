#' Get region classification key
#'
#' Imports region classification keys from Statistics Finland API. A wrapper for
#' `get_key()` that it calls under the hood.
#'
#' The classification keys e.g. from seutukunta to maakunta or maakunta to suuralue are not very well
#' available. Thus, the logic of the function is to first get all keys from kunta to all other regions,
#' construct a table that contains all municipalities with their corresponding larger regions and
#' then apply the selection implied by the arguments set by the user possible removing duplicates.
#' `get_regionkey()` can thus also construct keys that are not available in the Statistics
#' Finland classification keys API.
#'
#' @param ... character(s), the regions to include in the key.
#' @param year character or numerical, the year of the desired classification key.
#' @param lang `"fi"`, `"sv"` or `"en"`. Language of the key required.
#'    Defaults to `"fi"`.
#' @param only_codes logical, whether the key should contain only the region codes.
#'    Defaults to `FALSE`.
#' @param only_names logical, whether the key should contain only the region names.
#'    Defaults to `FALSE`.
#' @param offline logical, whether uses the key in the package data. Defaults `TRUE`.
#' @return Region classification key as a data.frame.
#' @import dplyr
#' @export
#' @examples
#'
#' get_regionkey()
#' get_regionkey("kunta", "seutukunta", only_codes = TRUE)
#'

get_regionkey <- function(...,
                          year = NULL,
                          only_codes = FALSE,
                          only_names = FALSE,
                          lang = "fi",
                          offline = TRUE) {

  ## Set up ##

  latest_year <- get_latest_year(offline = offline)
  regions <- unique(unlist(list(...)))
  if(is.null(regions)) {
    regions <- c("kunta", "seutukunta", "maakunta")
  } else {
    regions <- tolower(regions)
  }

  if(is.null(year)) {
    year <- latest_year
  } else if((year != latest_year & offline)) {
    offline <- FALSE
    message("Overriding default option for offline regionkey for years other than the latest year.")
  }

  if(lang != "fi" & offline) {
    offline <- FALSE
    message("Overriding default option for offline for language other than Finnish.")
  }

  # Override offline = FALSE if desired target not in offline regionkey
  if(!all(sapply(regions, function(x) {any(grepl(pattern = x, names(statficlassifications::regionkey)))})) & offline) {
    offline <- FALSE
    message("Overriding default option for offline for target region not in offline regionkey.")
  }

  ## Get key ##

  if(offline) {
    regionkey <- statficlassifications::regionkey

  } else {

   # Build a complete region key

    regionkey <- NULL

    for(region in regions[regions != "kunta"]) {

      # All keys are constructed using municipalities (kunta). Keys only between
      # larger regions may not be available, but keys between municipalities and
      # other regions are always available. Municipalities are thus used to
      # bind also larger regions together for which there might not be a specific
      # key.

      # Create local ID and get key
        localId <- create_localId_name("kunta", region, year)
        key <- suppressMessages(get_key(localId, lang = lang))

      # Return error if key not found
       if(length(key) == 0) { stop(paste("No key for", region, "found")) }

      # The codes in classification tables have only the numbers, not the region marker (e.g. MK, SK). Add
      # these region markers.

       key$source_code <- paste0(name_to_prefix("kunta"), key$source_code)
       key$target_code <- paste0(name_to_prefix(region, pass_unknown = TRUE), key$target_code)

      # set the variable names, codes get prefix '_code' and names get prefix '_name'. e.g. '(maa)kunta_name'
      # and '(maa)kunta_code'.

       names(key) <- c("kunta_code", "kunta_name", paste(region, c("code", "name"), sep = "_"))

       if(is.null(regionkey)) {
         regionkey <- key
       } else {
         regionkey <- dplyr::left_join(regionkey, key, by = c("kunta_code", "kunta_name"))
       }

    }

  }

  ## Format output and return ##

  # Apply potential user selection regarding regions

     regionkey <- dplyr::select(regionkey, c(paste(regions, "name", sep = "_"),
                                             paste(regions, "code", sep = "_")))

  # Apply potential user selection regarding names and codes

    if(only_codes & only_names) {
      stop("Can't give you a key that has only codes but also only names!")
    }
    if(only_codes) {regionkey <- dplyr::select(regionkey, contains("code"))}
    if(only_names) {regionkey <- dplyr::select(regionkey, contains("name"))}

  # Remove potential duplicate rows. Since regionkey is constructed using municipalities
  # no matter what the source argument is, using source argument other than municipality
  # generates duplicate rows.

    regionkey <- dplyr::distinct(regionkey)

  # All columns to factors

    regionkey <- data.frame(lapply(regionkey, as.factor))

  # Return

    regionkey
}

#' Get region classifications / code-name keys
#'
#' @param ... character(s), (vector), region(s) of required keys.
#' @param year character/numeric, year of the required keys. As default returns a general
#'    classification containing also abolished municipalities.
#' @param lang `"fi"`, `"sv"` or `"en"`. Language of the classification required.
#'    Defaults to `"fi"`.
#' @param as_named_vector logical, whether returns the key as a named vector rather than a
#'    data.frame. Defaults `FALSE`.
#' @param suppress_message logical, whether to suppress any messages the function might produce.
#' @param only_codes logical, whether to return only the codes in the classification.
#'     Defaults to `FALSE`.
#' @param only_names logical, whether to return only the names in the classification.
#'     Defaults to `FALSE`.
#' @param offline logical, whether uses the key in the package data. Defaults `TRUE`.
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
                                     only_names = FALSE,
                                     only_codes = FALSE,
                                     as_named_vector = FALSE,
                                     lang = "fi",
                                     offline = TRUE) {

  if(!is.null(year) & offline) {
    offline <- FALSE
    message("Overriding default option for offline when specific year is required.")
  }

  if(lang != "fi" & offline) {
    offline <- FALSE
      message("Overriding default option for offline for language other than Finnish.")
  }

  regions <- unlist(list(...))

  # If no specific region required, give the regions listed in the prefix_name_key
  if(is.null(regions)) { regions <- prefix_name_key$name }
  key <- data.frame()

  if(offline) {

   # Without year-argument a general region classification including abolished municipalities got.
    key <- statficlassifications::region_code_to_name_key
    filter_regexp <- paste(name_to_prefix(regions), collapse = "|")
    key <- key[grepl(pattern = filter_regexp, x = key$alue_code),]

  } else {

    # If user has not set the year of classification, use the latest available year
       if(is.null(year)) {year <- get_latest_year(offline = FALSE)}

        for(region in regions) {

           if(region == "KOKO MAA") {
              key_temp <- data.frame(code = "SSS", name = "KOKO MAA")
              key <- rbind(key, key_temp)
              next
           }

           localId <- paste0(region, "_1_", year, "0101")
           key_temp <- suppressMessages(get_classification(localId, lang = lang))
           if(length(key_temp) == 0) {
                message(paste0("No region name-code key found for ", region, " for year ", year))
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

  # Transform to named vector if required
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

