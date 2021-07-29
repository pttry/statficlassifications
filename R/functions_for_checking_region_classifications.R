# Functions for checking region classifications are divided into functions that
# test whether input is a certain element of classification returning logicals
# functions that check if input vector is ok. The checking functions return TRUE
# if everything is ok and FALSE if not. In case of FALSE, they also return a message
# containing information where the problem is.

# Testing of the region information of data should proceed by first checking
# if the region codes and names are ok and then whether they correctly
# correspond to each other.

################# CHECK REGION CLASSIFICATIONS ###################

#' Check region classifications
#'
#' @param region_names a vector of standard region names
#' @param region_codes a vector of standard region codes
#' @param year double or character, year of classification
#' @param lang \code{fi}, \code{sv} or \code{en}. Language of the input name. Defaults to \code{fi}.
#' @param offline logical, whether works offline with package data. Defaults to \code{TRUE}.
#'
#' @return
#' @export
#'
check_region_classification <- function(region_names, region_codes,
                                        year = NULL,
                                        lang = "fi",
                                        offline = TRUE) {

  if(
    all(check_region_names(region_names, year = year, lang = lang, offline = offline),
      check_region_codes(region_codes, year = year, offline = offline),
      check_region_name_code_correspondence(region_names, region_codes, year = year, lang = lang, offline = offline))
  ) {message("Region classification check: passed.")}
}

################# CHECK REGION CODES ##################

#' Check region codes
#'
#' Checks whether all the elements in the given vector of potential region codes
#' are standardized region codes. If they are returns \code{TRUE}, if they are
#' not, returns a message telling which elements are not recognized as region codes.
#'
#' @param x character vector or factor
#' @param year double or character, year of classification
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return vector of logicals
#' @export
#'
#'
check_region_codes <- function(x, year = NULL, offline = TRUE) {

  if(is.vector(x)){
    x <- check_region_codes_vct(x, offline = offline, year = year)
  } else if(is.factor(x)) {
    x <- check_region_codes_fct(x, offline = offline, year = year)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn check_region_codes
#'
#' Check region codes of a vector
#'
#' For internal use.
#'
#' @export
#'
check_region_codes_vct <- function(x, year = NULL, offline = TRUE) {

  logical <- is_region_code_with_prefix(x, offline = offline, year = year)
  if(all(logical)) {
    return(TRUE)
  } else {
    message(paste0("Region classification check: region code(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as standard region code(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
    return(FALSE)
  }
}

#' @describeIn check_region_codes
#'
#' Check region codes of a vector
#'
#' For internal use.
#'
#' @export
#'
check_region_codes_fct <- function(x, year = NULL, offline = TRUE) {

  check_region_codes_vct(levels(x), offline = offline, year = year)

}

################### CHECK REGION NAMES ####################

#' Check region names
#'
#' For a vector or a factor of region names, find the names that are not recognized and
#' if does not find such names returns \code{TRUE}. \code{check_region_names} uses
#' \code{check_region_names_vct} for vectors and \code{check_region_names_fct} for factors.
#'
#' @param x character vector or factor.
#' @param year double or character, year of classification.
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#' @param lang \code{fi}, \code{sv} or \code{en}. Language of the input name. Defaults to \code{fi}.
#'
#' @return logical
#' @export
#'
#'
check_region_names <- function(x, lang = "fi", year = NULL, offline = TRUE) {

  if(is.vector(x)){
    x <- check_region_names_vct(x, lang = lang, offline = offline, year = year)
  } else if(is.factor(x)) {
    x <- check_region_names_fct(x, lang = lang, offline = offline, year = year)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn check_region_names
#'
#' Check region names of a vector
#'
#' For internal use.
#'
check_region_names_vct <- function(x, lang = "fi", year = NULL, offline = TRUE) {

  logical <- is_region_name(x, lang = lang, offline = offline, year = year)
  if(all(logical)) {
    return(TRUE)
  } else {
    message(paste0("Region classification check: region name(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as standard region name(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
    return(FALSE)
  }
}

#' @describeIn check_region_names
#'
#' Check region names of a factor
#'
#' For internal use.
#'
#'
check_region_names_fct <- function(x, lang = "fi", year = NULL, offline = TRUE) {
  check_region_names_vct(levels(x), lang = lang, offline = offline, year = year)
}



################# CHECK REGION NAME CODE CORRESPONDENCE #################

#' Check if region names and codes correspond as in regionkey.
#'
#' @param data data.frame
#' @param region_name_var character the name of the regional name variable in the data
#' @param region_code_var character the name of the regional code variable in the data
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return logical
#' @export
#'
check_region_name_code_correspondence <- function(region_names, region_codes,
                                                  year = NULL,
                                                  lang = "fi",
                                                  offline = TRUE) {

  suppressMessages(
  if(!(check_region_names(region_names, year = year, lang = lang, offline = offline) &
       check_region_codes(region_codes, year = year, offline = offline))) {
    return(FALSE)
  }
  )

  x <- as.character(region_names) ==
       as.character(codes_to_names(region_codes, year = year, lang = lang, offline = offline))
  if(all(x)) {
    return(TRUE)
  } else {
    message(paste("Region classification check:", paste(unique(paste(region_codes[!x], region_names[!x], sep = "-")), collapse = ", "),
                  "not recognized as correct region name-code pairs."))
    return(FALSE)
  }

}


