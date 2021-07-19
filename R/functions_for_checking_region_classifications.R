# Functions for checking region classifications are divided into functions that
# test whether input is a certain element of classification returning logicals
# functions that check if input vector is ok.


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
    return(paste0("Region code(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as region code(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
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
#' @param lang, \code{fi}, \code{sv} or \code{en}. Language of the input name. Defaults to \code{fi}.
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
    return(paste0("Region name(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as region name(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
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

##################### TEST IF REGION CODE ###########################

#' Check if character is prefixed region code
#'
#' Given a vector of potential region codes, returns a vector of logicals indicating
#' which elements are region codes. Allows prefixed and non-prefixed region codes.
#'
#' Uses functions `is_region_code_with_prefix` and `is_region_code_without_prefix`.
#' These functions are required as such elsewhere.
#'
#'
#' @param x vector, potential region code
#' @param region_level, character, optional region level of the input region codes
#' @param year double, year of region classification searched
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return logical
#' @export
#'
#' @examples
#'
#'   is_region_code(c("KU005", "005"))
#'   is_region_code_with_prefix(c("KU005", "005"))
#'   is_region_code_without_prefix(c("KU005", "005"))
#'   is_region_code_with_prefix(c("KU005", "005"), region_level = "maakunta")
#'   is_region_code_without_prefix(c("KU005", "005"), region_level = "maakunta")
#'
is_region_code <- function(x, region_level = NULL, year = NULL, offline = TRUE) {
  is_region_code_with_prefix(x, region_level = region_level, year = year, offline = offline) |
    is_region_code_without_prefix(x, region_level = region_level, year = year, offline = offline)
}

#' @describeIn is_region_code
#'
#' Check if input is region code with prefix.
#'
#' @export
#'
is_region_code_with_prefix <- function(x, region_level = NULL, year = NULL, offline = TRUE) {

  # Get all standard region codes
  suppressMessages(
     codes <- get_regionclassification(region_level, year = year, offline = offline, only_codes = TRUE)
  )

  # Test if x is in the codes and return
  as.vector(x) %in% codes
}

#' @describeIn is_region_code
#'
#' Check if input is region code without prefix.
#'
#' @export
#'
is_region_code_without_prefix <- function(x, region_level = NULL, year = NULL, offline = TRUE) {

  # Get all standard region codes
  suppressMessages(
    codes <- get_regionclassification(region_level, year = year, offline = offline, only_codes = TRUE)
  )

  # Remove prefixes from standard region codes and remove NAs
  codes <- na.omit(as.double(sapply(codes, gsub, pattern = "[^0-9.-]", replacement = "")))

  # Test if x is in the codes and return
  as.double(as.vector(x)) %in% codes

}

######################### TEST IF REGION NAME ##############################

#' Check if character is region name.
#'
#' Note that by default, the function tests whether input is a name in standard form.
#'
#' @param x character, a vector of potential region names
#' @param region_level character, optional region level of the input region codes
#' @param year double
#' @param offline logical, whether works offline with package data. Defaults to \code{TRUE}.
#' @param allow_nonstandard_names logical, whether to accept a broader set of names as
#'    region names.
#' @param case_sensitive, logical, whether recognition is case sensitive, defaults
#'    to \code{FALSE}
#' @param lang, \code{fi}, \code{sv} or \code{en}. Language of the input name.
#'    Defaults to \code{fi}.
#'
#' @return logical
#' @export
#'
#' @examples
#'
#'  is_region_name(c("Kainuu", "Kainuun maakunta"))
#'  is_region_name(c("Kainuu", "Kainuun maakunta"), allow_nonstandard_names = TRUE)
#'
is_region_name <- function(x,
                           region_level = NULL,
                           year = NULL,
                           offline = TRUE,
                           allow_nonstandard_names = FALSE,
                           case_sensitive = TRUE,
                           lang = "fi") {

  if(lang != "fi") {
    offline <- FALSE
    message("Overriding default option for offline when language other than Finnish required.")
  }

  # Get all standard region names
  suppressMessages(
    names <- get_regionclassification(region_level, year = year, lang = lang,
                                      offline = offline, only_names = TRUE)
  )

  # Potentially add nonstandard region names
  if(allow_nonstandard_names) {
      nonstandard_names <-
        dplyr::filter(statficlassifications::region_name_to_code_key,
                      grepl(paste(name_to_prefix(region_level), collapse = "|"), alue_code))
      names <- c(names, nonstandard_names$alue_name)
  }

  # Test which of the inputs are in the got names depending on whether case sensitivity
  # is required and return
  ifelse(case_sensitive,
           x %in% names,
           tolower(x) %in% tolower(names))

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

