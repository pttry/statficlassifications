#' Check if string is region code
#'
#' Given a vector of potential region codes, returns a vector of logicals indicating
#' which elements are region codes. Allows prefixed and non-prefixed region codes.
#'
#' @details
#' Uses `is_region_code_with_prefix()` and `is_region_code_without_prefix()`.
#' These functions are required as such also elsewhere.
#'
#' Region variable detection functions use the testing functions here.
#'
#'
#' @param x vector, a potential region code.
#' @param region_level character, optional region level of the input region codes.
#'    Possible `region_level`s are `statficlassifications:::prefix_name_key$name`.
#' @param year double, year of region classification searched.
#' @param offline logical, whether works offline with package data. Defaults to `TRUE`.
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

#' @rdname is_region_code
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

#' @rdname is_region_code
#' @export
#'
is_region_code_without_prefix <- function(x, region_level = NULL, year = NULL, offline = TRUE) {

  # Get all standard region codes
  suppressMessages(
    codes <- get_regionclassification(region_level, year = year, offline = offline, only_codes = TRUE)
  )

  # Remove prefixes from standard region codes and remove NAs
  codes <- stats::na.omit(as.double(sapply(codes, gsub, pattern = "[^0-9.-]", replacement = "")))

  # Test if x is in the codes and return
  suppressWarnings(as.double(as.vector(x)) %in% codes)

}

#' Check if string is region name
#'
#' By default, the tests whether input is a name in standard form. Non-standard
#' forms can be tested by setting `allow_nonstandard_names = TRUE`.
#'
#' Region variable detection functions use the testing functions here.
#'
#' @param x character, a vector of potential region names.
#' @param region_level character, optional region level of the input region codes.
#'    Possible `region_level`s are `statficlassifications:::prefix_name_key$name`.
#' @param year double, year of region classification searched.
#' @param offline logical, whether works offline with package data. Defaults to `TRUE`.
#' @param allow_nonstandard_names logical, whether to accept a broader set of names as
#'    region names.
#' @param case_sensitive logical, whether recognition is case sensitive, defaults
#'    to `FALSE`.
#' @param lang `"fi"`, `"sv"` or `"en"`. Language of the input name.
#'    Defaults to `"fi"`.
#'
#' @return logical
#' @export
#'
#' @examples
#'
#'  is_region_name(c("Kainuu", "Kainuun maakunta", "Kajjjaani"))
#'  is_region_name(c("Kainuu", "Kainuun maakunta", "Kajjjaani"), allow_nonstandard_names = TRUE)
#'
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

  # If case sensitivity is not required, make everything lower case
  if(!case_sensitive) {x <- tolower(x); names <- tolower(names)}

  # Return
  x %in% names
}

