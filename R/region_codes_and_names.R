# Functions to recode region codes to region names and vice versa. Defines two
# functions that are exported: `codes_to_names` and `names_to_codes`.

# This file also contains functions to recode between prefixes and region levels:
# `prefix_to_name()` and `name_to_prefix()`.


#' Change region codes to region names
#'
#' Why do these functions exist? Replace by recode_region? Also:
#'
#' key_recode(v, get_regionclassification(), by = "values")
#'
#' Works with standardized region codes. A good practice is to first standardize
#' your region codes using `set_region_codes()`. `codes_to_names()`,
#' however can check if the region codes are standardized and applies `set_region_codes()`
#' if they are not.
#'
#' @param x a character (vector) of region codes
#' @param region_level character, optional region level of the input region codes
#' @param use_char_length_info TRUE or named vector, whether to use code character length
#'    information in determining their region level. Defaults to NULL.
#' @param year integer, the year of the applied classification key.
#' @param lang `"fi"`, `"sv"` or `"en"`. Language of output names.
#'    Defaults to `"fi"`.
#' @param offline logical, whether works offline with package data. Defaults to `TRUE`.
#' @param set_region_codes logical, whether tries to set standard region codes.
#'    Defaults to `FALSE`.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'   v <- c("SSS", "KU103", "KU061","SK213", "MK04")
#'   codes_to_names(v)
#'   v <- c("SSS", "KU103", "KU061","SK213", "MK04", "nav9w4t")
#'   codes_to_names(v)
#'   f <- factor(c("SSS", "KU103", "KU061","SK213", "MK04"))
#'   codes_to_names(f)
#'
codes_to_names <- function(x, year = NULL,
                           region_level = NULL,
                           use_char_length_info = NULL,
                           set_region_codes = FALSE,
                           lang = "fi", offline = TRUE) {

    UseMethod("codes_to_names")
}


#' @export
codes_to_names.default <- function(x, year = NULL,
                                   region_level = NULL,
                                   use_char_length_info = NULL,
                                   set_region_codes = FALSE,
                                   lang = "fi", offline = TRUE) {

  # If required, check if the the input contains region codes that are not
  # in the standardized form
  if(set_region_codes) {
    if(any(!is_region_code_with_prefix(x))) {
      message("Tried to add prefixes to your input codes.")
      x <- set_region_codes(x, region_level = region_level,
                            use_char_length_info = use_char_length_info)
    }
  }

  # Save potential names to add later back to output
  x_names <- names(x)

  # Find all valid unique prefixes in input region codes to find all required
  # region levels
  prefixes <- unique(sapply(unique(x), gsub, pattern = "[^a-zA-Z]", replacement = ""))
  prefixes <- prefixes[prefixes %in% prefix_name_key$prefix]
  region_levels <- prefix_to_name(prefixes, pass_unknown = TRUE)

  # Get a code-to-name classification for required region levels
  key <- get_regionclassification(region_levels, year = year,
                                  lang = lang, offline = offline)

  # Join names to codes
  names(key) <- c("alue_code", "alue_name")
  output <- dplyr::left_join(data.frame(alue_code = x), key,
                        by = "alue_code")$alue_name

  # Give a warning if some given codes could not be recoded to name and is given NA
  if(any(is.na(output))) {
    message(paste("Code(s)", paste(unique(x[is.na(output)]), collapse = ", "),
                  "not recognized as a region code(s) and given NA."))
  }

  # Return potential names
  names(output) <- x_names

  # Return
  output

}

#' @export
codes_to_names.factor <- function(x, year = NULL,
                                  region_level = NULL,
                                  use_char_length_info = NULL,
                                  set_region_codes = FALSE,
                                  lang = "fi", offline = TRUE) {

levels(x) <- codes_to_names.default(levels(x), year = year, lang = lang, offline = offline)
x

}

#' Change region names to region codes
#'
#' @param x character vector of factor of region codes.
#' @param year integer, the year of the applied classification key.
#' @param lang `"fi"`, `"sv"` or `"en"`. Input language. Defaults to `"fi"`.
#' @param offline logical, whether works offline with package data. Defaults to `TRUE`.
#' @param region_level character, region level of input codes, optional.
#'
#' @return vector or factor
#' @export
#'
#' @examples
#'
#'   v <- c("KOKO MAA", "Humppila", "Ålands skärgård", "Satakunta")
#'   names_to_codes(v)
#'   names_to_codes("Kajaani", region_level = "seutukunta")
#'   names_to_codes(c("KOKO MAA", "Kajaani"), region_level = "seutukunta")
#'   f <- factor(c("KOKO MAA", "Humppila","Ålands skärgård", "Satakunta"))
#'   names_to_codes(f)
#'
names_to_codes <- function(x,
                           year = NULL,
                           lang = "fi",
                           offline = TRUE,
                           region_level = NULL) {

  UseMethod("names_to_codes")
}

#' @export
names_to_codes.default <- function(x,
                               year = NULL,
                               lang = "fi",
                               offline = TRUE,
                               region_level = NULL) {

  # Save potential names to add later back to output
  x_names <- names(x)

  # Get names-to-codes mapping
  key <- get_regionclassification(region_level, year = year,
                                  lang = lang, offline = offline)

  # Join codes to names using the names-to-codes mapping got above
  names(key) <- c("alue_code", "alue_name")
  output <- dplyr::left_join(data.frame(alue_name = x), key,
                        by = "alue_name")$alue_code

  # Stop if ambiguity in name-to-code mapping
  if(length(output) > length(x)) {
   stop("Some region name(s) can be mapped to multiple region codes!
  You may want to have only one region level in your input vector
  and use the region_level argument to give more information.")
  }

  # Give a warning if some given names could not be recoded to codes and is given NA
  if(any(is.na(output))) {
    message(paste("Name(s)", paste(x[is.na(output)], collapse = ", "),
                  "not recognized as a region name(s) in language",
                  lang, "and is given NA."))
  }

  # Return potential names
  names(output) <- x_names

  # Return
  output
}

#' @export
names_to_codes.factor <- function(x, year = NULL, lang = "fi", offline = TRUE, region_level = NULL) {

  levels(x) <- names_to_codes.default(levels(x), year = year, lang = lang,
                                  offline = offline, region_level = region_level)
  x

}

#' Change region prefixes to names
#'
#' @param prefix region level code prefix
#' @param pass_unknown If TRUE pass unknown prefix as prefix. Not in use
#'
#'
#' @return region name
#' @export
#'
#' @examples
#'
#'  prefix_to_name("SK")
#'
prefix_to_name <- function(prefix, pass_unknown = FALSE) {

  # if(!pass_unknown & !all(prefix %in% prefix_name_key$prefix)) {
  if(!all(prefix %in% prefix_name_key$prefix)) {
    stop(paste0("Unknown region code prefix ", prefix[!(prefix %in% prefix_name_key$prefix)], "."))
  }

  if (is.null(prefix)) return(NULL)

  # dplyr::coalesce(prefix_name_key$name[match(prefix, prefix_name_key$prefix)], prefix)
  prefix_name_key$name[prefix_name_key$prefix %in% prefix]
}


#' Change region names to prefixes
#'
#' @param name region level name
#' @param pass_unknown If TRUE pass unknown names as names.
#'
#' @return region prefix
#' @export
#'
#' @examples
#'
#'  name_to_prefix("seutukunta")
#'
name_to_prefix <- function(name, pass_unknown = FALSE) {

  if(!pass_unknown & !all(name %in% prefix_name_key$name)) {
    stop(paste0("Unknown region name ", name[!(name %in% prefix_name_key$name)], "."))
  }

  if (is.null(name)) return(NULL)

  # pass names that are not in prefix_name_key
  dplyr::coalesce(prefix_name_key$prefix[match(name, prefix_name_key$name)], name)
}
