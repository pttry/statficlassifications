#' Change region codes to region names
#'
#' Works with standardized region codes. A good practice is to first standardize
#' your region codes using `set_region_codes`-function. This `codes_to_names`,
#' however checks if the region codes are standardized and applies `set_region_codes`
#' if they are not.
#'
#' @param x a character (vector) of region codes
#' @param region_level character, optional region level of the input region codes
#' @param use_char_length_info TRUE or named vector, whether to use code character length
#'    information in determining their region level. Defaults to NULL.
#' @param year integer, the year of the applied classification key.
#' @param lang \code{"fi"}, \code{"sv"}, \code{"en"}. Language of output names.
#'    Defaults to \code{"fi"}.
#' @param offline logical, whether works offline with package data. Defaults to \code{TRUE}.
#' @param region_codes_check logical, whether tries to set standard region codes.
#'    Defaults to \code{TRUE}.
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
codes_to_names <- function(x, region_level = NULL,
                           use_char_length_info = NULL,
                           year = NULL,
                           lang = "fi",
                           offline = TRUE,
                           region_codes_check = TRUE) {

  if(region_codes_check) {
   if(any(!is_region_code_with_prefix(x))) {
     message("Tried to add prefixes to your input codes.")
     x <- set_region_codes(x, region_level = region_level, use_char_length_info = use_char_length_info)
   }
  }

  args <- list(x = x, year = year, lang = lang, offline = offline)

   if(is.vector(x)){
      x <- do.call(codes_to_names_vct, args)
   } else if(is.factor(x)) {
      x <- do.call(codes_to_names_fct, args)
   } else {
     stop("Argument not a vector or factor.")
   }
   x
}

#´
#
#' @describeIn codes_to_names
#'
#' Change region codes to region names
#'
#' For internal use
#'
codes_to_names_vct <- function(x, year = NULL, lang = "fi", offline = TRUE) {

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


  names(key) <- c("alue_code", "alue_name")
  output <- dplyr::left_join(data.frame(alue_code = x), key,
                        by = "alue_code")$alue_name

  if(any(is.na(output))) {
    warning(paste("Code(s)", paste(x[is.na(output)], collapse = ", "), "not recognized as a region code(s) and given NA."))
  }
  names(output) <- x_names
  output

}

#' @describeIn codes_to_names
#'
#' Change region codes to region names
#'
#' For internal use.
#'
codes_to_names_fct <- function(x, year = NULL, lang = "fi", offline = TRUE) {

levels(x) <- codes_to_names_vct(levels(x), year = year, lang = lang, offline = offline)
x

}


#' Change region names to region codes
#'
#' @param x character vector of factor of region codes.
#' @param year integer, the year of the applied classification key.
#' @param lang, \code{fi}, \code{sv} or \code{en}. Input language. Defaults to \code{fi}.
#' @param offline logical, whether works offline with package data. Defaults to \code{TRUE}.
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
names_to_codes <- function(x, year = NULL, lang = "fi", offline = TRUE, region_level = NULL) {

  args <- list(x, year = year, lang = lang, offline = offline, region_level = region_level)

  if(is.vector(x)){
    x <- do.call(names_to_codes_vct, args)
  } else if(is.factor(x)) {
    x <- do.call(names_to_codes_fct, args)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}



#' @describeIn names_to_codes
#'
#' Change region names to region codes
#'
#' For internal use.
#'
names_to_codes_vct <- function(x,
                               year = NULL,
                               lang = "fi",
                               offline = TRUE,
                               region_level = NULL) {

  x_names <- names(x)

  key <- get_regionclassification(region_level, year = year,
                                  lang = lang, offline = offline)

  names(key) <- c("alue_code", "alue_name")
  output <- dplyr::left_join(data.frame(alue_name = x), key,
                        by = "alue_name")$alue_code

  if(length(output) > length(x)) {
   stop("Some region name(s) can be mapped to multiple region codes!
  You may want to have only one region level in your input vector
  and use the region_level argument to give more information.")
  }
  if(any(is.na(output))) {
    warning(paste("Name(s)", paste(x[is.na(output)], collapse = ", "),
                  "not recognized as a region name(s) in language",
                  lang, "and is given NA."))
  }

  names(output) <- x_names
  output
}



#' @describeIn names_to_codes
#'
#' #' Recode name to codes in factors
#'
#' For internal use.
#'
names_to_codes_fct <- function(x, year = NULL, lang = "fi", offline = TRUE, region_level = NULL) {

  levels(x) <- names_to_codes_vct(levels(x), year = year, lang = lang, offline = offline, region_level = region_level)
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
