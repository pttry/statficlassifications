#' Standardize region codes
#'
#' TODO: R2D2 IS SET TO SALO CURRENTLY. I.E. USE NUMBERS IN STRINGS MAYBE
#' ONLY IF THERE IS ONLY NUMBERS IN STRINGS
#'
#' To avoid non-unique region codes, use the prefixed region codes.
#' This function "standardizes" region codes in this format.
#' Region codes already in this format are left as they are.
#'
#' The function is strict in not making any assumptions in cases of
#' ambiguous region codes. Some plain numbers may map to multiple
#' region codes and in these cases the user can restrict the domain
#' by giving a set of region levels to whose codes input vector
#' codes are allowed to match.
#'
#' If your vector contains ambiguous codes you may try breaking it
#' into pieces where you know the unique region level of the codes in each
#' piece and treat these pieces separately giving region level as
#' an argument.
#'
#' The function does not create region codes from scratch itself but, inputs
#' are matched to the potential codes in the official classifications.
#' These classifications are accessed using \code{get_regionclassification}-function.
#' Thus, only real region codes can be set. The matching of inputs into
#' potential region codes occurs in \code{match_region_codes_interal}- and
#' \code{match_region_codes}-functions.
#'
#' @param x character vector of region codes
#' @param region_level character, optional region level of the input region codes
#' @param year double, optional year of the classification used
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#' @param use_char_length_info TRUE or named vector, whether to use code character length
#'    information in determining their region level. Defaults to \code{NULL}.
#'
#' @return
#' @export
#'
#' @examples
#'
#'   v <- c("191", "047", "063")
#'   set_region_codes(v)
#'   v <- c("020", "047", "15")
#'   set_region_codes(v, use_char_length_info = TRUE)
#'   v <- c("005", "020", "047", "MK01", "MK02", "MK04")
#'   set_region_codes(v, region_level = "kunta")
#'   f <- factor(c("005", "020", "047", "MK01", "MK02", "MK04"))
#'   set_region_codes(f, region_level = c("kunta"))
#'
#'   # If input is not recognized as region code, tries to reduce it to double
#'   # and infer region from this number. This is how following is possible:
#'   set_region_codes("alue045")
#'
#'   # But it means also that this happens
#'   set_region_codes("lsdf4fsdffs5sfs")
#'
set_region_codes <- function(x,
                             region_level = NULL,
                             year = NULL,
                             offline = TRUE,
                             use_char_length_info = NULL) {

  args <- list(x, region_level = region_level,
               year = year,
               offline = offline,
               use_char_length_info = use_char_length_info)

  if(is.vector(x)){
    x <- do.call(set_region_codes_vct, args)
  } else if(is.factor(x)) {
    x <- do.call(set_region_codes_fct, args)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn set_region_codes Standardize region codes with prefixes. For internal use
#'
set_region_codes_vct <- function(x,
                                 region_level = NULL,
                                 year = NULL,
                                 offline = TRUE,
                                 use_char_length_info = NULL) {

  # Test if region codes are set. If yes, return input as such.
  if(all(is_region_code_with_prefix(x))) {return(x)}

  # Save potential names of the input vector.
  x_names <- names(x)

  # Match the not-set region codes to region codes in classifications.
  suppressMessages(
  new_codes <- match_region_codes(x, offline = offline,
                                  year = year,
                                  region_level = region_level,
                                  use_char_length_info = use_char_length_info)
  )
  # Join the new codes to the old codes by naming.
  names(new_codes) <- x

  # Find non-unique matches and return an error if there are any.
  not_unique <- sapply(new_codes, length) > 1
  if(any(not_unique)) {
    message(paste("Code(s)",
               paste(names(new_codes)[not_unique], collapse = ", "),
               "are ambiguous and left as they were. You can restrict the ambiguity by giving regions to region_level argument or try use character length information."))
  }

  # Find region codes for which matches could not be found.
  not_recognized <- sapply(new_codes, length) == 0

  # Return information on non-matched region codes.
  if(any(not_recognized)) {
    message(paste("Code(s)",
                  paste(x[not_recognized], collapse = ", "),
                  "not recognized as",
                  paste(region_level, collapse = ", "),
                  "region code(s) and are left as they were."))
  }

  not_set <- not_recognized | not_unique

  # Join new names to the input vector.
  new_codes[not_set] <- NA
  join <- dplyr::left_join(data.frame(old_code = as.character(x)),
                           data.frame(old_code = names(new_codes),
                                      new_code = unlist(new_codes)),
                           by = "old_code")
  join <- dplyr::mutate(join, new_code = ifelse(is.na(new_code), old_code, new_code))
  output <- join$new_code

  # Return potential names
  names(output) <- x_names

  if(!all(is_region_code_with_prefix(output))) {
    message(paste("Not all input region codes succesfully set! Not set:",
                  paste(x[not_set], collapse = ", ")))
  }

  # Return
  output
}


#' @describeIn set_region_codes Standardize region codes with prefixes. For internal use
#'
set_region_codes_fct <- function(x,
                                 region_level = NULL,
                                 year = NULL,
                                 offline = TRUE,
                                 use_char_length_info = FALSE) {

  levels(x) <- set_region_codes_vct(levels(x), region_level = region_level,
                                    year = year,
                                    offline = offline,
                                    use_char_length_info = use_char_length_info)

  # Return
  x

}


#' Match to potential region codes
#'
#' Given a region code in "nonstandard" format, matches all "standardized" region
#' codes that it could denote. Works by first reducing the region code to a double
#' and then matching all region codes of all region levels that when reduced to
#' double are the same.
#'
#' If given a region code in "standardized" format, returns the input as such. That is,
#' region codes in standardized format have unique matches.
#'
#' By setting \code{year} you can restrict the domain of potential matches to region
#' codes in use in specific year. When \code{year} is not set, latest classifications
#' are used.
#'
#' By setting \code{region_level} you can restrict the domain of potential matches
#' to specific region levels.
#'
#' @param x input code
#' @param year double, year of classification
#' @param region_level character (vector) region level of the input codes
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#' @param use_char_length_info TRUE or named vector, whether to use code character length
#'    information in determining their region level. Defaults to NULL.
#'
#' @return
#'
#'
match_region_codes <- function(x, year = NULL,
                               region_level = NULL,
                               offline = TRUE,
                               use_char_length_info = NULL) {

  if(is.null(year)) {
    key <- statficlassifications::region_code_to_name_key
  } else {
    key <- get_regionclassification(region_level, year = year, offline = offline)
  }

  names(key) <- c("alue_code", "alue_name")
  key$number <- as.double(sapply(key$alue_code, gsub, pattern = "[^0-9.-]", replacement = ""))
  key$prefix <- sapply(key$alue_code, gsub, pattern = "[^a-zA-Z]", replacement = "")
  if(is.null(year)) {key$number[is.na(key$number)] <- 0}

  lapply(x, match_region_codes_internal,
         key = key,
         region_level = region_level,
         use_char_length_info = use_char_length_info)

}

#'@describeIn match_region_codes Match single region code. For internal use.
#'
match_region_codes_internal <- function(x, key,
                                        region_level = NULL,
                                        use_char_length_info = NULL) {

  if(length(x) != 1) {stop("This function is for one element inputs.")}

  # If input is already a valid region code, return
    if(is_region_code_with_prefix(x)) {return(x)}

  # "000" as a special case is matched to "SSS".
    if(x == "000") {return("SSS")}

  if(!is.null(use_char_length_info)) {
    if(is.logical(use_char_length_info)) {
      if(use_char_length_info == TRUE) {
        char_length_info <- c("maakunta" = 2, "kunta" = 3)
      }
    } else {
      char_length_info <- use_char_length_info
    }
    for(region in names(char_length_info)) {
      if(nchar(x) == char_length_info[region]) {
        region_level <- region
        message(paste("Code(s) of character length",
                                             char_length_info[region],
                                             "interpreted as",
                                             region, "code."))
      }
    }
  }

  x <- gsub(x, pattern = "[^0-9.-]", replacement = "")
  if(!is.null(region_level)) {key <- key[key$prefix %in% name_to_prefix(region_level),]}

  if(x == "") {
    message("No numerics in input, no matches found.")
    return(character(0))
  }

  if(!any(as.double(x) == key$number, na.rm = TRUE)) {
    message("No matches found!")
    return(character(0))
  }

  key$alue_code[as.double(x) == key$number]
}
