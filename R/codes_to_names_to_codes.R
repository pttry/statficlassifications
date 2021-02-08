#' Changes region codes to region names
#'
#' @param data data.frame, the input data that contains a variable of region codes.
#' @param region character, the name of the variable of region codes.
#' @param year integer, the year of the applied classification key.
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
codes_to_names <- function(x, year = NULL, offline = TRUE) {

  if(is.numeric(x)) {
    message("Input codes are numeric, this function currently does nothing to them.")
    return(x)
  }

   if(is.vector(x)){
      x <- codes_to_names_vct(x, year = year, offline = offline)
   } else if(is.factor(x)) {
      x <- codes_to_names_fct(x, year = year, offline = offline)
   } else {
     stop("Argument not a vector or factor.")
   }
   x
}

#' @describeIn codes_to_names
#' @export
#'
codes_to_names_vct <- function(x, year = NULL, offline = TRUE) {

  x_names <- names(x)

  if(offline) {
    key <- statficlassifications::region_code_to_name_key
  } else {
    prefixes <- unique(sapply(unique(x), gsub, pattern = "[^a-zA-Z]", replacement = ""))
    key <- get_region_code_name_key(prefixes, year = year)
  }

  output <- dplyr::left_join(data.frame(alue_code = x), key,
                        by = "alue_code")$alue_name

  if(any(is.na(output))) {
    warning(paste("Code(s)", paste(x[is.na(output)], collapse = ", "), "not recognized as a region code(s) and given NA."))
  }
  names(output) <- x_names
  output

}

#' @describeIn codes_to_names
#' @export
#'
codes_to_names_fct <- function(x, year = NULL, offline = TRUE) {

levels(x) <- codes_to_names_vct(levels(x), year = year, offline = offline)
x

}


#' Changes region names to region codes
#'
#' A wrapper that uses the statficlassifications::recode-function.
#'
#' @param data data.frame, the input data that contains a variable of region codes.
#' @param region character, the name of the variable of region codes.
#' @param year integer, the year of the applied classification key.
#'
#' @return data.frame
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
names_to_codes <- function(x, year = NULL, offline = TRUE, region_level = NULL) {

  if(is.vector(x)){
    x <- names_to_codes_vct(x, year = year, offline = offline, region_level = region_level)
  } else if(is.factor(x)) {
    x <- names_to_codes_fct(x, year = year, offline = offline, region_level = region_level)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn names_to_codes
#' @export
#'
names_to_codes_vct <- function(x, year = NULL, offline = TRUE, region_level = NULL) {

  x_names <- names(x)
  prefixes <- prefix_name_key$prefix

  if(!is.null(region_level)) {
    prefixes <- c(name_to_prefix(tolower(region_level)), "SSS")
  }

  if(offline) {
    key <- statficlassifications::region_name_to_code_key %>%
           filter(grepl(paste(prefixes, collapse = "|"), alue_code))
  } else {
    key <- get_region_code_name_key(prefixes, year = year)
  }

  output <- dplyr::left_join(data.frame(alue_name = x), key,
                        by = "alue_name")$alue_code

  if(length(output) > length(x)) {
   stop("Some region name(s) can be mapped to multiple region codes!
  You may want to have only one region level in your input vector
  and use the region_level argument to give more information.")
  }
  if(any(is.na(output))) {
    warning(paste("Name(s)", paste(x[is.na(output)], collapse = ", "), "not recognized as a region name(s) and given NA."))
  }

  names(output) <- x_names
  output
}


#' @describeIn names_to_codes
#' @export
#'
names_to_codes_fct <- function(x, year = NULL, offline = TRUE, region_level = NULL) {

  levels(x) <- names_to_codes_vct(levels(x), year = year, offline = offline, region_level = region_level)
  x

}

#' Change region prefixes to names
#'
#' @param prefix, region level code prefix
#'
#' @return region name
#' @export
#'
#' @examples
#'
#'  prefix_to_name("SK")
#'
prefix_to_name <- function(prefix) {

  if(!all(prefix %in% prefix_name_key$prefix)) {
    stop(paste0("Unknown region code prefix ", prefix[!(prefix %in% prefix_name_key$prefix)], "."))
  }
  prefix_name_key[prefix_name_key$prefix %in% prefix,]$name
}

#' Change region names to prefixes
#'
#' @param name, region level name
#'
#' @return region prefix
#' @export
#'
#' @examples
#'
#'  name_to_prefix("seutukunta")
#'
name_to_prefix <- function(name) {

  if(!all(name %in% prefix_name_key$name)) {
    stop(paste0("Unknown region name ", name[!(name %in% prefix_name_key$name)], "."))
  }
  prefix_name_key[prefix_name_key$name %in% name,]$prefix
}
