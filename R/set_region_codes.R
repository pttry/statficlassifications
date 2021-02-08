
#' Standardize region codes with prefixes
#'
#' @param x character vector of region codes
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'   v <- c("020", "047", "15", "133")
#'   set_region_codes(v)
#'   v <- c("Akaa" = "020", "Enontekiö" = "047", "Pohjanmaa" = "15", "Keuruu" =  "133")
#'   set_region_codes(v)
#'   v <- c("020" = "Akaa", "KU047" = "Enontekiö", "15" = "Pohjanmaa", "SK133" = "Keuruu")
#'   set_region_codes(names(v))
#'   f <- factor(c("020", "047", "15", "133"))
#'   set_region_codes(f)
#'
set_region_codes <- function(x, region_level = NULL, col = NULL) {

  if(is.vector(x)){
    x <- set_region_codes_vct(x, region_level = region_level)
  } else if(is.factor(x)) {
    x <- set_region_codes_fct(x, region_level = region_level)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn set_region_codes
#' @export
#'
set_region_codes_vct <- function(x, region_level = NULL) {

  x_names <- names(x)

  if(is.null(region_level)) {
    if(is.numeric(x)) {
      stop("For numeric region codes, please set region_level argument to kunta, seutukunta, maakunta or suuralue.")
    }
  # construct a list with all kunta, seutukunta, maakunta codes without prefixes
  abolished_mun_key <- statficlassifications::abolished_mun_key
  regionkey <- statficlassifications::regionkey
  prefixes <- c("KU", "MK", "SK")
  codes <- lapply(list(unique(abolished_mun_key$joiner),
                       unique(regionkey$maakunta_code),
                       unique(regionkey$seutukunta_code)), gsub, pattern = "[^0-9.-]", replacement = "")
  names(codes) <- prefixes

  if(any(x %in% codes$SK[codes$SK %in% codes$KU]))  {
    warning(paste("Code(s)", paste(x[x %in% codes$SK[codes$SK %in% codes$KU]], collapse = ", "),
                  "could have been interpreted as seutukunta code(s), but have been interpreted as municipality code(s)."))
  }

  if(!all(x %in% unlist(codes))) {
    warning(paste("Code(s)",
                  paste(x[!(x %in% unlist(codes))], collapse = ", "),
                  "not recognized as region codes. They have been left as they were.
  To manually set prefixes set region_level argument to 'KU', 'SK', 'MK' or 'SA'"))
  }

  # For kunta, seutukunta and maakunta, add corresponding prefixes if input does not have
  # prefixes

  for(prefix in prefixes) {
    x[(x %in% codes[[prefix]]) & !grepl(prefix,x)] <- paste0(prefix, x[x %in% codes[[prefix]]])
  }

  } else {
    x <- numeric_code_to_character(x, region_level_prefix = name_to_prefix(region_level))
    x <- paste0(name_to_prefix(region_level), x)
  }

# Tranform any "000"s to "SSS"s
if(any(grepl("000", x))) {
  x[x%in% c("000")] <- "SSS"
}
  names(x) <- x_names
  x

}


#' @describeIn set_region_codes
#' @export
#'
set_region_codes_fct <- function(x, region_level = NULL) {

  levels(x) <- set_region_codes_vct(levels(x), region_level = region_level)
  x

}


#' Title
#'
#' @param x
#' @param region_level_prefix
#'
#' @return
#' @export
#'
#' @examples
numeric_code_to_character <- function(x, region_level_prefix) {
     if(region_level_prefix %in% c("KU", "SK")) {
       char_length <- 3
     } else {
       char_length <- 2
     }
     stringr::str_sub(paste0("000", as.character(x)), -char_length,-1)
}


# if(any(grepl("^\\d+$", x))) {
#   message(cat("It appears you are trying to map codes without prefixes to regions.
#           In this case, same codes may map to multiple regions so there is a risk
#           errors in mapping. I will start by first trying to fit kunnat to
#           to your codes and then seutukunnat, then maakunnat then suuralueet and
#           then ely-alueet."))
#   for(prefix in prefix_name_key$prefix[prefix_name_key$prefix != "SSS"]) {
#      key <- statficlassifications::region_code_name_key
#      key <- dplyr::filter(key, grepl(prefix, key$alue_code))
#      key$alue_code <- unlist(sapply(key$alue_code, stringr::str_remove, pattern = prefix))
#   }
# }
