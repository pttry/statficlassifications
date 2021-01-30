
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
#'   df <- data.frame(kunta_code = c("020", "047", "15", "133"), values = rnorm(4))
#'   set_region_codes(df, "kunta_code")
#'
set_region_codes <- function(x, col = NULL) {

  if(is.vector(x)){
    x <- set_region_codes_vct(x)
  } else if(is.factor(x)) {
    x <- set_region_codes_fct(x)
  } else if(is.data.frame(x)) {
    x <- set_region_codes_df(x, col)
  } else {
    stop("Argument not a vector, factor nor a data.frame.")
  }
  x
}


#' @describeIn set_region_codes
#' @export
#'
set_region_codes_vct <- function(x) {

  x_names <- names(x)
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
                  "not recognized as region codes. They have been left as they were."))
  }

  # For kunta, seutukunta and maakunta, add corresponding prefixes if input does not have
  # prefixes

  for(prefix in prefixes) {
    x[(x %in% codes[[prefix]]) & !grepl(prefix,x)] <- paste0(prefix, x[x %in% codes[[prefix]]])
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
set_region_codes_fct <- function(x) {

  levels(x) <- set_region_codes_vct(levels(x))
  x

}

#' @describeIn set_region_codes
#' @export
#'
set_region_codes_df <- function(x, col) {

  if(is.vector(x[[col]])) {
    x[[col]] <- set_region_codes_vct(x[[col]])
  } else if(is.factor(x[[col]])) {
    x[[col]] <- set_region_codes_fct(x[[col]])
  }
  x
}

