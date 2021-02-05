#' Changes region codes to region names
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
#'   v <- c("SSS", "KU103", "KU061","SK213", "MK04")
#'   codes_to_names(v)
#'   v <- c("SSS", "KU103", "KU061","SK213", "MK04", "nav9w4t")
#'   codes_to_names(v)
#'   f <- factor(c("SSS", "KU103", "KU061","SK213", "MK04"))
#'   codes_to_names(f)
#'   df <- data.frame(kunta_code = c("SSS", "KU103", "KU061","SK213", "MK04"), values = rnorm(5))
#'   codes_to_names(df, "kunta_code")
#'   df <- data.frame(kunta_code = c("SSS", "KU103", "KU061","SK213", "MK04", "v43y"), values = rnorm(6))
#'   codes_to_names(df, "kunta_code")
#'
codes_to_names <- function(x, col = NULL) {

   if(is.vector(x)){
      x <- codes_to_names_vct(x)
   } else if(is.factor(x)) {
      x <- codes_to_names_fct(x)
   } else if(is.data.frame(x)) {
      x <- codes_to_names_df(x, col)
   } else {
     stop("Argument not a vector, factor nor a data.frame.")
   }
   x
}

#' @describeIn codes_to_names
#' @export
#'
codes_to_names_vct <- function(x) {



  x_names <- names(x)
  x <- dplyr::left_join(data.frame(alue_code = x),
                        statficlassifications::region_code_name_key,
                        by = "alue_code")$alue_name

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


  names(x) <- x_names
  x

}

#' @describeIn codes_to_names
#' @export
#'
codes_to_names_fct <- function(x) {

levels(x) <- codes_to_names_vct(levels(x))
x

}

#' @describeIn set_region_codes
#' @export
#'
codes_to_names_df <- function(x, col) {

  if(is.vector(x[[col]])) {
    x[[col]] <- codes_to_names_vct(x[[col]])
  } else if(is.factor(x[[col]])) {
    x[[col]] <- codes_to_names_fct(x[[col]])
  }
  x
}



#' Changes region codes to region names
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
#'   v <- c("KOKO MAA", "Humppila", "Forssa","Ålands skärgård", "Satakunta")
#'   names_to_codes(v)
#'   f <- factor(c("KOKO MAA", "Humppila","Ålands skärgård", "Satakunta"))
#'   names_to_codes(f)
#'   df <- data.frame(kunta_name = c("KOKO MAA", "Humppila", "Ålands skärgård", "Satakunta"),
#'                    values = rnorm(4))
#'   names_to_codes(df, "kunta_name")
#
#'
#'
names_to_codes <- function(x, col = NULL) {

  if(is.vector(x)){
    x <- names_to_codes_vct(x)
  } else if(is.factor(x)) {
    x <- names_to_codes_fct(x)
  } else if(is.data.frame(x)) {
    x <- names_to_codes_df(x, col)
  } else {
    stop("Argument not a vector, factor nor a data.frame.")
  }
  x
}

#' @describeIn codes_to_names
#' @export
#'
names_to_codes_vct <- function(x) {

  l <- length(x)
  x_names <- names(x)
  region_code_name_key <- dplyr::filter(statficlassifications::region_code_name_key,
                                        grepl("[a-zA-Z]", alue_code))
  x <- dplyr::left_join(data.frame(alue_name = x),
                        region_code_name_key,
                        by = "alue_name")$alue_code
  names(x) <- x_names
  if(length(x) > l) stop("Some region name(s) can be mapped to multiple region codes!")
  x

}


#' @describeIn codes_to_names
#' @export
#'
names_to_codes_fct <- function(x) {

  levels(x) <- names_to_codes_vct(levels(x))
  x

}

#' @describeIn set_region_codes
#'
names_to_codes_df <- function(x, col) {

  if(is.vector(x[[col]])) {
    x[[col]] <- names_to_codes_vct(x[[col]])
  } else if(is.factor(x[[col]])) {
    x[[col]] <- names_to_codes_fct(x[[col]])
  }
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
  # if(any(tolower(prefix) %in% prefix_name_key$name)) {
  #   return(tolower(prefix))
  # }
  # prefix <- toupper(prefix)
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
  # if(any(toupper(name) %in% prefix_name_key$prefix)) {
  #   return(toupper(name))
  # }
  # name <- ifelse(name != "KOKO MAA", tolower(name), name)
  if(!all(name %in% prefix_name_key$name)) {
    stop(paste0("Unknown region name ", name[!(name %in% prefix_name_key$name)], "."))
  }
  prefix_name_key[prefix_name_key$name %in% name,]$prefix
}
