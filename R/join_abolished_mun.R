#' Join abolished municipalities
#'
#' Takes a vector of municipality codes with KU-prefixes and transforms the codes of
#' the abolished municipalities to the codes of the municipality to which they have
#' joined.
#'
#' @param x an object containing municipality codes, a vector, a factor or a data.frame.
#' @param col name of column in case input is data.frame
#'
#' @return vector, factor or data.frame, depending on input.
#' @export
#'
#' @examples
#'
#'   v <- c("KU414", "KU609", "KU429", "KU272")
#'   join_abolished_mun(x = v)
#'   f <- factor(c("KU414", "KU609", "KU429", "KU272"))
#'   join_abolished_mun(f)
#'   df <- data.frame(kunta_code =  c("KU414", "KU609", "KU429", "KU272"), values = rnorm(4))
#'   join_abolished_mun(df, "kunta_code")
#'   df <- data.frame(kunta_code =  factor(c("KU414", "KU609", "KU429", "KU272")), values = rnorm(4))
#'   join_abolished_mun(df, "kunta_code")
#'
join_abolished_mun <- function(x, col = NULL) {

  UseMethod("join_abolished_mun")

}

#' @export
join_abolished_mun.default <- function(x) {

  if(!any(grepl("KU", x))) {
    stop("This function understands only prefixed municipality codes! Use set_region_codes() to standardize your codes")
  }

  # Save potential names
  x_names <- names(x)

  # Load a vector with all past and current municipality codes
  abolished_mun_key <- statficlassifications::abolished_mun_key
  kunta_codes <- abolished_mun_key$joiner

  # From the argument, extract elements that are past or current
  # municipality codes
  kunta_names <- x[x %in% kunta_codes]

  # Use abolished_mun_key to create a vector of corresponding current codes
  new_kunta <- dplyr::left_join(data.frame(joiner = kunta_names), abolished_mun_key, by = "joiner")$joinee
  if(length(new_kunta) > length(x)) {stop("Let Juho know about this error!")}

  # Let user know what the function did
  joined_lgl <- !x %in% new_kunta

  if(any(joined_lgl)) {
   message(paste0("Joined ",
                  paste0(x[joined_lgl],
                " to ",
                new_kunta[joined_lgl], collapse = ", "), "."))
  } else {
    message("No abolished municipalities to join.")
  }

   # Assign the newly created vector to the position of the original codes
   x[x %in% kunta_codes] <- new_kunta

  names(x) <- x_names
  x

}

#' @export
join_abolished_mun.factor <- function(x) {
  levels(x) <- join_abolished_mun.default(levels(x))
  x
}


#' @export
join_abolished_mun.data.frame <- function(x, col) {
  x[[col]] <- join_abolished_mun(x[[col]])
  x
}
