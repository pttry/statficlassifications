#' Fixes encoding errors in a data frame
#'
#' When importing Statistics Finland classifications, can be used to fix the potential encoding errors.
#'
#' @param data a data.frame containing encoding errors.
#' @return data.frame Returns a data.frame where the encoding errors in character variables are fixed.
#' @keywords NA-value
#' @export
#' @section Warning:
#' Upper case Ö is missing from the encoding key.
#' @examples

fix_encoding <- function(data) {

  char_vars <- sapply(data, class) == "character"
  encoding_key <- list(c("Ã¶","ö"), c("Ã¤","ä"), c("Ã¥","å"), c("Ã„","Ä"), c("Ã…","Å"))

  for(var in names(data)[char_vars]) {
    for(i in 1:length(encoding_key)) {
      data[var] <- lapply(data[var], gsub, pattern = encoding_key[[i]][1], replacement = encoding_key[[i]][2])
    }
  }
  data
}
