#' Transform classifications to keys
#'
#' @param .df
#' @param level_nchars
#'
#' @return
#' @export
#'
#' @examples
classification_to_key <- function(.df, level_nchars = NULL) {

  .df$code_nchar <- nchar(.df$code)

  if(is.null(level_nchars)) {
    level_nchars <- unique(.df$code_nchar)
    level_nchars <- level_nchars[level_nchars != max(level_nchars)]
  }

  i <- 2

  for(.nchar in level_nchars) {

    .df <- mutate(.df, code_temp_name = ifelse(code_nchar %in% .nchar, code, NA)) |>
           fill(code_temp_name, .direction = "down") |>
           rename_with(function(x) paste0("code", i), code_temp_name)

    .df <- mutate(.df, name_temp_name = ifelse(code_nchar %in% .nchar, name, NA)) |>
           fill(name_temp_name, .direction = "down") |>
           rename_with(function(x) paste0("name", i), name_temp_name)

    .df <- filter(.df, !code_nchar %in% .nchar)

    i <- i + 1
  }

  .df$code_nchar <- NULL

  .df
}



# 1) form a column that indicates the number of characters in the code
# 2) extract parts of df with different values in this new column to own dfs
# 3) start with the df with largest number in this new column and form a column
#    for joining by extracting the first x elmements of code-column where x
#    is the number of characters in the table that is be joined.
