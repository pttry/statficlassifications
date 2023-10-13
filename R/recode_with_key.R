#' Recode classifications using key
#'
#' Given a key, recodes input. In most of the cases is able to recode an input
#' given just a key. That is, the arguments are are the object to be recoded and
#' a key.
#'
#' Keys can be data.frames, named vectors or lists of named vectors.
#'
#' ISSUES: works now by finding for each column a suitable variable in key. But
#' sometimes we need to find the suitable column in the data.frame.
#'
#' @param x vector of elements to be recoded
#' @param key the key to use in recoding
#' @param from source column in key for recoding
#' @param to target column in key for recoding
#' @param x_name name of the vector (variable) to be recoded
#' @param by \code{names}, \code{values} or \code{all_values}. Option
#'    \code{names} uses column names to join key to input. Option
#'    \code{values} looks for the most suitable from-column in the key
#'     by comparing the values in columns. The from-column is chosen as
#'     the one that has most of the values of the input vector. Option
#'     \code{all_values} requires that all values are in the from-column.
#' @param add whether to add (or replace) the original vector to be recoded.
#'     Defaults to \code{FALSE}
#'
#' @return vector or data.frame
#' @export
#'
#' @examples
#'
#'   v <- c("a", "b", "a", "c", "b", "b")
#'
#'   key_data.frame <- data.frame(var1 = letters[1:4],
#'                                var2 = c("first letter",
#'                                         "second letter",
#'                                         "third letter",
#'                                         "fourth letter"))
#'
#'   key_recode(v, key_data.frame, by = "values")
#'   key_recode(key_recode(v, key_data.frame, by = "values"), key_data.frame, by = "values")
#'
#'   key_named_vector <- c("a" = "first letter",
#'                         "b" = "second letter",
#'                         "c" = "third letter",
#'                         "d" = "fourth letter")
#'
#'   key_recode(v, key_named_vector)
#'   key_recode(key_recode(v, key_named_vector), key_named_vector)
#'
#'   f <- factor(c("a", "b", "a", "c", "b", "b"))
#'
#'   key_data.frame <- data.frame(var1 = letters[1:4],
#'                                var2 = c("first letter",
#'                                         "second letter",
#'                                         "third letter",
#'                                         "fourth letter"))
#'
#'   key_recode(f, key_data.frame, by = "values")
#'   key_recode(key_recode(f, key_data.frame, by = "values"), key_data.frame, by = "values")
#'
#'   key_named_vector <- c("a" = "first letter",
#'                         "b" = "second letter",
#'                         "c" = "third letter",
#'                         "d" = "fourth letter")
#'
#'   key_recode(f, key_named_vector, by = "values")
#'   key_recode(key_recode(f, key_named_vector), key_named_vector)
#'
#'
#'   df <- data.frame(var1 = v,
#'                    y = rnorm(6))
#'   key_recode(df, key_data.frame, by = "values")
#'   key_recode(df, key_data.frame, from = "var1", to = "var2")
#'   key_recode(key_recode(df, key_data.frame), key_data.frame)
#'   key_recode(df, key_named_vector)
#'   key_recode(key_recode(df, key_data.frame), key_named_vector)
#'
#'
#'    x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
#'    key <- list(a = c("a1" = "first", "a2" = "second"),
#'                b = c("b1" = "other", "b2" = "something"))
#'    key_recode(x, key)
#'    key_recode(key_recode(x, key), key)
#'
#'    key$c <- key$a
#'    key$a <- NULL
#'    key_recode(x, key, by = "values")
#'    key_recode(x, key, by = "names")
#'
key_recode <- function(x, key,
                       from = NULL, to = NULL,
                       x_name = deparse(substitute(x)),
                       by = "names",
                       add = FALSE) {

  UseMethod("key_recode")

}

#' @describeIn Recode classifications using key
#' @export
key_recode_internal <- function(x, pkey) {

  UseMethod("key_recode_internal")
}

#' @describeIn Recode classifications using key
#' @export
key_recode_internal.default <- function(x, pkey) {

   if(length(pkey$key) == 0 | length(pkey$from) == 0) {
    message(paste("Input", pkey$to, "not recoded."))
    return(x)
  }

  if(length(pkey$from) > 1) stop("A unique from-column in key not found. Use from-argument.")
  if(!all(x %in% pkey$key[[pkey$from]])) message("Some values not in the key! Producing NAs.")
  if(length(unique(pkey$key[[pkey$from]])) < length(pkey$key[[pkey$from]])) {
    warning("Mapping not unique! First instance in key used.")
  }

  pkey$key[pkey$to][match(x, pkey$key[[pkey$from]]),]
  # output <- key[to][unlist(sapply(x, \(x) which(x == key[[from]]), USE.NAMES = FALSE)),]

}

#' @describeIn Recode classifications using key
#' @export
key_recode_internal.factor <- function(x, pkey) {

  levels(x) <- key_recode_internal(levels(x), pkey)
  x
}


#' @describeIn Recode classifications using key
#' @export
key_recode.default <- function(x, key,
                               from = NULL, to = NULL,
                               x_name = deparse(substitute(x)),
                               by = "names",
                               add = FALSE) {

  pkey <- prepare_key(x = x, key = key, from = from, to = to, x_name = x_name, by = by, add = add)
  key_recode_internal(x, pkey)
}

#' @describeIn Recode classifications using key
#' @export
key_recode.factor <- function(x, key,
                              from = NULL, to = NULL,
                              x_name = deparse(substitute(x)),
                              by = "names",
                              add = FALSE) {

  pkey <- prepare_key(x = levels(x), key = key, from = from, to = to, x_name = x_name, by = by, add = add)
  key_recode_internal(x = levels(x), pkey)
}

#' @describeIn Recode classifications using key
#' @export
key_recode.data.frame <- function(x, key,
                                  from = NULL, to = NULL,
                                  x_name = deparse(substitute(x)),
                                  by = "names",
                                  add = FALSE) {

  tibble::tibble(
      data.frame(
        lapply(names(x),
               \(name) {pkey <- prepare_key(x = x[[name]], key = key, from = from, to = to, x_name = name, by = by, add = add)
                        setNames(data.frame(key_recode_internal(x = x[[name]], pkey)), pkey$to)}))
  )
}


#' @describeIn Recode classifications using key
#' @export
statfi_recode <- function(x, key, ...) {
  key_recode(x, key, ...)
}

#' Prepare key for recoding with key
#'
#'
#' If argument from and to are given, selects these columns from the key. If
#' these arguments are not given, chooses the most suitable from-column from the
#' key given either by the name of x-argument or by the values of x-argument.
#'
#' key can be given either as a named vector, data.frame or a list. List is i
#' interpreted as a list of keys and if a list is given, a suitable key from
#' the list is found either by the name of x-argument or by the values of
#' x-argument.
#'
#' Also ensures that the prepared key does not have duplicate rows. Duplicate
#' rows may occur when keys with multiple levels (e.g. regional levels) are
#' are used such that the from-column is some higher level.
#'
#' @param x vector
#' @param key named vector, data.frame or a list
#' @param from source column in ky
#' @param to target column in key
#' @param x_name name of the source column to be decoded
#' @param by \code{names}, \code{values} or \code{all_values}. Controls
#'    how x is matched to from column in key
#'
#' @return list containing the prepared key, from-column name and to-column
#'    name(s).
#' @export
#'
#' @examples
#'
#' key <- data.frame(var1 = letters[1:4],
#'                   var2 = c("first letter",
#'                            "second letter",
#'                            "third letter",
#'                            "fourth letter"),
#'                   var3 = 1:4)
#'
#' prepare_key(key = key, from = "var1", to = "var3")
#'
prepare_key <- function(x = NULL, key,
                        from = NULL, to = NULL,
                        x_name = deparse(substitute(x)),
                        by = "names",
                        add = FALSE) {

   UseMethod("prepare_key", key)

}

#' @describeIn Prepare key for recoding with key
#' @export
prepare_key.default <- function(x = NULL, key,
                                from = NULL, to = NULL,
                                x_name = deparse(substitute(x)),
                                by = "names",
                                add = FALSE) {

  if(is.list(key)) {
    index <- unlist(match_col(x, key, x_name = x_name, by = by))
    if(length(index) == 0) return(failure_return(x_name))
    key <- key[[index]]
  }

  # if key is named vector then matching by name in key is not possible.
  # Match by values then. Also any from and to arguments will not make sense
    key <- setNames(data.frame(names(key),key), paste(x_name, 1:2, sep = "."))
    by <- "values"

    pkey <- prepare_key.data.frame(x = x, key = key, x_name = x_name, by = by, add = add)
    if(length(pkey$key) == 0) return(failure_return(x_name))

    if(length(pkey$to) == 1) {
      new_to <- substring(pkey$to, 1, nchar(pkey$to)-2)
      names(pkey$key)[names(pkey$key) == pkey$to] <- new_to
      pkey$to <- new_to
    }

   pkey
}

#' @describeIn Prepare key for recoding with key
#' @export
#'
#' @examples
#'
#' v <- c("a", "b", "a", "c", "b", "b")
#' df <- data.frame(var1 = v, y = rnorm(6))
#'
#' key <- data.frame(var1 = letters[1:4],
#'                   var2 = c("first letter",
#'                            "second letter",
#'                            "third letter",
#'                            "fourth letter"))
#'
#' prepare_key.data.frame(df, key)
#'
#' key <- data.frame(var1 = letters[1:4],
#'                   var3 = letters[4:1],
#'                   var2 = c("first letter",
#'                           "second letter",
#'                           "third letter",
#'                           "fourth letter"))
#'
#' prepare_key.data.frame(df, key)
#'
prepare_key.data.frame <- function(x = NULL, key,
                                   from = NULL, to = NULL,
                                   x_name = deparse(substitute(x)),
                                   by = "names",
                                   add = FALSE) {

  if(!is.null(from)) {

    if(!from %in% names(key)) {
           stop(paste(from, "not in the key."))}
    #if(from != x_name & by == "names") {
     #      return(failure_return(x_name))}
    if(length(unlist(match_col(key[[from]], x, x_name = from, by = by))) > 1) {
           stop("Column to be recoded not automatically found.")}

  }

  if(is.null(from)) {

    col_matches <- match_col(x, key, x_name = x_name, by = by)
    from <- unlist(col_matches)

    if(any(sapply(col_matches, \(x) {length(x) > 1}))) {
           stop("From-column in key not automatically found.")}
    if(length(from) == 0) {
           return(failure_return(x_name))}
    if(length(from) > 1) {
           stop("Column to be recoded not automatically found.")}

  }


  if(is.null(to)) to <- names(key)[names(key) != from]

  key <- key[,c(from, to)][!duplicated(key),]
  # if(length(to) == 0) to <- x_name
  if(add) to <- unique(c(from, to))

  list(key = key, from = from, to = to)

}

#' @describeIn Prepare key for recoding with key
#' @export
failure_return <- function(x_name) {
  list(key = character(0),
       from = character(0),
       to = x_name)
}


#' Match columns to list elements
#'
#' Find columns in list that match vector by values or by names.
#'
#'
#'
#' @param x vector, factor or data.frame
#' @param df data.frame where matcheds are found
#'
#' @return list with an element for each column in x. Each element contains all
#'   columns that where matched in df.
#' @export
#'
#' @examples
#'
#' v <- c("a", "b", "a", "c", "b", "b")
#' f <- factor(v)
#' df <- data.frame(var1 = v, y = rnorm(6))
#'
#' key <- data.frame(var1 = letters[1:4],
#'                   var2 = c("first letter",
#'                            "second letter",
#'                            "third letter",
#'                            "fourth letter"))
#'
#'  match_col(v, key, by = "values")
#'  var1 <- v
#'  match_col(var1, key)
#'  match_col(f, key, by = "values")
#'  match_col(df, key)
#'  match_col(df, key, by = "values")
#'
#' key <- data.frame(var1 = letters[1:4],
#'                   var3 = letters[4:1],
#'                  var2 = c("first letter",
#'                           "second letter",
#'                           "third letter",
#'                           "fourth letter"))
#'  match_col(v, key, by = "values")
#'  match_col(df, key, by = "names")
#'  match_col(df, key, by = "values")
#'
#'
#'
#'
match_col <- function(x, y, x_name = deparse(substitute(x)), by = "names") {
  UseMethod("match_col")
}

#' @describeIn Match columns to list elements
#' @export
match_col.default <- function(x, y, x_name = deparse(substitute(x)), by = "names") {


  if(by == "values") {
    return(match_col_by_values(x, y, x_name))
  } else if(by == "names") {
    return(match_col_by_names(x, y, x_name))
  } else if(by == "all_values") {
    return(match_col_by_all_values(x, y, x_name))
  } else if(by == "either") {
    return(lapply(mapply(c,
                         match_col_by_values(x, y, x_name),
                         match_col_by_names(x, y, x_name),
                         SIMPLIFY = FALSE),
                  unique))
  }
}

#' @describeIn Match columns to list elements
#' @export
match_col.factor <- function(x, y, x_name = deparse(substitute(x)), by = "names") {
  match_col.default(levels(x), y = y, x_name = x_name, by)
}

#' @describeIn Match columns to list elements
#' @export
match_col.data.frame <- function(x, y, x_name = deparse(substitute(x)), by = "names") {
  x_names <- names(x)
  unlist(lapply(x_names, \(name) {match_col(x[[name]], y = y, x_name = name, by)}), recursive = FALSE)
}


#' @describeIn Match columns to list elements
#' @export
match_col_by_values <- function(x, y, x_name = deparse(substitute(x))) {
  if(!is.list(y)) y <- setNames(list(y), x_name)
  z <- sapply(names(y), function(name) {sum(unique(x) %in% c(y[[name]], names(y[[name]])))})
  if(max(z) == 0) return(setNames(list(character()), x_name))
  setNames(list(names(y)[z == max(z)]), x_name)
}

#' @describeIn Match columns to list elements
#' @export
match_col_by_all_values <- function(x, y, x_name = deparse(substitute(x))) {
  if(!is.list(y)) y <- setNames(list(y), x_name)
  z <- sapply(names(y), function(name) {all(unique(x) %in% c(y[[name]], names(y[[name]])))})
  if(all(!z)) return(setNames(list(character()), x_name))
  setNames(list(names(y)[z]), x_name)
}

#' @describeIn Match columns to list elements
#' @export
match_col_by_names <- function(x, y, x_name = deparse(substitute(x))) {
  if(!is.list(y)) y <- setNames(list(y), x_name)
  setNames(list(intersect(x_name, names(y))), x_name)
}
