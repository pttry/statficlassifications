#' Import region classification key
#'
#' Imports NUTS-region classification keys from Statistics Finland API. Use together with 'dplyr::left_join'
#' to add regions to data. A wrapper for the \code{get_key} function that it calls under the hood.
#'
#' @param region character, the smallest region desired in the resulting classification key.
#' @param only_codes logical, whether the key should contain only the region codes. Defaults to FALSE.
#' @param only_names logical, whether the key should contain only the region names. Defaults to FALSE.
#' @param year character or numerical, the year of the desired classification key.
#' @return data.frame Returns a classification key as a data.frame.
#' @export
#' @examples
#'
#' regionkey <- get_regionkey(year = 2020)
#'

get_regionkey <- function(region = "kunta", only_codes = FALSE, only_names = FALSE, year) {

  alueet <- factor(c("kunta", "seutukunta", "maakunta", "suuralue"), levels = c("kunta", "seutukunta", "maakunta", "suuralue"))
  hallintoalueet <- c("seutukunta", "maakunta", "suuralue")
  hallintoalueet_codes <- c("SK", "MK", "SA")
  source_region = "kunta"
  hallintoaluekey <- data.frame()

  for(target_region in hallintoalueet) {

    localID <- create_localID_name(source_region, target_region, year)
    key <- get_key(localID)

    # The codes in classification tables have only the numbers, not the region marker (e.g. MK, SK). Add
    # these region markers.

      key$source_code <- paste0("KU", key$source_code)
      key$target_code <- paste0(hallintoalueet_codes[which(hallintoalueet == target_region)], key$target_code)

    # set the variable names, codes get prefix '_code' and names get prefix '_name'. e.g. '(maa)kunta_name'
    # and '(maa)kunta_code'.

    names(key) <- c("kunta_code", "kunta_name", paste(target_region, c("code", "name"), sep = "_"))

    assign(paste(target_region, "key", sep = "_"), key)

  }

  regionkey <- dplyr::left_join(seutukunta_key, maakunta_key, by = c("kunta_code", "kunta_name"))
  regionkey <- dplyr::left_join(regionkey, suuralue_key, by = c("kunta_code", "kunta_name"))

  if(only_codes) {
    regionkey <- dplyr::select(regionkey, contains("code"))
  } else if(only_names) {
    regionkey <- dplyr::select(regionkey, contains("name"))
  }
  regionkey <- regionkey[,as.double(alueet[alueet == region]) <= as.double(alueet)]
  regionkey[!duplicated(regionkey),]
  dplyr::mutate_all(regionkey, as.factor)

}
