# Create the prefix name correspondence used by the package

prefix_name_key <- data.frame(prefix = c("SSS", "KU", "SK", "MK", "ELY", "SA"),
                              name = c("KOKO MAA", "kunta", "seutukunta", "maakunta", "ely", "suuralue"))

usethis::use_data(prefix_name_key, overwrite = TRUE)
