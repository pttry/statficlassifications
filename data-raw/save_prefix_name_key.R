# Create the prefix name correspondence used by the package

key <- c("SSS" = "KOKO MAA",
         "KU" = "kunta",
         "SK" = "seutukunta",
         "MK" = "maakunta",
         "SA" = "suuralue",
         "ELY" = "ely",
         "MA" = "manner_suomi_ahvenanmaa",
         "TK" = "kuntaryhmitys")

prefix_name_key<- data.frame(prefix = names(key), name = key)
rownames(prefix_name_key) <- NULL

usethis::use_data(prefix_name_key, overwrite = TRUE, internal = TRUE)
