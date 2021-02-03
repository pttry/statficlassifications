# Create the nonstandard region names key

key <- c("koko suomi" = "SSS",
         "uudenmaan maakunta" = "MK01",
         "itä-uudenmaan maakunta" = "MK20",
         "varsinais-suomen maakunta" = "MK02",
         "satakunnan maakunta" = "MK04",
         "kanta-hämeen maakunta" = "MK05",
         "pirkanmaan maakunta" = "MK06",
         "päijät-hämeen maakunta" = "MK07",
         "kymenlaakson maakunta" = "MK08",
         "etelä-karjalan maakunta" = "MK09",
         "etelä-savon maakunta" = "MK10",
         "pohjoissavon maakunta" = "MK11",
         "pohjois-karjalan maakunta" = "MK12",
         "keski-suomen maakunta" = "MK13",
         "etelä-pohjanmaan maakunta" = "MK14",
         "pohjanmaan maakunta" = "MK15",
         "keski-pohjanmaan maakunta" = "MK16",
         "pohjois-pohjanmaan maakunta" = "MK17",
         "kainuun maakunta" = "MK18",
         "lapin maakunta" = "MK19",
         "ahvenanmaa - åland" = "MK21")

nonstandard_region_names_key <- data.frame(alue_name = names(key), alue_code = key)
rownames(nonstandard_region_names_key) <- NULL

usethis::use_data(nonstandard_region_names_key, internal = FALSE, overwrite = TRUE)
