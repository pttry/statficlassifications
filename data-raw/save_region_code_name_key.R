# Prepare and save general code-name key

# The idea is to go through all region classifications determined below for all years and look
# for all possible regions. In this way the the key will have all regions that have existed
# in year 2008 and after.

# For municipalities, the key also has their codes without prefixes?



regions <- c("kunta", "maakunta", "seutukunta", "suuralue", "ely")
key <- data.frame()

years <- 2020:2020
for(year in years) {
  key_temp <- get_region_code_name_key(regions, year = year)
  key <- rbind(key, key_temp)
  print(year)
}

key <- key[!duplicated(key), ]
#key <- key[!duplicated(region_code_name_key$alue_code),]

key <- rbind(key, data.frame(alue_code = "SSS", alue_name = "KOKO MAA"))
rownames(key) <- NULL

# Vuonna 2018 ja ennen Ahvenanmaa - Åland, Vuonna 2019 ja jälkeen Ahvenanmaa
key <- dplyr::filter(key, alue_name != "Ahvenanmaa - Åland")

region_code_name_key <- key

usethis::use_data(region_code_name_key, overwrite = TRUE)

