# Prepare code-name key

# For municipalities, the key also has their codes without prefixes.

regions1 <- c("kunta", "maakunta")
regions2 <- c("seutukunta", "suuralue", "ely")

codes_names_key <- data.frame()
years <- 2020:2020
for(year in years) {
codes_names_key_temp <- purrr::map(regions1, get_region_code_name_key, year = year, offline = FALSE) %>%
  purrr::map(setNames, c("alue_name", "alue_code")) %>%
  plyr::ldply()
codes_names_key <- rbind(codes_names_key, codes_names_key_temp)
print(year)
}

codes_names_key1 <- distinct(codes_names_key)

codes_names_key3 <- purrr::map(regions2, get_region_code_name_key, year = 2020, offline = FALSE) %>%
  purrr::map(setNames, c("alue_name", "alue_code")) %>%
  plyr::ldply()
codes_names_key2 <- codes_names_key1
codes_names_key2$alue_code <- sapply(codes_names_key1$alue_code, gsub, pattern = "[^0-9.-]", replacement = "")

region_code_name_key <- rbind(data.frame(alue_name = rep("KOKO MAA",2), alue_code = c("SSS", "000")),
                         codes_names_key1,
                         codes_names_key2,
                         codes_names_key3)


usethis::use_data(region_code_name_key, overwrite = TRUE)


# region_code_name_key[duplicated(region_code_name_key$alue_code),]
