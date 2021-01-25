# Prepare code-name key

# For municipalities, the key also has their codes without prefixes.

regions1 <- c("kunta")
regions2 <- c("seutukunta", "maakunta", "suuralue", "ely")

codes_names_key1 <- purrr::map(regions1, get_region_code_name_key, year = 2020, offline = FALSE) %>%
  purrr::map(setNames, c("alue_name", "alue_code")) %>%
  plyr::ldply()
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
