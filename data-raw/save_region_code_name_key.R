# Prepare and save general code-name key

# The idea is to go through all region classifications determined below for all years and look
# for all possible regions. In this way the the key will have all regions that have existed
# in year 2008 and after.

# A key only works if it is a function. That is, it maps all possible codes to unique names.
# This means that the region to code key can map each region to a single code and the code
# to region key can map each code to a single region.

# Region code to name key
   # Mapping from codes to names

regions <- c("kunta", "maakunta", "seutukunta", "suuralue", "ely")
master_key <- data.frame()

years <- 2008:2020
for(year in years) {
  master_key_temp <- get_region_code_name_key(regions, year = year)
  master_key_temp$year <- year
  master_key <- rbind(master_key, master_key_temp)
  print(year)
}

  key <- master_key
# Remove duplicates due to repetition of years
  key <- key[!duplicated(dplyr::select(key, -year)), ]

# Find duplicated codes and remove them from the key
  duplicates <- dplyr::filter(key, alue_code %in% key[duplicated(key$alue_code),]$alue_code)
  key <- dplyr::filter(key, !(alue_code %in% key[duplicated(key$alue_code),]$alue_code))

# Find the latest name for each duplicated code and mark it with TRUE
  temp_df<-duplicates %>%
           group_by(alue_code) %>%
           summarize(year = max(year)) %>%
           mutate(leave = TRUE)

# Add the marks of latest name to the duplicates and leave those of the latest year
  duplicates <- left_join(duplicates, temp_df, by = c("alue_code", "year")) %>%
                filter(leave == TRUE) %>%
                select(-year, -leave)

  # Combine with the other codes
  key <- dplyr::select(key, -year)
  key <- rbind(key, duplicates)

key <- rbind(key, data.frame(alue_code = "SSS", alue_name = "KOKO MAA"))
rownames(key) <- NULL

# Vuonna 2018 ja ennen Ahvenanmaa - Åland, Vuonna 2019 ja jälkeen Ahvenanmaa
#key <- dplyr::filter(key, alue_name != "Ahvenanmaa - Åland")

region_code_to_name_key <- key

usethis::use_data(region_code_to_name_key, overwrite = TRUE)


# When mapping names to codes, only with the information of the region name it
# is impossible to uniquely map. Some region names simply correspond to multiple
# codes.

key <- master_key
# Remove duplicates due to repetition of years
key <- key[!duplicated(dplyr::select(key, -year)), ]

# Find duplicated codes and remove them from the key
duplicates <- dplyr::filter(key, alue_name %in% key[duplicated(key$alue_name),]$alue_name)
key <- dplyr::filter(key, !(alue_code %in% key[duplicated(key$alue_code),]$alue_code))
key <- dplyr::select(key, -year)

# Find the latest name for each duplicated code and mark it with TRUE
temp_df<-duplicates %>%
  group_by(alue_name) %>%
  mutate(do_smth = length(unique(sapply(unique(alue_code), gsub, pattern = "[^a-zA-Z]", replacement = ""))) == 1)

to_key <- filter(temp_df, !do_smth) %>% select(-year, -do_smth)
key <- rbind(key, to_key)

temp_df <- temp_df %>%
  filter(do_smth) %>%
  group_by(alue_name) %>%
  summarize(year = max(year)) %>%
  mutate(leave = TRUE)

# Add the marks of latest name to the duplicates and leave those of the latest year
duplicates <- left_join(duplicates, temp_df, by = c("alue_name", "year")) %>%
  filter(leave == TRUE) %>%
  select(-year, -leave)

key <- rbind(key, duplicates)
key <- rbind(key, data.frame(alue_code = "SSS", alue_name = "KOKO MAA"))
key <- key[!duplicated(key),]

nonstandard_key <- c("koko maa" = "SSS",
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

nonstandard_region_names_key <- data.frame(alue_name = names(nonstandard_key), alue_code = nonstandard_key)
rownames(nonstandard_region_names_key) <- NULL

key <- rbind(key, nonstandard_key)

region_name_to_code_key <- key

usethis::use_data(region_name_to_code_key, overwrite = TRUE)

