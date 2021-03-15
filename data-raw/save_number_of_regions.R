# Data set for the number of municipalities in each year

number_of_regions <- data.frame()

sys_current_year <- as.double(substring(Sys.Date(), 1,4))

regions <- prefix_name_key$name[-1]
years <- 2008:sys_current_year

for(region in regions) {
  for(year in years) {
    regionclassification <- get_regionclassification(region, year = year, offline = FALSE, only_codes = TRUE)
    number_of_regions <- rbind(number_of_regions,
                          data.frame(year = year,
                                     number_of_regions = length(regionclassification),
                                     region_level = region))
    Sys.sleep(0.01)
    print(paste(region, year))
  }
}

usethis::use_data(number_of_regions, overwrite = TRUE)
