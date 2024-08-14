# This file creates a data with all past and current municipality codes and their correspondence
# to current municipality codes.

# Note that the key is approximate. Some municipalities might have been separated and joined into
# multiple municipalities. Use of the key, however, requires a unique joinee. In cases of multiple
# joinees, a single joinee is arbitrarily selected. Different arbitrary selections may lead to
# different final joinees.

# For the ease of use, (so that a whole vector of municipalities can be matches to their current
# municipalities) also add (the trivial correspondeces of) the municipalities that were
# never abolished.

   df_key <- get_regionkey(from = "kunta", "seutukunta", offline = FALSE) |>
       dplyr::mutate(joiner = kunta_code, joinee = kunta_code) |>
       dplyr::select(joiner, joinee)


  # Haetaan tiedosto lakkautetut kunnat aakkosjarjestyksessa osoitteesta https://www.stat.fi/fi/luokitukset/tupa/
      # url <-  "https://www.stat.fi/static/media/uploads/meta/luokitukset/lakkautetut_kunnat_aakkosjarj20_taul6.xlsx"
      # tmp = tempfile(fileext = ".xlsx")
      # download.file(url = url, destfile = tmp, mode="wb")
      # df <- readxl::read_excel(tmp)


      url2 <-  "https://www.stat.fi/static/media/uploads/meta/luokitukset/lakkautetut_kunnat_vuoteen_2024_asti.xlsx"
      tmp2 = tempfile(fileext = ".xlsx")
      download.file(url = url2, destfile = tmp2, mode="wb")
      df2 <- readxl::read_excel(tmp2, skip = 2)

  # Poimitaan aineistosta lakkautettujen kuntien koodit ja niiden kuntien koodit, johon lakkautetut
  # kunnat ovat liittyneet
      # df <- df[-(1:8),]
      # df <- dplyr::select(df, 1,4)
      # names(df) <- c("joiner", "joinee")

      df2 <- df2[c(3,5)]
      names(df2) <- c("joiner", "joinee")


  # Remove municipalities in luovutetulla alueella
      # df <- filter(df, joinee != "-")

      df2 <- filter(df2, joinee != "-")

      # otetaan käyttöön uusi (vanhan df koodin voisi poistaa, jos toimii)
      df <- df2

  # Set prefixes
      df$joiner <- paste0("KU", df$joiner)
      df$joinee <- paste0("KU", df$joinee)

      current_muns <- df2$joinee

  key <- data.frame()

  for(i in seq_along(df$joiner)) {

    mun <- as.character(df$joiner[i])
    duplicated <- sum(as.double(mun == df$joiner)) > 1

    # If i is separated and joined into multiple municipalities, choose
    # the last one of these on the table.
    if(duplicated) {
     key_row <- tail(df[df$joiner == mun,], n = 1)
    } else {
      key_row <- df[i,]
    }
    j <- 1

    # repeat until the joinee of the row is in the set of current municipalities
    while(!(key_row$joinee %in% current_muns)) {
      # If the joinee is not in the set of current municipalities and not in the set
      # of joiner municipalities, the joinee cannot be determined.
      if(!(key_row$joinee %in% df$joiner)) {
        key_row$joinee <- NA
        break
      }

      # Find the joinee on the list of joiners
      temp_df <- df[df$joiner == key_row$joinee,]
      # Set new joinee to be the joinee of the joiner, arbitrarily choose the last
      # element if duplicates.
      key_row$joinee <- tail(temp_df$joinee, n = 1)
      j <- j + 1
      if(j > 100) {
        key_row$joinee <- tail(temp_df$joinee, n = 2)[1]
      }
    }

    key <- rbind(key, key_row)
    print(c(i,j))

  }

  # One joinee is luovutetulla alueella and joiner gets value NA
  key <- filter(key, !is.na(joiner) & !is.na(joinee))
  key <- distinct(key)

  # Combine the components to create the final correspondence table.

    abolished_mun_key <- rbind(df, df_key)

  # Save
    usethis::use_data(abolished_mun_key, overwrite = TRUE)
