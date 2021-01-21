
# Haetaan tiedosto lakkautetut kunnat aakkosjarjestyksessa
url <-  "https://www.stat.fi/static/media/uploads/meta/luokitukset/lakkautetut_kunnat_aakkosjarj20_taul6.xlsx"

tmp = tempfile(fileext = ".xlsx")
download.file(url = url, destfile = tmp, mode="wb")
df <- readxl::read_excel(tmp)

df <- df[-(1:8),]
df <- dplyr::select(df, 1,4)
names(df) <- c("old", "current")

df$old <- paste0("KU", df$old)
df$current <- paste0("KU", df$current)

df2 <- get_regionkey(source = "kunta", targets = "kunta", year = get_latest_year(), offline = FALSE)
df2 <- mutate(df2, old = kunta_code, current = kunta_code)
df2 <- dplyr::select(df2, old, current)

old_current_mun_key <- rbind(df, df2)

usethis::use_data(old_current_mun_key, overwrite = TRUE)
