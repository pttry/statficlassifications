## Aluetyppiluokitus

# Maaseutualueiden osalta luokitus täältä: https://www.tilastokeskus.fi/tup/msind/msindaluetyypit.html
# Ei enää olemassa
# Lisäksi työssäkäyntialueen keskus, yliopistokeskus, ja pääkaupunkiseutus

library(tidyverse)

aluetyyppi <- readxl::read_xls("data-raw/msindaluejaot10.xls") %>%
  mutate(kunta_code = statfitools::extract_code(Kunta, numbers_as_numeric = FALSE),
         kunta_code = as_factor(set_region_codes(kunta_code, region_level = "kunta")),
         kunta_name = codes_to_names(kunta_code),
         aluetyyppi = coalesce(luokka2, Alueluokka)) %>%
  select(kunta_code, kunta_name, aluetyyppi) %>%
  mutate(aluetyyppi = fct_relevel(aluetyyppi,
                                  rev(c("Harvaan asuttu maaseutu", "Ydinmaaseutu", "Kaupunkien läh. maaseutu",
                                        "Kaupungit", "TKA", "YO", "PK seutu")))) %>%
  mutate(aluetyyppi = fct_recode(aluetyyppi,
                                 'Muut kaupungit' = "Kaupungit",
                                 'Muut keskukset' = "TKA",
                                 'Muut yliopistokaupungit' = "YO",
                                 'PK-seutu' = "PK seutu"))


# check_region_codes(aluetyyppi$kunta_code, year = 2021)

use_data(aluetyyppi, overwrite = TRUE)


## PK-kehyskunnat

aluetyyppi2_alueet <-
  c(PK = "Pääkaupunkiseutu",
    KK = "Kehyskunnat",
    YO = "Muut yliopistokaupungit",
    TK1 = "Muut kaupunkimaiset kunnat",
    TK2 = "Muut taajaan asutut kunnat",
    TK3 = "Muut maaseutumaiset kunnat")

aluetyyppi2 <-
  get_regionkey("kunta", "seutukunta", "kuntaryhmitys", offline = FALSE) %>%
  left_join(aluetyyppi, by = c("kunta_code", "kunta_name")) %>%
  rename_with(~gsub("kuntaryhmitys", "aluetyyppi2", .x)) %>%
  mutate(aluetyyppi2_code =
           case_when(
             kunta_code %in% c("KU091", "KU235", "KU049", "KU092") ~ "PK",
             seutukunta_code == "SK011" ~ "KK",
             aluetyyppi == "Muut yliopistokaupungit" ~ "YO",
             TRUE ~ as.character(aluetyyppi2_code))) %>%
  mutate(aluetyyppi2_name = recode(aluetyyppi2_code, !!!aluetyyppi2_alueet)) %>%
  select(kunta_code, kunta_name, aluetyyppi2_code, aluetyyppi2_name) %>%
  add_row(kunta_code = "SSS", kunta_name = "KOKO MAA", aluetyyppi2_code = "SSS", aluetyyppi2_name = "KOKO MAA")

use_data(aluetyyppi2, overwrite = TRUE)
