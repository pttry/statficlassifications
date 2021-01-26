
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statficlassifications

<!-- badges: start -->

<!-- badges: end -->

The `statficlassifications`-package allows the access of the
(correspondence tables of) open classifications API of Statistics
Finland at <https://data.stat.fi/api/classifications/v2>. (For
information on the API, see <https://www.stat.fi/fi/luokitukset/info/>.)
`statficlassifications` gives functions that get correspondence tables /
classification conversion keys. Also provides tools for reclassifying
regions in data.

## Installation

To install and start using the package:

``` r
install.packages("devtools")
devtools::install_github("pttry/statficlassifications")
library(statficlassifications)
```

# Searching for keys

To list all available keys, use `search_keys` without arguments:

``` r
head(search_keys())
#> [1] "mannersu_ahvena 2017 -> ely 2017"           
#> [2] "maakunta 2010 -> avi 2010"                  
#> [3] "seutukunta 2007 -> maakunta 2007"           
#> [4] "instit_sektori 1996 -> sektoriluokitus 2000"
#> [5] "seutukunta 2009 -> tyov_elink_kesk 2009"    
#> [6] "seutukunta 2009 -> maakunta 2009"
```

Plug in search terms to search for available keys or use more specific
options:

``` r
search_keys("maakunta")
#>  [1] "maakunta 2010 -> avi 2010"        "maakunta 2012 -> seutukunta 2012"
#>  [3] "maakunta 2007 -> suuralue 2003"   "maakunta 2012 -> suuralue 2012"  
#>  [5] "maakunta 2011 -> suuralue 2011"   "maakunta 2016 -> suuralue 2016"  
#>  [7] "seutukunta 2007 -> maakunta 2007" "seutukunta 2009 -> maakunta 2009"
#>  [9] "seutukunta 2010 -> maakunta 2010" "seutukunta 2011 -> maakunta 2011"
#> [11] "kunta 2007 -> maakunta 2007"      "kunta 2008 -> maakunta 2008"     
#> [13] "kunta 2009 -> maakunta 2009"      "kunta 2010 -> maakunta 2010"     
#> [15] "kunta 2011 -> maakunta 2011"      "kunta 2012 -> maakunta 2012"     
#> [17] "kunta 2013 -> maakunta 2013"      "kunta 2014 -> maakunta 2014"     
#> [19] "kunta 2015 -> maakunta 2015"      "kunta 2016 -> maakunta 2016"     
#> [21] "kunta 2017 -> maakunta 2017"      "kunta 2019 -> maakunta 2019"     
#> [23] "kunta 2019 -> maakunta 2019"      "kunta 2020 -> maakunta 2020"     
#> [25] "kunta 2018 -> maakunta 2018"      "kunta 2021 -> maakunta 2021"
search_keys("maakunta", search_source = TRUE)
#> [1] "maakunta 2010 -> avi 2010"        "maakunta 2012 -> seutukunta 2012"
#> [3] "maakunta 2007 -> suuralue 2003"   "maakunta 2012 -> suuralue 2012"  
#> [5] "maakunta 2011 -> suuralue 2011"   "maakunta 2016 -> suuralue 2016"
search_keys(source = "kunta", target = "suuralue", year = 2020)
#> [1] "kunta 2020 -> suuralue 2020"
```

# Getting keys

The API uniquely identifies each classification key by a local ID.
Having found the suitable key, you can use `search_keys` with an
argument `as_localId = TRUE` to print the localId of the
key:

``` r
localId <- search_keys(source = "kunta", target = "suuralue", year = 2020, as_localId = TRUE)
localId
#> [1] "kunta_1_20200101%23suuralue_1_20200101"
```

You can also create a local Id yourself:

``` r
localId <- create_localId_name("kunta", "maakunta", year = 2020)
localId
#> [1] "kunta_1_20200101%23maakunta_1_20200101"
```

Finally, you can get the key by:

``` r
key <- get_key(localId)
#> Vuoden 2020 kuntien ja maakuntien välinen luokitusavain
head(key)
#>   source_code source_name target_code target_name
#> 1         257 Kirkkonummi          01     Uusimaa
#> 2         245      Kerava          01     Uusimaa
#> 3         543  Nurmijärvi          01     Uusimaa
#> 4         186   Järvenpää          01     Uusimaa
#> 5         434     Loviisa          01     Uusimaa
#> 6         638      Porvoo          01     Uusimaa
```

``` r
library(statficlassifications)
```

The package provides more more specialised ways for dealing with
regional classifications.

### Getting regional classification keys

To load regional classification keys, use `get_regionkey`:

``` r
regionkey <- get_regionkey(only_names = TRUE)
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
head(regionkey)
#>    kunta_name seutukunta_name maakunta_name              ely_name suuralue_name
#> 1        Akaa Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus   Länsi-Suomi
#> 2      Urjala Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus   Länsi-Suomi
#> 3 Valkeakoski Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus   Länsi-Suomi
#> 4     Tammela          Forssa    Kanta-Häme     Hämeen ELY-keskus   Etelä-Suomi
#> 5    Humppila          Forssa    Kanta-Häme     Hämeen ELY-keskus   Etelä-Suomi
#> 6   Jokioinen          Forssa    Kanta-Häme     Hämeen ELY-keskus   Etelä-Suomi
regionkey <- get_regionkey(only_codes = TRUE)
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
head(regionkey)
#>   kunta_code seutukunta_code maakunta_code ely_code suuralue_code
#> 1      KU020           SK063          MK06    ELY05           SA3
#> 2      KU887           SK063          MK06    ELY05           SA3
#> 3      KU908           SK063          MK06    ELY05           SA3
#> 4      KU834           SK053          MK05    ELY04           SA2
#> 5      KU103           SK053          MK05    ELY04           SA2
#> 6      KU169           SK053          MK05    ELY04           SA2
```

You can also get more specialised regional classification keys by
setting arguments:

``` r
kunta_maakunta_key <- get_regionkey("kunta", "maakunta")
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
head(kunta_maakunta_key)
#>    kunta_name maakunta_name kunta_code maakunta_code
#> 1        Akaa     Pirkanmaa      KU020          MK06
#> 2      Urjala     Pirkanmaa      KU887          MK06
#> 3 Valkeakoski     Pirkanmaa      KU908          MK06
#> 4     Tammela    Kanta-Häme      KU834          MK05
#> 5    Humppila    Kanta-Häme      KU103          MK05
#> 6   Jokioinen    Kanta-Häme      KU169          MK05
```

### Manipulating regional variables

´statficlassifications´ also gives you a selection of ways and functions
that help you to standardize and manipulate the regional information in
your data. For examples, generate random municipal
data:

``` r
data <- get_regionkey() %>% dplyr::select(kunta_name) %>% dplyr::mutate(values = rnorm(dplyr::n()))
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
head(data)
#>    kunta_name      values
#> 1        Akaa  2.25379837
#> 2      Urjala  1.32789006
#> 3 Valkeakoski  0.09849152
#> 4     Tammela  0.27872330
#> 5    Humppila -1.70114496
#> 6   Jokioinen  1.03625297
```

You can use regional classification tables to add regions to your
data:

``` r
dplyr::left_join(data, get_regionkey(only_names = TRUE), by = "kunta_name") %>% head()
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
#>    kunta_name      values seutukunta_name maakunta_name              ely_name
#> 1        Akaa  2.25379837 Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus
#> 2      Urjala  1.32789006 Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus
#> 3 Valkeakoski  0.09849152 Etelä-Pirkanmaa     Pirkanmaa Pirkanmaan ELY-keskus
#> 4     Tammela  0.27872330          Forssa    Kanta-Häme     Hämeen ELY-keskus
#> 5    Humppila -1.70114496          Forssa    Kanta-Häme     Hämeen ELY-keskus
#> 6   Jokioinen  1.03625297          Forssa    Kanta-Häme     Hämeen ELY-keskus
#>   suuralue_name
#> 1   Länsi-Suomi
#> 2   Länsi-Suomi
#> 3   Länsi-Suomi
#> 4   Etelä-Suomi
#> 5   Etelä-Suomi
#> 6   Etelä-Suomi
```

For a shortcut, use `add_region`:

``` r
data %>% add_region("maakunta") %>% head()
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
#>    kunta_name      values maakunta_name
#> 1        Akaa  2.25379837     Pirkanmaa
#> 2      Urjala  1.32789006     Pirkanmaa
#> 3 Valkeakoski  0.09849152     Pirkanmaa
#> 4     Tammela  0.27872330    Kanta-Häme
#> 5    Humppila -1.70114496    Kanta-Häme
#> 6   Jokioinen  1.03625297    Kanta-Häme
```

It is also straightforward to compute, say, maakunta-level means give
the municipal data.

``` r
data %>% add_region("maakunta") %>% 
         dplyr::group_by(maakunta_name) %>%
         dplyr::summarize(maakunta_mean = mean(values)) %>% head()
#> Warning in get_latest_year(offline = offline): In January, the offline latest
#> year is the last year in case no fresh keys updated.
#> `summarise()` ungrouping output (override with `.groups` argument)
#> # A tibble: 6 x 2
#>   maakunta_name   maakunta_mean
#>   <fct>                   <dbl>
#> 1 Ahvenanmaa            -0.119 
#> 2 Etelä-Karjala         -0.0552
#> 3 Etelä-Pohjanmaa        0.372 
#> 4 Etelä-Savo            -0.453 
#> 5 Kainuu                 0.409 
#> 6 Kanta-Häme             0.508
```

`statficlassifications` aims to impose the use of prefixed region codes.
Prefixes are useful in making sure codes do not map to multiple names.
There are, for instance, kuntia and seutukuntia with identical numbers.
Prefixes help distinguish between these. The correpondence of region
names and prefixes is

    #>   prefix       name
    #> 1    SSS   KOKO MAA
    #> 2     KU      kunta
    #> 3     SK seutukunta
    #> 4     MK   maakunta
    #> 5    ELY        ely
    #> 6     SA   suuralue

Function ´set\_region\_codes´helps you set your region codes in a
standard format:

``` r
v <- c("020", "047", "15", "063")
set_region_codes(v)
#> [1] "KU020" "KU047" "MK15"  "SK063"
```

It also easy to map region codes to corresponding names and vice versa:

``` r
v <- set_region_codes(v)
codes_to_names(v)
#> [1] "Akaa"            "Enontekiö"       "Pohjanmaa"       "Etelä-Pirkanmaa"
v <- codes_to_names(v)
#names_to_codes(v)
```
