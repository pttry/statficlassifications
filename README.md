
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statficlassifications

<!-- badges: start -->

<!-- badges: end -->

The `statficlassifications` allows the access of Statistics Finland
classifications API at <https://data.stat.fi/api/classifications/v2>
from within R. (For information on the API, see
<https://www.stat.fi/fi/luokitukset/info/>.) Gets correspondence tables
or how the package calls them, classification keys or just keys.
Provides tools for reclassifying regions in data.

## Installation

To install the packag and start using the package:

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

Each key is uniquely identified by a local ID in the API. Having found
the suitable key, you can use `search_keys` with an argument `as_localID
= TRUE` to print the local ID of the
key:

``` r
localID <- search_keys(source = "kunta", target = "suuralue", year = 2020, as_localID = TRUE)
localID
#> [1] "kunta_1_20200101%23suuralue_1_20200101"
```

You can also create a local ID yourself:

``` r
localID <- create_localID_name("kunta", "maakunta", year = 2020)
localID
#> [1] "kunta_1_20200101%23maakunta_1_20200101"
```

Finally, you can get the key by:

``` r
key <- get_key(localID)
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
head(regionkey)
#>    kunta_name seutukunta_name maakunta_name suuralue_name              ely_name
#> 1        Akaa Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi Pirkanmaan ELY-keskus
#> 2      Urjala Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi Pirkanmaan ELY-keskus
#> 3 Valkeakoski Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi Pirkanmaan ELY-keskus
#> 4     Tammela          Forssa    Kanta-Häme   Etelä-Suomi     Hämeen ELY-keskus
#> 5    Humppila          Forssa    Kanta-Häme   Etelä-Suomi     Hämeen ELY-keskus
#> 6   Jokioinen          Forssa    Kanta-Häme   Etelä-Suomi     Hämeen ELY-keskus
regionkey <- get_regionkey(only_codes = TRUE)
head(regionkey)
#>   kunta_code seutukunta_code maakunta_code suuralue_code ely_code
#> 1      KU020           SK063          MK06           SA3    ELY05
#> 2      KU887           SK063          MK06           SA3    ELY05
#> 3      KU908           SK063          MK06           SA3    ELY05
#> 4      KU834           SK053          MK05           SA2    ELY04
#> 5      KU103           SK053          MK05           SA2    ELY04
#> 6      KU169           SK053          MK05           SA2    ELY04
```

You can also get more specialised regional classification keys by
setting arguments:

``` r
kunta_maakunta_key <- get_regionkey("kunta", "maakunta")
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
head(data)
#>    kunta_name     values
#> 1        Akaa  0.1086406
#> 2      Urjala  0.4132672
#> 3 Valkeakoski  0.7598157
#> 4     Tammela  1.6135480
#> 5    Humppila -1.9664104
#> 6   Jokioinen  1.3680512
```

You can use regional classification tables to add regions to your
data:

``` r
dplyr::left_join(data, get_regionkey(only_names = TRUE), by = "kunta_name") %>% head()
#>    kunta_name     values seutukunta_name maakunta_name suuralue_name
#> 1        Akaa  0.1086406 Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi
#> 2      Urjala  0.4132672 Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi
#> 3 Valkeakoski  0.7598157 Etelä-Pirkanmaa     Pirkanmaa   Länsi-Suomi
#> 4     Tammela  1.6135480          Forssa    Kanta-Häme   Etelä-Suomi
#> 5    Humppila -1.9664104          Forssa    Kanta-Häme   Etelä-Suomi
#> 6   Jokioinen  1.3680512          Forssa    Kanta-Häme   Etelä-Suomi
#>                ely_name
#> 1 Pirkanmaan ELY-keskus
#> 2 Pirkanmaan ELY-keskus
#> 3 Pirkanmaan ELY-keskus
#> 4     Hämeen ELY-keskus
#> 5     Hämeen ELY-keskus
#> 6     Hämeen ELY-keskus
```

For a shortcut, use `add_region`:

``` r
data %>% add_region("maakunta") %>% head()
#>    kunta_name     values maakunta_name
#> 1        Akaa  0.1086406     Pirkanmaa
#> 2      Urjala  0.4132672     Pirkanmaa
#> 3 Valkeakoski  0.7598157     Pirkanmaa
#> 4     Tammela  1.6135480    Kanta-Häme
#> 5    Humppila -1.9664104    Kanta-Häme
#> 6   Jokioinen  1.3680512    Kanta-Häme
```

It is also straightforward to compute, say, maakunta-level means give
the municipal data.

``` r
data %>% add_region("maakunta") %>% 
         dplyr::group_by(maakunta_name) %>%
         dplyr::summarize(maakunta_mean = mean(values)) %>% head()
#> `summarise()` ungrouping output (override with `.groups` argument)
#> # A tibble: 6 x 2
#>   maakunta_name   maakunta_mean
#>   <fct>                   <dbl>
#> 1 Ahvenanmaa             -0.746
#> 2 Etelä-Karjala          -0.173
#> 3 Etelä-Pohjanmaa        -0.275
#> 4 Etelä-Savo              0.127
#> 5 Kainuu                  0.413
#> 6 Kanta-Häme              0.377
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
v <- c("020", "047", "15", "193")
set_region_codes(v)
#> [1] "KU020" "KU047" "MK15"  "SK193"
```

It also easy to map region codes to corresponding names and vice versa:

``` r
v <- set_region_codes(v)
codes_to_names(v)
#> [1] "Akaa"         "Enontekiö"    "Pohjanmaa"    "Torniolaakso"
v <- codes_to_names(v)
#names_to_codes(v)
```
