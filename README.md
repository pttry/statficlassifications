
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statficlassifications

<!-- badges: start -->
<!-- badges: end -->

`statficlassifications` accesses the open classifications API of
Statistics Finland at <https://data.stat.fi/api/classifications/v2>.
`statficlassifications` contains tools for searching and importing
classifications and classification conversion keys in an R-friendly
format. The package also provides special tools for (re)classifying
regions.

For further information about open classifications API of Statistics
Finland, see <https://www.stat.fi/en/luokitukset/info/>.

## Installation

To install and start using the package:

``` r
install.packages("devtools")
devtools::install_github("pttry/statficlassifications")
library(statficlassifications)
```

## Accessing API

There are two types of classification objects of interest: keys and
classifications. Keys, or conversion keys or correspondence tables, are
used to convert between different classifications and classifications
can be used to convert names to codes and vice versa.

### Searching for Keys

To list all available classification keys (correspondence tables), use
`search_keys()` without arguments:

``` r
head(search_keys())
#> [1] "kunta 2019 -> ely 2019"             "kunta 2022 -> hyvinvointialue 2022"
#> [3] "kunta 2019 -> kuntaryhmitys 2019"   "kunta 2022 -> ely 2022"            
#> [5] "ammatti 1987 -> ammatti 2001"       "kunta 2019 -> seutukunta 2019"
```

Plug in search terms to search for available keys:

``` r
search_keys("maakunta")
#>  [1] "kunta 2007 -> maakunta 2007"      "kunta 2008 -> maakunta 2008"     
#>  [3] "kunta 2009 -> maakunta 2009"      "kunta 2010 -> maakunta 2010"     
#>  [5] "kunta 2011 -> maakunta 2011"      "kunta 2012 -> maakunta 2012"     
#>  [7] "kunta 2013 -> maakunta 2013"      "kunta 2014 -> maakunta 2014"     
#>  [9] "kunta 2015 -> maakunta 2015"      "maakunta 2007 -> suuralue 2003"  
#> [11] "kunta 2016 -> maakunta 2016"      "maakunta 2010 -> avi 2010"       
#> [13] "seutukunta 2009 -> maakunta 2009" "seutukunta 2007 -> maakunta 2007"
#> [15] "maakunta 2012 -> seutukunta 2012" "maakunta 2012 -> suuralue 2012"  
#> [17] "maakunta 2011 -> suuralue 2011"   "seutukunta 2010 -> maakunta 2010"
#> [19] "seutukunta 2011 -> maakunta 2011" "maakunta 2016 -> suuralue 2016"  
#> [21] "kunta 2017 -> maakunta 2017"      "kunta 2018 -> maakunta 2018"     
#> [23] "kunta 2019 -> maakunta 2019"      "kunta 2020 -> maakunta 2020"     
#> [25] "kunta 2021 -> maakunta 2021"      "maakunta 2021 -> suuralue 2021"  
#> [27] "kunta 2022 -> maakunta 2022"      "maakunta 2022 -> suuralue 2022"  
#> [29] "kunta 2023 -> maakunta 2023"
search_keys("kunta", "suuralue", 2020)
#> [1] "kunta 2020 -> suuralue 2020"
```

### Getting Keys

The open classifications API uniquely identifies each classification key
by a local ID. Having found the suitable key, you can use
`search_keys()` with an argument `as_localId = TRUE` to print the
localId of the key:

``` r
localId <- search_keys(source = "kunta", target = "suuralue", year = 2020, as_localId = TRUE)
localId
#> [1] "kunta_1_20200101%23suuralue_1_20200101"
```

Then you can get the key by:

``` r
key <- get_key(localId)
#> Vuoden 2020 kuntien ja suuralueiden välinen luokitusavain
head(key)
#>   source_code source_name target_code      target_name
#> 1         710   Raasepori           1 Helsinki-Uusimaa
#> 2         755     Siuntio           1 Helsinki-Uusimaa
#> 3         753       Sipoo           1 Helsinki-Uusimaa
#> 4         858     Tuusula           1 Helsinki-Uusimaa
#> 5         927       Vihti           1 Helsinki-Uusimaa
#> 6         018      Askola           1 Helsinki-Uusimaa
```

### Searching for Classifications

To list all available classifications, use `search_classifications()`
without arguments:

``` r
head(search_classifications())
#> [1] "verolaji 2019"        "coicop 2018"          "nettovaral_luok 2023"
#> [4] "koulutusaste 2018"    "suuruusluokka 2018"   "erva 2020"
```

Plug in search terms to search for available classifications:

``` r
search_classifications("ammatti")
#> [1] "ammatti 1987" "ammatti 1980" "ammatti 2001" "ammatti 1997" "ammatti 2010"
#> [6] "ammatti 2018" "ammatti 2021"
search_classifications("ammatti", 2021)
#> [1] "ammatti 2021"
```

### Getting Classifications

To print the localId of the desired classification, use
`as_localId = TRUE`.

``` r
localId <- search_classifications("ammatti", year = 2021, as_localId = TRUE)
```

Use localId to get the classification

``` r
head(get_classification(localId))
#> Tulorekisterin TK10-ammattiluokitus
#>    code                                                        name
#> 1 01100                                                    Upseerit
#> 2 02100                                                 Aliupseerit
#> 3 03100                                    Sotilasammattihenkilöstö
#> 4 11110                                                Lainsäätäjät
#> 5 11121                            Valtion keskushallinnon johtajat
#> 6 11122 Alue- ja paikallishallinnon johtajat ja ylimmät virkamiehet
```

## Tools for Regional Classifications

For regional classifications, `statficlassifications` has tailored
tools.

### Getting regional classifications

To load regional classifications, use `get_regionclassification`:

``` r
region_classification <- get_regionclassification()
head(region_classification)
#>   alue_code alue_name
#> 1     KU020      Akaa
#> 2     KU004  Alahärmä
#> 3     KU005  Alajärvi
#> 4     KU006  Alastaro
#> 5     KU009 Alavieska
#> 6     KU010    Alavus
seutukunta_classification <- get_regionclassification("seutukunta")
head(seutukunta_classification)
#>   seutukunta_code         seutukunta_name
#> 1           SK063         Etelä-Pirkanmaa
#> 2           SK143 Eteläiset seinänaapurit
#> 3           SK053                  Forssa
#> 4           SK072                 Heinola
#> 5           SK011                Helsinki
#> 6           SK051             Hämeenlinna
kunta_2010_classification <- get_regionclassification("kunta", year = 2010)
#> Overriding default option for offline when specific year is required.
head(kunta_2010_classification)
#>   kunta_code kunta_name
#> 1      KU020       Akaa
#> 2      KU005   Alajärvi
#> 3      KU009  Alavieska
#> 4      KU010     Alavus
#> 5      KU015   Artjärvi
#> 6      KU016   Asikkala
```

### Getting regional classification keys

To load regional correspondence tables / classification keys, use
`get_regionkey`:

``` r
regionkey <- get_regionkey(only_names = TRUE)
head(regionkey)
#>    kunta_name seutukunta_name maakunta_name
#> 1        Akaa Etelä-Pirkanmaa     Pirkanmaa
#> 2      Urjala Etelä-Pirkanmaa     Pirkanmaa
#> 3 Valkeakoski Etelä-Pirkanmaa     Pirkanmaa
#> 4      Forssa          Forssa    Kanta-Häme
#> 5     Tammela          Forssa    Kanta-Häme
#> 6    Humppila          Forssa    Kanta-Häme
regionkey <- get_regionkey(only_codes = TRUE)
head(regionkey)
#>   kunta_code seutukunta_code maakunta_code
#> 1      KU020           SK063          MK06
#> 2      KU887           SK063          MK06
#> 3      KU908           SK063          MK06
#> 4      KU061           SK053          MK05
#> 5      KU834           SK053          MK05
#> 6      KU103           SK053          MK05
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
#> 4      Forssa    Kanta-Häme      KU061          MK05
#> 5     Tammela    Kanta-Häme      KU834          MK05
#> 6    Humppila    Kanta-Häme      KU103          MK05
```

### Manipulate regional variables

`statficlassifications` also gives you a selection of ways and functions
that help you to standardize and manipulate the regional information in
your data. For examples, generate random municipal data:

``` r
data <- get_regionkey() |> dplyr::select(kunta_name) |> dplyr::mutate(values = rnorm(dplyr::n()))
head(data)
#>    kunta_name      values
#> 1        Akaa  1.60532403
#> 2      Urjala -0.59541349
#> 3 Valkeakoski  0.86430228
#> 4      Forssa  0.26600502
#> 5     Tammela -0.09058066
#> 6    Humppila -0.24403987
```

You can use regional classification tables to add regions to your data:

``` r
dplyr::left_join(data, get_regionkey(only_names = TRUE), by = "kunta_name") |> head()
#>    kunta_name      values seutukunta_name maakunta_name
#> 1        Akaa  1.60532403 Etelä-Pirkanmaa     Pirkanmaa
#> 2      Urjala -0.59541349 Etelä-Pirkanmaa     Pirkanmaa
#> 3 Valkeakoski  0.86430228 Etelä-Pirkanmaa     Pirkanmaa
#> 4      Forssa  0.26600502          Forssa    Kanta-Häme
#> 5     Tammela -0.09058066          Forssa    Kanta-Häme
#> 6    Humppila -0.24403987          Forssa    Kanta-Häme
```

For a shortcut, use `add_region`:

``` r
 data |> add_region("maakunta") |> head()
#> The region classification in data seems to fit year(s) 2021. A key corresponding to this year is used.
#>    kunta_name      values   maakunta
#> 1        Akaa  1.60532403  Pirkanmaa
#> 2      Urjala -0.59541349  Pirkanmaa
#> 3 Valkeakoski  0.86430228  Pirkanmaa
#> 4      Forssa  0.26600502 Kanta-Häme
#> 5     Tammela -0.09058066 Kanta-Häme
#> 6    Humppila -0.24403987 Kanta-Häme
```

It is also straightforward to compute, say, maakunta-level means give
the municipal data.

``` r
data |> add_region("maakunta") |> 
        dplyr::group_by(maakunta) |>
       dplyr::summarize(maakunta_mean = mean(values)) |> head()
#> The region classification in data seems to fit year(s) 2021. A key corresponding to this year is used.
#> # A tibble: 6 × 2
#>   maakunta        maakunta_mean
#>   <fct>                   <dbl>
#> 1 Ahvenanmaa            -0.363 
#> 2 Etelä-Karjala         -0.201 
#> 3 Etelä-Pohjanmaa        0.227 
#> 4 Etelä-Savo             0.0455
#> 5 Kainuu                 0.0385
#> 6 Kanta-Häme            -0.0827
```

### Region code prefixes

`statficlassifications` aims to impose the use of prefixed region codes.
Prefixes are useful in making sure codes do not map to multiple names.
There are, for instance, kuntia and seutukuntia with identical numbers.
Prefixes help distinguish between these. The correpondence of region
names and prefixes is

    #>   prefix                    name
    #> 1    SSS                KOKO MAA
    #> 2     KU                   kunta
    #> 3     SK              seutukunta
    #> 4     MK                maakunta
    #> 5     SA                suuralue
    #> 6    ELY                     ely
    #> 7     MA manner_suomi_ahvenanmaa
    #> 8     TK           kuntaryhmitys

Function `set_region_codes` helps you set the prefixes of you region
codes:

``` r
v <- c("191", "047", "063")
set_region_codes(v)
#> [1] "SK191" "KU047" "SK063"
```

If your input vector has codes that may match to multiple region codes,
you get an error without giving more information. If you know the region
level to which your codes correspond to you can submit the region level
to `region_level`-argument.

``` r
v <- c("5", "47", "20")
set_region_codes(v, region_level = "kunta")
#> [1] "KU005" "KU047" "KU020"
```

Often you find data where kunta codes are not prefixed but the codes of
other regions are. With your information on to which region level codes
the codes without prefixes should be mapped, you can restrict the domain
of this mapping by providing the `region_level`-argument:

``` r
v <- c("020", "047", "005", "MK01", "MK02")
set_region_codes(v, region_level = "kunta")
#> [1] "KU020" "KU047" "KU005" "MK01"  "MK02"
```

Again sometimes you have codes of multiple region levels mixed in the
data without prefixes but the charachter length of the codes might make
a difference. In these cases you can set `use_char_length_info = TRUE`.
The default is way to use character length information is that three
characters incidate kuntia and two characters indicate maakuntia.

``` r
v <- c("020", "047", "005", "01", "02")
set_region_codes(v, use_char_length_info = TRUE)
#> [1] "KU020" "KU047" "KU005" "MK01"  "MK02"
```

Prefixed region codes map unambiguosly to region names so it is then
easy to map the codes to names. Note, however, that some region names do
not map uniquely to region codes. In these cases you have to supply the
`region_level`-argument again.

``` r

v <- c("020", "047", "005", "MK01", "MK02")
v <- set_region_codes(v, region_level = "kunta")
codes_to_names(v)
#> [1] "Akaa"            "Enontekiö"       "Alajärvi"        "Uusimaa"        
#> [5] "Varsinais-Suomi"
v <- codes_to_names(v)
names_to_codes(v)
#> [1] "KU020" "KU047" "KU005" "MK01"  "MK02"
```
