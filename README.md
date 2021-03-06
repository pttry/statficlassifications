
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statficlassifications

<!-- badges: start -->

<!-- badges: end -->

The `statficlassifications`-package accesses the open classifications
API of Statistics Finland at
<https://data.stat.fi/api/classifications/v2>. `statficlassifications`
contains tools to search and import classifications and classification
conversion keys in a format readily to be used by R. The package also
provides special tools for (re)classifying regions in data.

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
classifications. Keys are used to convert between different
classifications and classifications are can be used to convert names to
codes and vice versa.

### Searching for keys

To list all available classification keys (correspondence tables), use
`search_keys` without arguments:

``` r
head(search_keys())
#> [1] "instit_sektori 1996 -> sektoriluokitus 2000"
#> [2] "kunta 2007 -> maakunta 2007"                
#> [3] "kunta 1997 -> kielisuhde 1993"              
#> [4] "kunta 2004 -> kielisuhde 2003"              
#> [5] "kunta 2003 -> kielisuhde 2003"              
#> [6] "kunta 2001 -> kielisuhde 1993"
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
#> [23] "kunta 2019 -> maakunta 2019"      "kunta 2021 -> maakunta 2021"     
#> [25] "kunta 2020 -> maakunta 2020"
search_keys("kunta", "suuralue", 2020)
#> [1] "kunta 2020 -> suuralue 2020"
```

### Getting keys

The open classifications API uniquely identifies each classification key
by a local ID. Having found the suitable key, you can use `search_keys`
with an argument `as_localId = TRUE` to print the localId of the
key:

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
#> 1         018      Askola           1 Helsinki-Uusimaa
#> 2         235  Kauniainen           1 Helsinki-Uusimaa
#> 3         245      Kerava           1 Helsinki-Uusimaa
#> 4         186   Järvenpää           1 Helsinki-Uusimaa
#> 5         434     Loviisa           1 Helsinki-Uusimaa
#> 6         638      Porvoo           1 Helsinki-Uusimaa
```

### Searching for classifications

To list all available classifications, use `search_classifications`
without arguments:

``` r
head(search_classifications())
#> [1] "siviiliasiat 2014"    "verolaji 2019"        "sosioekon_asema 2011"
#> [4] "ikakausi 1979"        "kuolinsyyt 1996"      "ammatti 2018"
```

Plug in search terms to search for available classifications:

``` r
search_classifications("ammatti")
#> [1] "ammatti 2018" "ammatti 2021" "ammatti 2010" "ammatti 2001" "ammatti 1987"
#> [6] "ammatti 1980" "ammatti 1997"
search_classifications("ammatti", 2021)
#> [1] "ammatti 2021"
```

### Getting classifications

To print the localId of the desired classification, use `as_localId =
TRUE`.

``` r
localId <- search_classifications("ammatti", year = 2021, as_localId = TRUE)
```

Use localId to get the classification

``` r
head(get_classification(localId))
#> Tilastokeskuksen ammattiluokitus (TK10)
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
#> Without year-argument a general region classification including abolished municipalities got.
head(region_classification)
#>   alue_code alue_name
#> 1     KU020      Akaa
#> 2     KU004  Alahärmä
#> 3     KU005  Alajärvi
#> 4     KU006  Alastaro
#> 5     KU009 Alavieska
#> 6     KU010    Alavus
seutukunta_classification <- get_regionclassification("seutukunta")
#> Without year-argument a general region classification including abolished municipalities got.
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
#> 2 Valkeakoski Etelä-Pirkanmaa     Pirkanmaa
#> 3      Urjala Etelä-Pirkanmaa     Pirkanmaa
#> 4      Forssa          Forssa    Kanta-Häme
#> 5   Jokioinen          Forssa    Kanta-Häme
#> 6     Tammela          Forssa    Kanta-Häme
regionkey <- get_regionkey(only_codes = TRUE)
head(regionkey)
#>   kunta_code seutukunta_code maakunta_code
#> 1      KU020           SK063          MK06
#> 2      KU908           SK063          MK06
#> 3      KU887           SK063          MK06
#> 4      KU061           SK053          MK05
#> 5      KU169           SK053          MK05
#> 6      KU834           SK053          MK05
```

You can also get more specialised regional classification keys by
setting arguments:

``` r
kunta_maakunta_key <- get_regionkey("kunta", "maakunta")
head(kunta_maakunta_key)
#>    kunta_name maakunta_name kunta_code maakunta_code
#> 1        Akaa     Pirkanmaa      KU020          MK06
#> 2 Valkeakoski     Pirkanmaa      KU908          MK06
#> 3      Urjala     Pirkanmaa      KU887          MK06
#> 4      Forssa    Kanta-Häme      KU061          MK05
#> 5   Jokioinen    Kanta-Häme      KU169          MK05
#> 6     Tammela    Kanta-Häme      KU834          MK05
```

### Manipulate regional variables

`statficlassifications` also gives you a selection of ways and functions
that help you to standardize and manipulate the regional information in
your data. For examples, generate random municipal
data:

``` r
data <- get_regionkey() %>% dplyr::select(kunta_name) %>% dplyr::mutate(values = rnorm(dplyr::n()))
head(data)
#>    kunta_name     values
#> 1        Akaa -1.3019578
#> 2 Valkeakoski -1.4338528
#> 3      Urjala -0.2843392
#> 4      Forssa -0.3854009
#> 5   Jokioinen  0.8419691
#> 6     Tammela -0.7676178
```

You can use regional classification tables to add regions to your
data:

``` r
dplyr::left_join(data, get_regionkey(only_names = TRUE), by = "kunta_name") %>% head()
#>    kunta_name     values seutukunta_name maakunta_name
#> 1        Akaa -1.3019578 Etelä-Pirkanmaa     Pirkanmaa
#> 2 Valkeakoski -1.4338528 Etelä-Pirkanmaa     Pirkanmaa
#> 3      Urjala -0.2843392 Etelä-Pirkanmaa     Pirkanmaa
#> 4      Forssa -0.3854009          Forssa    Kanta-Häme
#> 5   Jokioinen  0.8419691          Forssa    Kanta-Häme
#> 6     Tammela -0.7676178          Forssa    Kanta-Häme
```

For a shortcut, use `add_region`:

``` r
data %>% add_region("maakunta") %>% head()
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#>    kunta_name     values maakunta_name
#> 1        Akaa -1.3019578     Pirkanmaa
#> 2 Valkeakoski -1.4338528     Pirkanmaa
#> 3      Urjala -0.2843392     Pirkanmaa
#> 4      Forssa -0.3854009    Kanta-Häme
#> 5   Jokioinen  0.8419691    Kanta-Häme
#> 6     Tammela -0.7676178    Kanta-Häme
```

It is also straightforward to compute, say, maakunta-level means give
the municipal data.

``` r
data %>% add_region("maakunta") %>% 
         dplyr::group_by(maakunta_name) %>%
         dplyr::summarize(maakunta_mean = mean(values)) %>% head()
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> # A tibble: 6 x 2
#>   maakunta_name   maakunta_mean
#>   <fct>                   <dbl>
#> 1 Ahvenanmaa              0.196
#> 2 Etelä-Karjala           0.191
#> 3 Etelä-Pohjanmaa         0.194
#> 4 Etelä-Savo              0.174
#> 5 Kainuu                  0.174
#> 6 Kanta-Häme             -0.204
```

### Region code prefixes

`statficlassifications` aims to impose the use of prefixed region codes.
Prefixes are useful in making sure codes do not map to multiple names.
There are, for instance, kuntia and seutukuntia with identical numbers.
Prefixes help distinguish between these. The correpondence of region
names and prefixes is

    #>   prefix          name
    #> 1    SSS      KOKO MAA
    #> 2     KU         kunta
    #> 3     SK    seutukunta
    #> 4     MK      maakunta
    #> 5     SA      suuralue
    #> 6    ELY           ely
    #> 7     TK kuntaryhmitys

Function `set_region_codes` helps you set the prefixes of you region
codes:

``` r
v <- c("191", "047", "063")
set_region_codes(v)
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> [1] "SK191" "KU047" "SK063"
```

If your input vector has codes that may match to multiple region codes,
you get an error without giving more information. If you know the region
level to which your codes correspond to you can submit the region level
to `region_level`-argument.

``` r
v <- c("5", "47", "20")
set_region_codes(v, region_level = "kunta")
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> [1] "KU005" "KU047" "KU020"
```

Often you find data where kunta codes are not prefixed but the codes of
other regions are. With your information on to which region level codes
the codes without prefixes should be mapped, you can restrict the domain
of this mapping by providing the `region_level`-argument:

``` r
v <- c("020", "047", "005", "MK01", "MK02")
set_region_codes(v, region_level = "kunta")
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
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
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> [1] "KU020" "KU047" "KU005" "MK01"  "MK02"
```

Prefixed region codes map unambiguosly to region names so it is then
easy to map the codes to names. Note, however, that some region names do
not map uniquely to region codes. In these cases you have to supply the
`region_level`-argument again.

``` r

v <- c("020", "047", "005", "MK01", "MK02")
v <- set_region_codes(v, region_level = "kunta")
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
#> Without year-argument a general region classification including abolished municipalities got.
codes_to_names(v)
#> Without year-argument a general region classification including abolished municipalities got.
#> [1] "Akaa"            "Enontekiö"       "Alajärvi"        "Uusimaa"        
#> [5] "Varsinais-Suomi"
v <- codes_to_names(v)
#> Without year-argument a general region classification including abolished municipalities got.
names_to_codes(v)
#> Without year-argument a general region classification including abolished municipalities got.
#> [1] "KU020" "KU047" "KU005" "MK01"  "MK02"
```
