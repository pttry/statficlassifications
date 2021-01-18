
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statficlassifications

<!-- badges: start -->

<!-- badges: end -->

The `statficlassifications` allows the access of Statistics Finland
classifications API from within R. Gets correspondence tables or
classification keys.

## Installation

To install the packag and start using the package:

``` r
install.packages("devtools")
devtools::install_github("pttry/statficlassifications")
library(statficlassifications)
library(dplyr)
```

## List all available classification keys

``` r
search_keys()
```

## Search for available classifications

``` r
search_keys("maakunta")
search_keys("maakunta", search_source = TRUE)
search_keys(searchword_source = "kunta", searchword_target = "maakunta", year = 2020)
```

## Print the localID of the search results and assign

``` r

localID <- search_keys(searchword_source = "kunta", searchword_target = "maakunta", year = 2020, as_localID = TRUE)
```

## Create localID yourself

``` r
localID <- create_localID_name("kunta", "maakunta", year = 2020)
```

## Get classification key

``` r
key <- get_key(localID)
```

## Get the latest region key table

Load the complete regionkey

``` r
key <- get_regionkey(only_codes = FALSE, only_names = FALSE)
```

## Manipulate region information in data

Generate random municipal data

``` r
data <- get_regionkey() %>%
        dplyr::select(kunta_name) %>%
        mutate(values = rnorm(n()))
```

Use  with this regionkey to add regions to your data

``` r
left_join(data, get_regionkey(only_names = TRUE), by = "kunta_name")
```

Compute the region (maakunta) -level
means.

``` r
data %>% left_join(get_regionkey(only_names = TRUE), by = "kunta_name") %>% 
         group_by(maakunta_name) %>%
         summarize(maakunta_mean = mean(values)) 
```

Change municipality names to municipality codes

``` r
data <- names_to_codes(data)
```

Change municpality codes to municipality names

``` r
data <- codes_to_names(data)
```