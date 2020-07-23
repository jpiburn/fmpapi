
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fmp: An R package for interfacing with the Financial Modeling Prep API

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fmp)](https://CRAN.R-project.org/package=fmp)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of `fmp` is to provide a simple and consistent interface to the
**Financial Modeling Prep** [Financial Data
API](https://financialmodelingprep.com/) that can be used along side and
integrated with other common R resources for collecting and analyzing
financial data, such as `tidyquant`, `xts`, `TTR`, and `quantmod`.

## Installation

<!-- You can install the released version of fmp from [CRAN](https://CRAN.R-project.org) with: -->

<!-- --- actually not on CRAN yet -->

<!-- ``` r -->

<!-- install.packages("fmp") -->

<!-- ``` -->

You can install the latest development version of fmp from
[Github](https://github.com) with:

``` r
remotes::install_github('jpiburn/fmp')
```

## Getting Started

Before getting started with `fmp` you first must obtain an API key for
the Financial Modeling Prep Financial Data API. For details, see
[here](https://financialmodelingprep.com/developer/docs/pricing/).

Once you sign up, you can add your API key to your `.Renviron` file by
using `fmp_api_key()`. This will add a `FMP_API_KEY` entry to your
`.Renviron` file so it will automatically be available in future
sessions. When first installed however, you will need to reload your
.Renviron file by either restarting R or running
`readRenviron('~/.Renviron')`

``` r
library(fmp)

api_key <- 'my_api_key'
fmp_api_key(api_key)

# reload
readRenviron('~/.Renviron')
```

## Company Valuation

The Financial Modeling Prep Financial Data API, provides several useful
enpoints for company valutation analysis.From general company overviews,
using `fmp_profile()`

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 3.6.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(fmp)

my_stocks <- c('AAPL', 'GE')

d <- fmp_profile(my_stocks)

glimpse(d)
#> Rows: 2
#> Columns: 1
#> $ error_message <chr> "Limit Reach . Please upgrade your plan or visit our ...
```

To more detailed analysis, such as cash flow and balance sheet
statements, key metrics, and discounted cash flow value.

``` r
library(dplyr)
library(fmp)

my_stocks <- c('AAPL', 'GE')

d_cash <- fmp_cash_flow(my_stocks)
d_balance <- fmp_balance_sheet(my_stocks)
d_metrics <- fmp_key_metrics(my_stocks)

glimpse(d_metrics)
#> Rows: 2
#> Columns: 1
#> $ error_message <chr> "Limit Reach . Please upgrade your plan or visit our ...
```

## More documentation coming soon
