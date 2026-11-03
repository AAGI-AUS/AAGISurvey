# Get the AAGI Survey Base URL

Returns the base URL of the AAGI Qualtrics survey. This is the single
place the URL is defined, so dependent packages and apps (e.g., Shiny)
can use it rather than hard coding their own copy.

## Usage

``` r
survey_base_url()
```

## Value

A length-one character string.

## Examples

``` r
survey_base_url()
#> [1] "https://curtin.au1.qualtrics.com/jfe/form/SV_6xUesPlghPZuKlo"
```
