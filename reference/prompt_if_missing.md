# Prompt for a code only if it is needed and missing

Prompt for a code only if it is needed and missing

## Usage

``` r
prompt_if_missing(value, dict, title, needed = TRUE)
```

## Arguments

- value:

  The user supplied value, possibly `NULL`.

- dict:

  Named character vector of allowed values.

- title:

  Character string used as the menu title.

- needed:

  Logical; if `FALSE`, `value` is returned untouched.

## Value

`value`, or the code the user selected.
