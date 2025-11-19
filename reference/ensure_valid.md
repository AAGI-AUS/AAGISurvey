# Validate user inputs

Validate user inputs

## Usage

``` r
ensure_valid(vals, allowed, field)
```

## Arguments

- vals:

  Character vector of user inputs.

- allowed:

  Named character vector of allowed values.

- field:

  Character string of field name for error messages.

## Value

`NULL` invisibly if all values are valid, otherwise throws an
informative error.
