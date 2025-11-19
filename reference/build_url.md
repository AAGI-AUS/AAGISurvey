# Create the URL string from user provided values

Create the URL string from user provided values for returning to the
user.

## Usage

``` r
build_url(
  base,
  support_type,
  design_type,
  analysis_type,
  aagi_node,
  organisation_type
)
```

## Arguments

- base:

  Character string of base URL.

- support_type:

  Character vector of support types.

- design_type:

  Character string of design type.

- analysis_type:

  Character string of analysis type.

- aagi_node:

  Character string of AAGI node.

- organisation_type:

  Character string of organisation type.

## Value

The full URL string.

## Examples

``` r
build_url(
  "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",
  "S_D",
  "D_SP",
  "CU",
  "O_ACA"
)
#> Error in build_url("https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",     "S_D", "D_SP", "CU", "O_ACA"): could not find function "build_url"
```
