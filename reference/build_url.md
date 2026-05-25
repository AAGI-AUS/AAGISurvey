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

  Character string of support type.

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
  base = "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",
  support_type = "S_D",
  design_type = "D_SP",
  analysis_type = "A_BIO",
  aagi_node = "CU",
  organisation_type = "O_ACA"
)
#> [1] "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa?ST=S_D&DT=D_SP&AT=A_BIO&AN=CU&OT=O_ACA"
```
