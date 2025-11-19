# Create an AAGI Survey URL

An interactive menu-driven function to generate a URL for sharing a
survey with AAGI partners. You can either supply all the necessary
details and script it or use interactively and run the function
answering a menu-driven set of questions to generate the URL.

## Usage

``` r
create_survey_url(
  support_type = NULL,
  design_type = NULL,
  analysis_type = NULL,
  aagi_node = NULL,
  organisation_type = NULL
)
```

## Arguments

- support_type:

  Character vector of support types. Options are:

  "S_D"

  :   Experimental design support

  "S_A"

  :   Analysis support

  You may choose both.

- design_type:

  Character string of design type. Required if "S_D" is selected for
  `support_type`. Options are:

  "D_SP"

  :   Small plot trial design,

  "D_OFE"

  :   On farm experiment design,

  "D_GH"

  :   Glasshouse experiment design,

  "D_GC"

  :   Growth chamber experiment design,

  "D_LAB"

  :   Lab experiment design,

  "D_OTHER"

  :   Other type to be selected by recipient.

- analysis_type:

  Character string of analysis type. Required if "S_A" is selected for
  `support_type`. Options are:

  "A_SP"

  :   Small plot trial data analysis,

  "A_OFE"

  :   On farm experiment data analysis,

  "A_PRO"

  :   Protected environment experiment data analysis (glasshouse or
      growth chamber),

  "A_BIO"

  :   Bioinformatics or genetic data,

  "A_BRE"

  :   Breeding or selection program data,

  "A_ENV"

  :   Environmental or geospatial data,

  "A_IMG"

  :   Imagery data,

  "A_REC"

  :   Farm records or monitoring data,

  "A_OTHER"

  :   Other type to be selected by recipient.

- aagi_node:

  Character string of AAGI node. Options are:

  "CU"

  :   Curtin University,

  "UA"

  :   University of Adelaide,

  "UQ"

  :   University of Queensland.

- organisation_type:

  Character string of organisation type. Options are:

  "O_GRO"

  :   Grower group, industry association, or farm cooperative,

  "O_AGR"

  :   Agronomy or farm advisory practice,

  "O_ACA"

  :   Academic institution,

  "O_GOV"

  :   Government agency or department,

  "O_BRE"

  :   Seed or breeding company,

  "O_TEC"

  :   Tech, biotech, or chemical company,

  "O_OTHER"

  :   Other type to be selected by recipient.

## Value

The full survey URL to the OS clipboard (invisibly). Prints a summary
for convenience.

## Author

Rose Megirian, <rose.megirian@curtin.edu.au> and Adam H. Sparks,
<adam.sparks@curtin.edu.au>

## Examples

``` r
if (FALSE) { # interactive()
# create a survey URL for design and analysis performed on small plots for
# a government agency or department performed by CU
create_survey_url(
  support_type = c("S_D", "S_A"),
  design_type = "D_SP",
  analysis_type = "A_SP",
  aagi_node = "CU",
  organisation_type = "O_GOV"
)

# create a survey URL for a bioinformatics analysis for an academic
# institution performed by UA
create_survey_url(
  support_type = "S_A",
  analysis_type = "A_BIO",
  aagi_node = "UA",
  organisation_type = "O_ACA"
)
}
```
