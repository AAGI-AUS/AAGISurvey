# Create an AAGI Survey URL

This function is designed to be used each time you need to generate a
survey URL for requesting feedback from a partner. It embeds information
already known by the team so that respondents do not need to provide it.
This includes the type of support provided, the output delivered, the
AAGI node that delivered the work, and the organisation type of the
partner. These details are included as metadata within the URL so that
the survey platform can automatically link each respondents feedback to
the relevant contextual information.

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

  Character string specifying the type of support being evaluated. You
  must choose one. Options are:

  "S_D"

  :   Experimental design support

  "S_A"

  :   Analysis support

- design_type:

  Character string specifying the type of experimental design (required
  if "S_D" is selected for `support_type`). You must choose one.'
  Options are:

  "D_SP"

  :   Small plot trial design

  "D_OFE"

  :   On farm experiment design

  "D_GH"

  :   Glasshouse experiment design

  "D_GC"

  :   Growth chamber experiment design

  "D_LAB"

  :   Lab experiment design

  "D_OTHER"

  :   Other design type chosen by the recipient

- analysis_type:

  Character string of analysis type. Required if "S_A" is selected for
  `support_type`. Options are:

  "A_SP"

  :   Small plot trial data analysis

  "A_OFE"

  :   On farm experiment data analysis

  "A_PRO"

  :   Protected environment experiment data analysis (glasshouse or
      growth chamber)

  "A_BIO"

  :   Bioinformatics or genetic data

  "A_BRE"

  :   Breeding or selection program data

  "A_ENV"

  :   Environmental or geospatial data

  "A_IMG"

  :   Imagery data

  "A_REC"

  :   Farm records or monitoring data

  "A_OTHER"

  :   Other type to be selected by recipient

- aagi_node:

  Character string of AAGI node. Options are:

  "ANU"

  :   Australian National University

  "AU"

  :   Adelaide University

  "CU"

  :   Curtin University

  "QDPI"

  :   Queensland Department of Primary Industries

  "UQ"

  :   University of Queensland

  "UWA"

  :   University of Western Australia

- organisation_type:

  Character string of organisation type. Options are:

  "O_GRO"

  :   Grower group, industry association or farm cooperative

  "O_AGR"

  :   Agronomy or farm advisory practice

  "O_ACA"

  :   Academic institution

  "O_GOV"

  :   Government agency or department

  "O_BRE"

  :   Seed or breeding company

  "O_TEC"

  :   Technology, biotechnology or chemical company

  "O_OTHER"

  :   Other organisation type chosen by the recipient

## Value

Once the relevant information has been provided, the function returns
the completed survey URL, prints a short summary of the details you
supplied and copies the URL to your operating system clipboard for
inclusion in an email or other communication.

## Details

You can use the function in two ways. You can supply all required
arguments directly in your script, or you can run the function in
interactive mode. In interactive use, the function presents a menu in
the console that guides you through each field.

## Author

Rose Megirian, <rose.megirian@curtin.edu.au> and Adam H. Sparks,
<adam.sparks@curtin.edu.au>

## Examples

``` r
if (FALSE) { # interactive()
# create a survey URL for
# - a small plot trial design
# - for a government agency or department
# - performed by Curtin node
create_survey_url(
  support_type = "S_D",
  design_type = "D_SP",
  aagi_node = "CU",
  organisation_type = "O_GOV"
)

# create a survey URL for
# - a bioinformatics analysis
# - for an academic institution
# - performed by Adelaide University node
create_survey_url(
  support_type = "S_A",
  analysis_type = "A_BIO",
  aagi_node = "AU",
  organisation_type = "O_ACA"
)
}
```
