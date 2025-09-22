# AAGISurvey

<!-- badges: start -->

[![R-CMD-check](https://github.com/AAGI-AUS/AAGISurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AAGI-AUS/AAGISurvey/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation instructions

{AAGISurvey} is available through the [R-Universe](https://aagi-aus.r-universe.dev/packages) with pre-built binaries that will install as if it was available from CRAN.

To get started:

### Enable this universe

```r
options(repos = c(
    aagi_aus = 'https://aagi-aus.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
```

### Install

```r
install.packages("AAGISurvey")
```

# Quick start

Currently {AAGISurvey} only contains one function, `create_survey_url()`, which creates a URL that links to a Qualtrics survey for you to send to AAGI Service and Support partners for completion.

Creation of the URL can be either scripted where you provide all of of the necessary arguments to the `create_survey_url()` or interactively through a menu-driven options system.
The menu will step through the needed arguments and provide help text for each.
At the end of the menu (or if you have scripted it) you will be provided with the URL to copy and paste into an email or other communication that is automatically copied to your OS's clipboard for you.

```r
library(AAGISurvey)
create_survey_url()
```

## Contributions

All contributions are appreciated, but please make sure to follow the [Contribution Guidelines](.github/CONTRIBUTING.md).
