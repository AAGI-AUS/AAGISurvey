# AAGISurvey

<!-- badges: start -->

[![R-CMD-check](https://github.com/AAGI-AUS/AAGISurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AAGI-AUS/AAGISurvey/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation instructions

{AAGISurvey} is available through the [R-Universe](https://aagi-aus.r-universe.dev/packages) with pre-built binaries that will install as if it was available from CRAN.

To get started:

### Enable this universe

Note that you only need to do this once.
If you have done this before for another AAGI package, you can skip this step.

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

Currently, {AAGISurvey} only contains one function, `create_survey_url()`, which creates a URL that links to a Qualtrics survey for you to send to AAGI Service and Support partners for completion.

Creation of the URL can be either scripted where you provide all the necessary arguments to the `create_survey_url()` or interactively through a menu-driven options system.
The menu will step through the needed arguments and provide help text for each.
At the end of the menu (or if you have scripted it) you will be provided with the URL to copy and paste into an email or other communication that is automatically copied to your OS's clipboard for you.

```r
library(AAGISurvey)
create_survey_url()
```

## Contributions

All contributions are appreciated, but please make sure to follow the [Contribution Guidelines](.github/CONTRIBUTING.md).

### Getting Started with Contributions

1. Clone the repository to your local machine using:

   ```bash
   git clone https://github.com/AAGI-AUS/AAGISurvey.git
   ```

2. Navigate to the project directory:

   ```bash
   cd AAGISurvey
   ```

3. Make some edits to the .R files in the R/ directory.

4. Test your changes by running:

   ```r
   devtools::load_all()
   ```

   and testing your new functionality.

5. If you are adding new functions, make sure to document them using Roxygen2 comments and generate the documentation with:

   ```r
   devtools::document()
   ```

6. When you are satisfied with your changes, commit them to your local repository:

   ```bash
   git add .
   git commit -m "Describe your changes here"
   ```

7. Push your changes to the repository:

   ```bash
   git push
   ```
