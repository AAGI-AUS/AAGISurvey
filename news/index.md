# Changelog

## AAGISurvey 2.0.1

### Bug fix

- Add missing Import for {withr}.

## AAGISurvey 2.0.0

### Breaking changes

- [`build_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/build_url.md)
  now takes `base` as its last argument, and it is optional. Code that
  passed `base` as the first positional argument must now name it
  (`base = "https://..."`) or move it to the end.

### New features

- New exported function
  [`survey_base_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/survey_base_url.md)
  returns the base URL of the AAGI Qualtrics survey. The URL is now
  defined in one place (`SURVEY_BASE_URL` in `constants.R`), so
  dependent packages and apps, such as the Shiny app, can use it instead
  of hard coding their own copy.

- [`build_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/build_url.md)
  uses
  [`survey_base_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/survey_base_url.md)
  as the default for `base`.
  [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
  no longer hard codes the URL.

### Bug fixes

- [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
  now prints the “Support type” line in its summary. It was never shown
  before.

- [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
  now shows `<blank>` in the summary for a design or analysis type that
  doesn’t apply. It previously showed nothing.

- [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
  now stops with “You must select a support type” if the support type
  menu is cancelled. Previously it carried on without a support type.

### Minor improvements and internal changes

- [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
  was split into smaller internal helpers
  ([`resolve_support_type()`](https://AAGI-AUS.github.io/AAGISurvey/reference/resolve_support_type.md),
  [`prompt_if_missing()`](https://AAGI-AUS.github.io/AAGISurvey/reference/prompt_if_missing.md),
  [`label_for()`](https://AAGI-AUS.github.io/AAGISurvey/reference/label_for.md)
  and
  [`print_survey_summary()`](https://AAGI-AUS.github.io/AAGISurvey/reference/print_survey_summary.md))
  to reduce its cyclomatic complexity. Its behaviour is unchanged apart
  from the fixes above.

- Added an internal `select_one()` wrapper around
  [`utils::select.list()`](https://rdrr.io/r/utils/select.list.html) so
  the interactive menu can be mocked in tests.

- Expanded test coverage for
  [`build_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/build_url.md),
  [`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md),
  [`copy_to_clip()`](https://AAGI-AUS.github.io/AAGISurvey/reference/copy_to_clip.md)
  and
  [`survey_base_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/survey_base_url.md).
  The clipboard is now mocked, so tests no longer skip when no clipboard
  is available.

## AAGISurvey 1.0.1

- Provide new URL for survey, necessary since the survey was moved from
  Rose’s Curtin Qualtrics to Adam’s Curtin Qualtrics account.

## AAGISurvey 1.0.0

### Minor changes

- Add ANU, QDPI and UWA to nodes.
- Updates text prompts for clarity.

### Breaking changes

- Users can no longer select more than one service type.

### Bug fixes

- Replace more instances of UA/University of Adelaide with AU/Adelaide
  University.

## AAGISurvey 0.0.3

### Minor changes

- Update “University of Adelaide” to “Adelaide University”.

## AAGISurvey 0.0.2

### Minor changes

- Adds
  [vignette](https://aagi-aus.github.io/AAGISurvey/articles/AAGISurvey.html)
  from Rose on how to use the survey.

## AAGISurvey 0.0.1

- Initial release.
