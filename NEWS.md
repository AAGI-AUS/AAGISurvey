# AAGISurvey 2.0.1

## Bug fix

- Add missing Import for {withr}.

# AAGISurvey 2.0.0

## Breaking changes

- `build_url()` now takes `base` as its last argument, and it is optional.
  Code that passed `base` as the first positional argument must now name it (`base = "https://..."`) or move it to the end.

## New features

- New exported function `survey_base_url()` returns the base URL of the AAGI Qualtrics survey.
  The URL is now defined in one place (`SURVEY_BASE_URL` in `constants.R`), so dependent packages and apps, such as the Shiny app, can use it instead of hard coding their own copy.

- `build_url()` uses `survey_base_url()` as the default for `base`.
  `create_survey_url()` no longer hard codes the URL.

## Bug fixes

- `create_survey_url()` now prints the "Support type" line in its summary.
  It was never shown before.

- `create_survey_url()` now shows `<blank>` in the summary for a design or analysis type that doesn't apply.
  It previously showed nothing.

- `create_survey_url()` now stops with "You must select a support type" if the support type menu is cancelled.
  Previously it carried on without a support type.

## Minor improvements and internal changes

- `create_survey_url()` was split into smaller internal helpers (`resolve_support_type()`, `prompt_if_missing()`, `label_for()` and `print_survey_summary()`) to reduce its cyclomatic complexity.
  Its behaviour is unchanged apart from the fixes above.

- Added an internal `select_one()` wrapper around `utils::select.list()` so the interactive menu can be mocked in tests.

- Expanded test coverage for `build_url()`, `create_survey_url()`, `copy_to_clip()` and `survey_base_url()`.
  The clipboard is now mocked, so tests no longer skip when no clipboard is available.

# AAGISurvey 1.0.1

- Provide new URL for survey, necessary since the survey was moved from Rose's Curtin Qualtrics to Adam's Curtin Qualtrics account.

# AAGISurvey 1.0.0

## Minor changes

- Add ANU, QDPI and UWA to nodes.
- Updates text prompts for clarity.

## Breaking changes

- Users can no longer select more than one service type.

## Bug fixes

- Replace more instances of UA/University of Adelaide with AU/Adelaide University.

# AAGISurvey 0.0.3

## Minor changes

- Update "University of Adelaide" to "Adelaide University".

# AAGISurvey 0.0.2

## Minor changes

- Adds [vignette](https://aagi-aus.github.io/AAGISurvey/articles/AAGISurvey.html) from Rose on how to use the survey.

# AAGISurvey 0.0.1

- Initial release.
