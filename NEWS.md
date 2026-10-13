# codriver (development version)

### Enhancements

* LLM prompts now report packages currently attached in the active R session and
  available for unqualified use. This replaces reporting `library()` calls found
  above the cursor, which may not have been executed, and gives the LLM a more
  accurate signal about the packages available to generated code.

* New function `codriver::codriver_info()` collects the codriver version, active
  configuration, R/RStudio environment details, and token usage for the active
  ellmer session.

* New function `codriver::codriver_last_chat()` returns the system prompt, user
  prompt, and raw assistant response from the most recent codriver request in
  the current R session.

### Other

* Increased test coverage to 100%.

* Documentation updates:
  * New article [Free API access](https://vdwulp.github.io/codriver/articles/free-api-access.html),
  * Edited articles [Getting started](https://vdwulp.github.io/codriver/articles/getting-started-with-codriver.html)
    and [Using codriver](https://vdwulp.github.io/codriver/articles/start-using-codriver.html),
  * Edited intro texts and links to articles in `README`,
  * Added [Umami](https://umami.is/) statistics and
    [GitHub Sponsors](https://github.com/sponsors/vdwulp) button to all pkgdown
    webpages,
  * Expanded package reference index,
  * Added automated spell checking.

* Addressed some linting suggestions.


# codriver 1.0.0

* Initial CRAN submission.
