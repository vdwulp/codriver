# 🛣️ codriver → addin for RStudio
<img src="man/figures/logo.png" align="right" width="139" style="float:right; width:139px;" class="pkgdown-hide">

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/codriver)](https://CRAN.R-project.org/package=codriver)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/vdwulp/codriver/blob/master/LICENSE.md)
[![R-CMD-check](https://github.com/vdwulp/codriver/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vdwulp/codriver/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*A context-aware AI assistant for RStudio that is literally at your fingertips.*

Codriver works directly in the RStudio source editor, without switching context
or opening a separate chat window. When you press the keyboard shortcut,
codriver reads your cursor position and selection to choose one of five modes —
complete, generate, continue, edit, or comment — and presents suggestions
in-place.

## 📋 Requirements

- RStudio **≥ 2026.04**
- **API access** to an LLM provider or local model server

## ✈️ Installation

Run this R command to install the latest release of codriver from [CRAN](https://CRAN.R-project.org/package=codriver):

``` r
install.packages("codriver")
```

Run these R commands to install the development version of codriver from [GitHub](https://github.com/vdwulp/codriver):

```r
# Install remotes package if necessary
install.packages("remotes")

# Install codriver from GitHub
remotes::install_github("vdwulp/codriver")
```

## ⚙️ Configuration

Configure codriver to connect to your LLM provider or local model server:

```r
# OpenAI
codriver::codriver_configure("openai", model = "gpt-4.1")

# Anthropic Claude
codriver::codriver_configure("anthropic", model = "claude-sonnet-4-5")

# OpenAI-compatible endpoint
codriver::codriver_configure("openai_compatible",
  base_url = "https://llmproxy.example.com/v1",
  model    = "gpt-4.1"
)
```

Codriver builds on [ellmer](https://ellmer.tidyverse.org) and supports any
LLM provider ellmer supports, covering a wide range of cloud and local model
servers. For most providers, ellmer reads API credentials from an **environment
variable**, for example `OPENAI_API_KEY`, `ANTHROPIC_API_KEY` or
`GOOGLE_API_KEY`. See
[Getting started](https://vdwulp.github.io/codriver/articles/getting-started-with-codriver.html)
for more information on provider configuration.

## 🚀 Usage

> **Restart RStudio after configuration** for the keyboard shortcut to take effect.

▶️ **Invoke codriver** — press **`Ctrl+/`** in the RStudio source editor. Codriver
takes it from there.

_A keyboard shortcut is strongly recommended for the best codriver experience.
If you skipped registering the default shortcut `Ctrl+/` during configuration or
want to change it later, use **Tools > Modify Keyboard Shortcuts** in RStudio._

Codriver determines the mode from your cursor position and any selection. At a
high level:

| Mode | Trigger | Action |
|---|---|---|
| **complete** | At end of a code line | Completes the line |
| **generate** | On a comment line | Uses the comment as an instruction to generate new code |
| **continue** | In whitespace | Generates a new line or block from context only |
| **edit** | In existing code | Suggests improvements or fixes |
| **comment** | In an inline comment | Suggests or improves a short inline comment |

Results appear as **ghost text** - gray italic, accept with `TAB`, dismiss by
moving the cursor - or as an **edit suggestion** with green/red highlighting -
accept with `TAB` or _Apply_, dismiss with `Escape` or _Discard_.

See [Using codriver](https://vdwulp.github.io/codriver/articles/start-using-codriver.html)
for a full walkthrough of all modes and result presentation.

## 🔒 Data privacy

Codriver sends (parts of) your editor context and session information directly
to the LLM provider or model server you configure. There is no intermediary
server. Sent information includes metadata about objects in the RStudio global
environment (names, named elements, types and dimensions), but _not_ data
values. Be aware of this before using Codriver in projects with sensitive or
proprietary information.

Please review your provider's data policy before use.

## 📄 License

MIT License, see [LICENSE](https://github.com/vdwulp/codriver/blob/master/LICENSE.md) file.
