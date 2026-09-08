# GRM Workshop

Materials for the session **Item Response Theory: Graded Response Model (GRM)**.

The site is built with [Quarto](https://quarto.org/) and published to GitHub Pages.

## Contents

- [`index.qmd`](index.qmd) — landing page with the workshop outline and setup instructions
- [`files/intro.qmd`](files/intro.qmd) — the slide deck
- [`files/exercise.qmd`](files/exercise.qmd) / [`files/exercise.Rmd`](files/exercise.Rmd) — hands-on GRM exercise in R (Quarto and R Markdown versions)
- [`data/`](data/) — teaching dataset used in the exercise (NaDiRA)
- [`docs/`](docs/) — rendered site output (Quarto `output-dir`)

## Rendering locally

```r
install.packages(c("tidyverse", "psych", "pak", "mirt", "caret", "skimr", "haven"), dependencies = TRUE)
pak::pak("masurp/ggmirt")
```

```sh
quarto render
```

## License

Materials are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — reuse is welcome as long as the original source is cited.
