
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shiny.plague

This is an exercise on how to develop a game mostly using Shiny. It is a
shiny application structured as an R package, following the
[golem](https://github.com/ThinkR-open/golem) framework.

## Get Started

First, we’ll need to install all the necessary libraries. A complete
list can be found in the `DESCRIPTION` file. But it’s just easier if you
install the development version of the app and follow the instructions
to install all dependencies.

``` r
# install.packages("devtools")
devtools::install_github("aljrico/shiny-plague")
```

And then you can play the game by simply running the following:

``` r
library(shiny.plague)
shiny.plague::run_app()
```

## Walkthrough

This exercise is part of a Workshop in 2022’s
[EARL](https://www.ascent.io/earl) conference. The slides of which can
be found within the `presentation` folder.
