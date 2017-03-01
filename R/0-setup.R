# ---- 0-setup
library(tidyverse)
library(broom)
library(lme4)
library(grid)
library(png)
library(magrittr)

library(wordsintransition)

img <- function(png_stem, ...) {
  grid.newpage()
  paste0(png_stem, ".png") %>%
    file.path("img", .) %>%
    readPNG() %>%
    rasterGrob(...) %T>%
    grid.draw()
}