## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
source("R/bookmarks.R")


## ------------------------------------------------------------------------
library(flextable)
#bookmark(x = flextable(mtcars), name = "table1")


## ------------------------------------------------------------------------
bookmark(x = "_And_ what about this?", name = "bullshit")


## ------------------------------------------------------------------------
6.234 %>% as_bookmark(name = "d")

