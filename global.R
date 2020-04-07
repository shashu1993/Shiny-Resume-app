
library(DT)
library(lazyeval)
library(leaflet)
library(lubridate)
library(plotly)
library(scales)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(visNetwork)

ab <- read.csv('data/ab.csv')

base <- read.csv('data/mlb.csv')

sql_r <- read.csv('data/sql.csv')

nyc <- read.csv('data/nyc.csv')

accumulate_by <- function(dat, var) {
  var <- f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(
    seq_along(lvls), 
    function(x) {
      cbind(
        dat[var %in% lvls[seq(1, x)], ], 
        frame = lvls[[x]]
      )
    }
  )
  bind_rows(dats)
}