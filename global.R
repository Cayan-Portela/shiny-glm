list.of.packages <- c("DT", "car","hnp","purrr","dplyr","shiny","scales",
                      "plotly","ggplot2","ggthemes","ggfortify","shinydashboard",
                      "shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(DT)
library(car)
library(hnp)
library(purrr)
library(dplyr)
library(shiny)
library(scales)
library(plotly)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(shinydashboard)
library(shinycssloaders)
