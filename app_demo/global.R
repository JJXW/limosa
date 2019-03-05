library(shiny)
library(shinydashboard)
library(DT)
library(sqldf)
library(ggplot2)
library("data.table")
library(clustMixType)
library(klaR)
require(plyr)
library(dplyr)


survey_data_default <- read.csv('intellisurvey_demo.csv')
