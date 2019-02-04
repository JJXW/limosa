library(shiny)
library(shinydashboard)
library(DT)
library(sqldf)
library(dplyr)
library(ggplot2)
library("data.table")

survey_data_default <- read.csv('intellisurvey_demo.csv')