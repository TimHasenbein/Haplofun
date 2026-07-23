##**************************##
##  Haplofun: app function  ##
##**************************##
## Project: haplofun
## Last modification 07.2026
## Creation: 2023
library(shiny)
library(vcfR)
library(pegas)
library(data.table)
library(dplyr)
library(ggdendro)
library(ggplot2)
library(shinycssloaders)
library(graphics)
library(shinyFeedback)
library(tools)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)
library(reshape2)
library(GenomicRanges)
source('./appUI.R')
source('./appServer.R')
source('./global.R') 

# Run the application
shinyApp(ui = ui, server = server)
