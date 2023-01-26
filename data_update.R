library(shiny)
library(ggplot2)
library(xlsx)
library(readxl)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(ggpubr)
library(see)
library(DT)
library(stringr)
library(tidyr)
library(labelled)
library(ggrepel)
library(lubridate)
library(curl)
library(data.table)

# appending to the original dataset

bind_rows(complete_df_NCT_drugs, mined_table) %>% 
  mutate(date=Sys.Date()) %>% 
  unique() %>% 
  write.xlsx("complete_df_NCT_drugs.xlsx")

# table for unmined data

unmined_table<- empT %>% 
  filter(is.na(replace)) %>% 
  write.xlsx("unmined_interventions.xlsx") 

#what is needed here is to add info to the dictionary and decide whether trials are to be included or not


# Manual update necessary 
