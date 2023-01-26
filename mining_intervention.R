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
library(shinysky)
library(labelled)
library(ggrepel)
library(lubridate)
library(data.table)

# uploading dictionary manually curated

dictionary<-read_excel("Druglist.xlsx")

# cleaning the dictionary

dictionary_pivot<-
  dictionary %>%    
  separate(Intervention,into=paste0("pivinterv_synonim",1:(max(data.frame(count=str_count(dictionary$Intervention,";")) %>% drop_na() %>% pull())+1)),sep=";") %>% 
  pivot_longer(starts_with("pivinterv_synonim"),names_to="delete",values_to="synonims") %>% 
  select(!(delete)) %>% 
  drop_na(synonims)%>% 
  mutate(synonims=str_trim(synonims, side = c("both"))) %>% 
  mutate(lower_synonims=tolower(synonims))%>% 
  mutate(lower_synonims=gsub("-"," ",lower_synonims)) %>% 
  filter(!(lower_synonims==""|is.na(lower_synonims)))

# subsetting the dictionary to allow smooth screening

list<-split(dictionary_pivot, (seq(nrow(dictionary_pivot))) %/% 200)
names(list) <- 0:max(as.numeric(names(list)))+1

# loop preparation

empT<-data.frame()  
index_patter<-max(as.numeric(names(list)))

#mining for the intervention in InterventionName and Brief title

for(i in 1:index_patter){
  data<-list[[i]]$lower_synonims
  pattern<-paste0(data, collapse = "|")
  df<-anti_match_NCTs %>% 
    mutate(lower_intervention=tolower(InterventionName),lower_title=tolower(BriefTitle))%>%
    mutate(lower_intervention=gsub("-"," ",lower_intervention),lower_title=gsub("-"," ",lower_title)) %>%
    mutate(match_detect_intervention=str_detect(lower_intervention, pattern)) %>% 
    mutate(match_detect_BT=str_detect(lower_title, pattern)) %>% 
    mutate(replace=case_when(
      match_detect_intervention==T&match_detect_BT==T~regmatches(lower_intervention, gregexpr(pattern, lower_intervention)),
      match_detect_intervention==T&match_detect_BT==F~regmatches(lower_intervention, gregexpr(pattern, lower_intervention)),
      match_detect_intervention==F&match_detect_BT==T~regmatches(lower_title, gregexpr(pattern, lower_title)))) %>% 
    unnest_longer(replace)
  empT<-rbind(empT,df) %>% unique
}


#new table to append to the data set 

mined_table<-empT %>% 
  filter(!(is.na(replace))) %>% 
  unique() %>% 
  merge(.,dictionary_pivot,by.x="replace",by.y="lower_synonims",all.x=T)%>% 
  mutate(URL=paste0("https://clinicaltrials.gov/ct2/show/",NCTId,"?term=",NCTId,"&draw=2&rank=1"))

