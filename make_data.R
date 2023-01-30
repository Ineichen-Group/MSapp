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
library(labelled)
library(tidyr)
library(ggrepel)
library(lubridate)
library(curl)
library(data.table)

# download all the records matching our search

URL1a<-"https://clinicaltrials.gov/api/query/study_fields?expr=AREA%5BCondition%5D%22multiple+sclerosis%22+AND+AREA%5BStudyType%5DInterventional&fields=NCTId%2COfficialTitle%2CBriefTitle%2CPrimaryOutcomeMeasure%2CPrimaryOutcomeDescription%2CSecondaryOutcomeMeasure%2CSecondaryOutcomeDescription%2CStdAge%2CMaximumAge%2CMinimumAge%2CGender%2CDetailedDescription&min_rnk="
URL1b<-"&max_rnk="
URL1c<-"&fmt=csv"

nStudiesFound<-read.csv(paste0(URL1a,1,URL1b,1000,URL1c)) %>%
  rename(col = 1) %>% 
  filter(grepl("NStudiesFound",col)) %>% 
  mutate(col=gsub("NStudiesFound: ","",col)) %>% 
  mutate(col=as.numeric(col)) %>% 
  pull() 

empty_df1<-data.frame()
for(i in c(seq(0,floor(nStudiesFound/1000)*1000,1000))){
  URL_pasted<-paste0(URL1a,i+1,URL1b,i+1000,URL1c)
  data_frame<-data.frame(fread(URL_pasted))
  empty_df1<-rbind(empty_df1,data_frame)
}


URL2a<-"https://clinicaltrials.gov/api/query/study_fields?expr=AREA%5BCondition%5D%22multiple+sclerosis%22+AND+AREA%5BStudyType%5DInterventional&fields=NCTId%2CBriefSummary%2CCondition%2CInterventionName%2CInterventionType%2CPhase%2COverallStatus%2CStudyType%2CLocationCountry%2CStudyFirstPostDate%2CLastUpdatePostDate%2CPrimaryCompletionDate%2CConditionMeshTerm&min_rnk="
URL2b<-"&max_rnk="
URL2c<-"&fmt=csv"

empty_df2<-data.frame()
for(i in c(seq(0,floor(nStudiesFound/1000)*1000,1000))){
  URL_pasted<-paste0(URL2a,i+1,URL2b,i+1000,URL2c)
  data_frame<-data.frame(fread(URL_pasted))
  empty_df2<-rbind(empty_df2,data_frame)
}

empty_df<-merge(empty_df1,empty_df2,by="NCTId")

Count_interventions <- data.frame(count=str_count(empty_df$InterventionName,"[|]")) %>% drop_na() %>% pull()
max_interventions<-max(Count_interventions)+1  

df_clean<-empty_df%>% 
  select(NCTId,InterventionName,InterventionType,BriefTitle) %>% 
  mutate(across(starts_with("Intervention"), gsub, pattern = "[|]", replacement = "_"))%>% 
  separate(InterventionName,into=paste0("pivinterv_name",1:max_interventions),sep="_") %>% 
  separate(InterventionType,into=paste0("pivinterv_type",1:max_interventions),sep=",") %>% 
  pivot_longer(cols=starts_with("pivinterv"),
               names_to = c(".value", "Category"), 
               names_sep = "_") %>%
  mutate(Category2=ifelse(grepl("name",Category),"name","type")) %>% 
  mutate(Category=gsub("name","",Category),Category=gsub("type","",Category)) %>% 
  pivot_wider(names_from=Category2,values_from=pivinterv) %>% 
  drop_na(name) %>% 
  fill(type)%>% 
  rename(InterventionName=name,InterventionType=type)
  # filter(InterventionType=="Drug"|InterventionType=="Biological")

#saving the cleaned new data
write.xlsx(df_clean,"trials_clean.xlsx")

#uploading our existing table
complete_df_NCT_drugs <- read_excel("complete_df_NCT_drugs.xlsx") %>% 
  unique() %>% 
  select("Of_Drug_Name",
         "NCTId",
         "OfficialTitle",
         "BriefTitle",
         "PrimaryOutcomeMeasure",
         "PrimaryOutcomeDescription",
         "SecondaryOutcomeMeasure",
         "SecondaryOutcomeDescription",
         "StdAge",
         "MaximumAge",
         "MinimumAge",
         "Gender",
         "DetailedDescription",
         "BriefSummary",
         "Condition",
         "InterventionName",
         "InterventionType",
         "Phase",
         "OverallStatus",
         "StudyType",
         "LocationCountry",
         "StudyFirstPostDate",
         "LastUpdatePostDate",
         "PrimaryCompletionDate",
         "ConditionMeshTerm",
         "URL")

# retreiving new records (i.e. new NCTIds) from the newly downloaded (and cleaned) data frame
anti_match_NCTs<-
  anti_join(df_clean %>% select(NCTId),complete_df_NCT_drugs%>% select(NCTId)) %>% unique() %>% 
  merge(.,empty_df,by="NCTId")

# uploading dictionary manually curated

dictionary<-read_excel("Druglist.xlsx") %>% 
  rename(Of_Drug_Name=Name)

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


#append additional information to the original table 
complete_df_NCT_drugs<-merge(complete_df_NCT_drugs,dictionary,by="Of_Drug_Name")

#new table to append to the data set 

mined_table<-empT %>% 
  filter(!(is.na(replace))) %>% 
  unique() %>% 
  merge(.,dictionary_pivot,by.x="replace",by.y="lower_synonims",all.x=T)%>% 
  mutate(URL=paste0("https://clinicaltrials.gov/ct2/show/",NCTId,"?term=",NCTId,"&draw=2&rank=1"))

# appending to the original dataset

bind_rows(complete_df_NCT_drugs, mined_table) %>% 
  write.xlsx("complete_df_NCT_drugs.xlsx")

# table for unmined data

empT %>% 
  select(NCTId) %>% 
  anti_join(.,mined_table %>% select(NCTId)) %>% 
  merge(.,empT %>% filter(is.na(replace)))%>% unique() %>% 
  write.xlsx("unmined_interventions.xlsx") 
