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
  select(NCTId,InterventionName,InterventionType) %>% 
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
  rename(InterventionName=name,InterventionType=type) %>% 
  filter(InterventionType=="Drug"|InterventionType=="Biological")

#uploading our existing table
complete_df_NCT_drugs <- read_excel("complete_df_NCT_drugs.xlsx")

# retreiving new records (i.e. new NCTIds) from the newly downloaded (and cleaned) data frame
anti_match_NCTs<-
  anti_join(df_clean %>% select(NCTId),complete_df_NCT_drugs%>% select(NCTId)) %>% unique() %>% 
  merge(.,empty_df,by="NCTId")

