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


complete_df_NCT_drugs <- read_excel("complete_df_NCT_drugs.xlsx") %>% mutate(date=Sys.time())
complete_df_NCT_drugs$Gender<-as.factor(complete_df_NCT_drugs$Gender)
complete_df_NCT_drugs$StdAge<-as.factor(complete_df_NCT_drugs$StdAge)
complete_df_NCT_drugs$Phase<-as.factor(complete_df_NCT_drugs$Phase)

complete_df_NCT_drugs$LocationCountry <- sapply(complete_df_NCT_drugs$LocationCountry, function(x) paste(unique(unlist(str_split(x,"[|]"))), collapse = ", "))
complete_df_NCT_drugs$InterventionType <- sapply(complete_df_NCT_drugs$InterventionType, function(x) paste(unique(unlist(str_split(x,"[|]"))), collapse = ", "))
complete_df_NCT_drugs<-
  complete_df_NCT_drugs %>% 
  mutate(LocationCountry=gsub("NA",NA,LocationCountry))

complete_df_NCT_drugs$URL<-paste0("https://clinicaltrials.gov/ct2/show/",complete_df_NCT_drugs$NCTId,
                                  "?term=",complete_df_NCT_drugs$NCTId,"&draw=2&rank=1")
complete_df_NCT_drugs$hyperlink<-paste0("<a href=","\"",complete_df_NCT_drugs$URL,"\"",">",complete_df_NCT_drugs$NCTId,"</a>")

complete_df_NCT_drugs$pubmed_URL<-paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",complete_df_NCT_drugs$NCTId)
complete_df_NCT_drugs$pubmed_hyperlink<-paste0("<a href=","\"",complete_df_NCT_drugs$pubmed_URL,"\"",">Pubmed search</a>")

data<-complete_df_NCT_drugs %>% 
  select(hyperlink,PrimaryOutcomeMeasure,SecondaryOutcomeMeasure,LocationCountry,Of_Drug_Name,BriefTitle,StdAge,Phase,OverallStatus,Gender,pubmed_hyperlink) %>% 
  rename(NCT=hyperlink,
         Pubmed=pubmed_hyperlink,
         `Primary Outcome`=PrimaryOutcomeMeasure,
         `Secondary Outcome`=SecondaryOutcomeMeasure,
         Location=LocationCountry,
         `Treatment Name`=Of_Drug_Name,
         `Brief Title of Clinical Trial`=BriefTitle,
         `Age of Participants`=StdAge,
         Phase=Phase,
         Status=OverallStatus,
         Sex=Gender
         ) 


ui_MS <- 
                    fluidPage(
  theme = shinytheme("yeti"),
  titlePanel(title=h2("Multiple Sclerosis treatments")),
  
           column(3,
           helpText("Here you can filter the displayed results:"),
                  pickerInput("Sex","Select Sex:",
                              selected=c("All","Female","Male"),
                              choices =c("All","Female","Male"),
                              options = list(`actions-box` = TRUE,`selected-text-format` = paste0("count >",2),
                                             `count-selected-text` = "All"),multiple = T),
                  pickerInput("age","Select Age:",
                              selected=c("Adult","Child","Older Adult"),
                              choices = c("Adult","Child","Older Adult"),
                              options = list(`actions-box` = TRUE,`selected-text-format` = paste0("count >",2),
                                             `count-selected-text` = "All"),multiple = T),
                  pickerInput("phase","Select Phase:",
                              selected=c(levels(as.factor(data$Phase))),
                              choices = c(levels(as.factor(data$Phase))),
                              options = list(`actions-box` = TRUE,`selected-text-format` = paste0("count >",4),
                                             `count-selected-text` = "All"),multiple = T),
                  selectizeInput(
                    inputId = 'search_drug',
                    label = 'Select one or more treatments:',
                    choices = c(unique(data$`Treatment Name`)),
                    selected = NULL,
                    multiple = TRUE,
                    options=NULL),
      selectInput(inputId = "select_cols", 
                      label="Select further columns to display", choices=names(data), 
                     selected=c("NCT","Pubmed","Treatment Name","Brief Title of Clinical Trial",
                                "Age of Participants","Sex","Phase"),
                     multiple = TRUE),
      textOutput("fluid")),
           column(9,
                  fluidRow(textOutput("selected_var")),
                  br(),
                  fluidRow(
                    column(4,plotOutput("phase_plot")),
                    column(4,plotOutput("age_plot")),
                    column(4,plotOutput("Sex_plot"))),
                  fluidRow(dataTableOutput("Table"))
                  )
         )


server_MS <- function(input, output) {


  data.d<-reactive({data[grepl(paste(input$search_drug,collapse="|"),data$`Treatment Name`),]})
  data.g<-reactive({data.d()[grepl(paste(input$Sex,collapse="|"),data.d()$Sex),]})
  data.p<-reactive({data.g()[grepl(paste(input$phase,collapse="|"),data.g()$Phase),]})
  data.a<-reactive({data.p()[grepl(paste(input$age,collapse="|"),data.p()$`Age of Participants`),]})

  
  drugs_selected_text<-
  reactive({
    case_when(length(input$search_drug)>4~"more than 4 treatments",
              length(input$search_drug)<=4&length(input$search_drug)>0~paste(input$search_drug,collapse=", "),
              length(input$search_drug)==0~"treatments")
  })
  
    output$Table <- renderDataTable({
        datatable(data.a()[,c(input$select_cols),with=F],escape = F)
    })

    output$selected_var <- renderText({ 
      paste(nrow(data.a()),"output(s) found in which",drugs_selected_text(),"were tested, distributed in clinical phases, Sex, and age as shown below:")
    })
    
    output$fluid <- renderText({
      paste("Latest update:", unique(complete_df_NCT_drugs$date))
    })
    
  output$age_plot<-renderPlot({
    data.a() %>% 
      mutate(`Age of Participants`=gsub("[|]","_",`Age of Participants`))%>% 
      select(`Age of Participants`) %>% 
      separate(`Age of Participants`,into=c("Age1","Age2","Age3"),sep="_") %>% 
      mutate(rowsum=1/rowSums(!is.na(.))) %>% 
      pivot_longer(starts_with("Age")) %>% 
      select(!(name)) %>% 
      drop_na() %>% 
      group_by(value) %>% 
      summarise(summary=sum(rowsum)) %>% 
      mutate(percent=round(summary/sum(summary),3)) %>% 
      mutate(csum = rev(cumsum(rev(percent))), 
             pos = percent/2 + lead(csum, 1),
             pos = if_else(is.na(pos), percent/2, pos)) %>% 
      ggplot(aes(x="", y=percent, fill=value))+
      geom_text_repel(aes(y = pos, label =paste(value,"\n",(paste0(percent*100, "%")))),segment.color = 'grey50',nudge_x = 0.7, show.legend = FALSE)+
      geom_bar(stat="identity")+
      coord_polar(theta="y")+
      theme_void()+
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5,face = "bold",size=14))+
      scale_fill_manual(values=c("#00577e","#ce599e","#ffa600"))+
      ggtitle("Age Distribution")
  })
  
  
  output$phase_plot<-renderPlot({
    data.a() %>% 
      group_by(Phase) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(percent=round((n/sum(n)),2)) %>% 
      mutate(csum = rev(cumsum(rev(percent))), 
             pos = percent/2 + lead(csum, 1),
             pos = if_else(is.na(pos), percent/2, pos)) %>%  
      ggplot(aes(x="", y=percent, fill=Phase))+
      geom_bar(stat="identity")+
      geom_text_repel(aes(y = pos, label =paste(Phase,"\n",(paste0(percent*100, "%")))),segment.color = 'grey50',nudge_x = 0.7, show.legend = FALSE)+
      coord_polar(theta="y")+
      theme_void()+
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5,face = "bold",size=14))+
      scale_fill_manual(values=c("#003f5c","#2f4b7c","#665191","#a05195","#d45087","#f95d6a","#ff7c43","#ffa600"))+
      ggtitle("Phase Distribution")
  })
  
  
  output$Sex_plot<-renderPlot({
    data.a() %>% 
      group_by(Sex) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(Sex=as.character(Sex))%>% 
      tidyr::replace_na(list(Sex = "Unknown")) %>% 
      mutate(percent=round(n/sum(n),3)) %>% 
      mutate(csum = rev(cumsum(rev(percent))), 
             pos = percent/2 + lead(csum, 1),
             pos = if_else(is.na(pos), percent/2, pos)) %>% 
      ggplot(aes(x="", y=percent, fill=Sex))+
      geom_text_repel(aes(y = pos, label =paste(Sex,"\n",(paste0(percent*100, "%")))),segment.color = 'grey50',nudge_x = 0.7, show.legend = FALSE)+
      geom_bar(stat="identity")+
      coord_polar(theta="y")+
      theme_void()+
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5,face = "bold",size=14))+
      scale_fill_manual(values=c("#00577e","#8e5faa","#fc5d7c","#ffa600"))+
      ggtitle("Sex Distribution")
  })
}

# Run the application 
shinyApp(ui = ui_MS, server = server_MS)


