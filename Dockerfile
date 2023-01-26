FROM rocker/shiny:4.2.1
RUN install2.r shiny ggplot2 xlsx readxl dplyr shinyWidgets shinythemes ggpubr see DT stringr tidyr labelled ggrepel lubridate data.table curl rsconnect
WORKDIR /home/MSapp
COPY make_data.R make_data.R 
COPY app.R app.R 
COPY trials_clean.xlsx trials_clean.xlsx
COPY complete_df_NCT_drugs.xlsx complete_df_NCT_drugs.xlsx
COPY Druglist.xlsx Druglist.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R
