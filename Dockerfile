FROM rocker/shiny:4.2.1
RUN install2.r shiny ggplot2 xlsx readxl dplyr shinyWidgets shinythemes ggpubr see DT stringr tidyr labelled ggrepel lubridate data.table
WORKDIR /home/shinytweet
COPY make_data.R make_data.R 
COPY mining_intervention.R mining_intervention.R 
COPY data_update.R data_update.R
CMD Rscript make_data.R mining_intervention.R data_update.R
