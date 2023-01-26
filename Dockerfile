FROM rocker/shiny:4.2.1
RUN install2.r shiny ggplot2 xlsx readxl dplyr shinyWidgets shinythemes ggpubr see DT stringr tidyr labelled ggrepel lubridate data.table curl
WORKDIR /home/MSapp
COPY make_data.R make_data.R 
CMD Rscript make_data.R
