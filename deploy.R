library(rsconnect)

# Authenticate
setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))
# Deploy
deployApp(appFiles = c("app_MultipleSclerosis.R","complete_df_NCT_drugs.xlsx"))
