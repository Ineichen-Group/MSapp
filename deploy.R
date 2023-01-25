# Authenticate
setAccountInfo(name = Sys.getenv("ineichengroup"),
               token = Sys.getenv("B919EE93E93687AD1216490ED3861589"),
               secret = Sys.getenv("Na8tUFpfzjL7ILta02TuQGUGaa9Z+nrHL7jUXLJk"))
# Deploy
deployApp(appFiles = c("app_MultipleSclerosis.R","complete_df_NCT_drugs.xlsx"))

