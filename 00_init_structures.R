library(openxlsx)
library(data.table)
col_names <-c("registre","nom","cognom1","cognom2","edat","email","cp",
              "sexe","carnet_conduir",
              "1a","1b","1c","1d",
              "2a","2b","2c","2d",
              "3a","3b","3c","3d","3e","3f",
              "4a","4b","4c",
              "4bis-a","4bis-b","4bis-c","4bis-d",
              "5a","5b","5c","5d","5e","5f","5altres",
              "6a","6b","6c","6d","6e",
              "7a","7b","7c","7d",
              "8a","8b","8c","8d","8e","8f","8altres",
              "9positiu","9negatiu",
              "10a","10b","10c","10d","10e","10f","10g","10altres",
              "11a","11b","11c","11d","11e","11altres",
              "12a","12b","12c",
              "12bis1-a","12bis1-b","12bis1-c","12bis1-d","12bis1-e","12bis1-f",
              "12bis2-a","12bis2-b","12bis2-c","12bis2-d","12bis2-e","12bis2-f",
              "segura","segurb","segurc")
create_initial_table <- function(){
  poll_data <- data.table(matrix(ncol=length(col_names),nrow=0))
  colnames(poll_data)<-col_names
  poll_data <- poll_data[, lapply(.SD, as.character)]
}