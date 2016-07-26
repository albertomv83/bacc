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
read_text_var <- function(dt,var_name){
  input <- tolower(readline(paste0(var_name,': ')))
  input <- ifelse(input=="",NA,input)
  setnames(dt,var_name,"xxxXxxx")
  dt[nrow(dt),xxxXxxx:=input]
  setnames(dt,"xxxXxxx",var_name)
}
read_two_options_var <- function(dt,var_name, options){
  input <- tolower(readline(paste0(var_name,'(',paste(options,collapse=','),'): ')))
  input <- ifelse(input %in% options,input,NA)
  setnames(dt,var_name,"xxxXxxx")
  dt[nrow(dt),xxxXxxx:=input]
  setnames(dt,"xxxXxxx",var_name)
}
read_one_option_var <- function(dt,var_name, options){
  input <- tolower(readline(paste0(var_name,'(',paste(options,collapse=','),'): ')))
  input <- ifelse(input=="",NA,input)
  if (input %in% options){
    setnames(dt,paste0(var_name,input),"xxxXxxx")
    dt[nrow(dt),xxxXxxx:='x']
    setnames(dt,"xxxXxxx",paste0(var_name,input))
  }else if("altres" %in% options & !is.na(input)){
    setnames(dt,paste0(var_name,'altres'),"xxxXxxx")
    dt[nrow(dt),xxxXxxx:=input]
    setnames(dt,"xxxXxxx",paste0(var_name,'altres'))
  }
}
read_multiple_option_var <- function(dt,var_name, options){
  input <- tolower(readline(paste0(var_name,'(',paste(options,collapse=','),'): ')))
  answers <- strsplit(input,split=",",fixed=T)
  for(answer in answers[[1]]){
    if (answer %in% options){
      setnames(dt,paste0(var_name,answer),"xxxXxxx")
      dt[nrow(dt),xxxXxxx:='x']
      setnames(dt,"xxxXxxx",paste0(var_name,answer))
    }else if("altres" %in% options & !is.na(answer)){
      setnames(dt,paste0(var_name,'altres'),"xxxXxxx")
      dt[nrow(dt),xxxXxxx:=answer]
      setnames(dt,"xxxXxxx",paste0(var_name,'altres'))
    }
  }
}

read_one_line <- function(poll_data){
  new_line <- data.table(matrix(ncol=length(col_names),nrow=1))
  colnames(new_line)<-col_names
  poll_data <- rbind(poll_data,new_line)
  read_text_var(poll_data,"registre")
  read_text_var(poll_data,"nom")
  read_text_var(poll_data,"cognom1")
  read_text_var(poll_data,"cognom2")
  read_text_var(poll_data,"edat")
  read_text_var(poll_data,"email")
  read_text_var(poll_data,"cp")
  read_two_options_var(poll_data,"sexe",c("h","d"))
  read_two_options_var(poll_data,"carnet_conduir",c("s","n"))
  read_one_option_var(poll_data,"1",c("a","b","c","d"))
  read_one_option_var(poll_data,"2",c("a","b","c","d"))
  read_one_option_var(poll_data,"3",c("a","b","c","d","e","f"))
  read_one_option_var(poll_data,"4",c("a","b","c"))
  read_one_option_var(poll_data,"4bis-",c("a","b","c","d"))
  read_multiple_option_var(poll_data,"5",c("a","b","c","d","e","f","altres"))
  read_one_option_var(poll_data,"6",c("a","b","c","d","e"))
  read_one_option_var(poll_data,"7",c("a","b","c","d"))
  read_multiple_option_var(poll_data,"8",c("a","b","c","d","e","f","altres"))
  read_text_var(poll_data,"9positiu")
  read_text_var(poll_data,"9negatiu")
  read_multiple_option_var(poll_data,"10",c("a","b","c","d","e","f","g","altres"))
  read_multiple_option_var(poll_data,"11",c("a","b","c","d","e","altres"))
  read_one_option_var(poll_data,"12",c("a","b","c"))
  read_multiple_option_var(poll_data,"12bis1-",c("a","b","c","d","e","f"))
  read_multiple_option_var(poll_data,"12bis2-",c("a","b","c","d","e","f"))
  read_one_option_var(poll_data,"segur",c("a","b","c"))
  poll_data
}

record <- function(filename){
  dt <- readRDS(filename)
  action <- "C"
  while(action=="C"){
    print("Comenzar nuevo registro")
    dt <- read_one_line(dt)
    saveRDS(dt,filename)
    action <- toupper(
      readline("Elija (C)ontinuar, (S)alir o (M)ostrar última línea y continuar (C): "))
    action <- ifelse(action=="",NA,action)
    if(is.na(action)){
      action <- "C"
    }else if(action=="M"){
      print(dt[nrow(dt),])
      action <- "C"
    }
  }
  print("Saliendo")
}