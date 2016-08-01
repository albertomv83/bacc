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