library(openxlsx)
library(data.table)
replace_text_by_options <- function(dt.in,options,column){
  dt <- copy(dt.in)
  setnames(dt,column,"xXx")
  for(name in names(options)){
    value = options[name]
    dt[grepl(value,xXx,fixed = T),xXx:=gsub(value,substr(name,1,1),xXx,fixed=T)]
  }
  setnames(dt,"xXx",column)
  dt
}

obtain_unique_from_multiple <- function(options){
  all <- paste(options[!is.na(options)], collapse=', ')
  all <- gsub(", ([A-ZAÀÉÈÍÓÒÚÏÜ])","#\\1",all)
  unique(unlist(strsplit(all, "#")))
}

bacc <- as.data.table(read.xlsx("respostes.xlsx"))

#save question titles for later
question_titles <- gsub("[.]"," ",names(bacc))

#undo option names
r1 <-unique(bacc[[8]])
r1 <- c(a=r1[2],b=r1[1],c=r1[3],d=r1[4])
bacc <- replace_text_by_options(bacc,r1,names(bacc)[8])
r2 <- unique(bacc[[9]])
r2 <- c(a=r2[3],b=r2[2],c=r2[4],d=r2[1])
bacc <- replace_text_by_options(bacc,r2,names(bacc)[9])
#seems question 3 changed replies through time
r3 <- unique(bacc[[10]])
r3 <- c(a1=r3[3],a2=r3[7],b1=r3[2],b2=r3[4],c1=r3[1],c2=r3[8],d=r3[9],e=r3[5],f=r3[6])
bacc <- replace_text_by_options(bacc,r3,names(bacc)[10])
r4 <- unique(bacc[[11]])
r4 <- c(a=r4[3],b=r4[2],c=r4[1])
bacc <-replace_text_by_options(bacc,r4,names(bacc)[11])
#question 4bis is multiple options
r4bis <- unique(bacc[[12]])
r4bis <- obtain_unique_from_multiple(r4bis)
r4bis <- c(a=r4bis[2],b=r4bis[3],c=r4bis[4],d=r4bis[1])
bacc <- replace_text_by_options(bacc,r4bis,names(bacc)[12])
#question 5 is multiple options
r5 <- unique(bacc[[13]])
r5 <- obtain_unique_from_multiple(r5)
r5 <- c(a1=r5[5],a2=r5[8],b1=r5[3],b2=r5[6],b3=r5[17],c1=r5[7],c2=r5[1],d=r5[4],e=r5[2])
bacc <- replace_text_by_options(bacc,r5,names(bacc)[13])

