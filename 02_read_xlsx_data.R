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
carnet <- unique(bacc[[5]])
carnet <- c(s=carnet[1],n=carnet[2])
bacc <- replace_text_by_options(bacc,carnet,names(bacc)[5])
#question 1 simple choice
r1 <-unique(bacc[[8]])
r1 <- c(a=r1[2],b=r1[1],c=r1[3],d=r1[4])
bacc <- replace_text_by_options(bacc,r1,names(bacc)[8])
#question 2 simple choice
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
#question 5 is multiple options, changed over time and allowed free text
r5 <- unique(bacc[[13]])
r5 <- obtain_unique_from_multiple(r5)
r5 <- c(a1=r5[5],a2=r5[8],b1=r5[3],b2=r5[6],b3=r5[17],c1=r5[7],c2=r5[1],d=r5[4],e=r5[2])
bacc <- replace_text_by_options(bacc,r5,names(bacc)[13])
#question 6 is simple options
r6 <- unique(bacc[[14]])
r6 <- obtain_unique_from_multiple(r6)
r6 <- c(a=r6[2],b=r6[6],c=r6[4],d=r6[1],e1=r6[3],e2=r6[5])
bacc <- replace_text_by_options(bacc,r6,names(bacc)[14])
#question 7 is simple options
r7 <- unique(bacc[[15]])
r7 <- obtain_unique_from_multiple(r7)
r7 <- c(a1=r7[5],a2=r7[3],b1=r7[2],b2=r7[4],c=r7[1],d=r7[6])
bacc <- replace_text_by_options(bacc,r7,names(bacc)[15])
#question 8 is multiple options
r8 <- unique(bacc[[16]])
r8 <- obtain_unique_from_multiple(r8)
r8 <- c(a=r8[3],b=r8[4],c=r8[5],d=r8[1],e1=r8[2],e2=r8[6])
bacc <- replace_text_by_options(bacc,r8,names(bacc)[16])
#question 9 (positiu/negatiu) are free texts
#question 10 is multiple choice
r10 <- unique(bacc[[19]])
r10 <- obtain_unique_from_multiple(r10)
r10 <- c(a1=r10[1],a2=r10[6],b=r10[2],c1=r10[3],c2=r10[10],
         d1=r10[4],d2=r10[11],e=r10[7],f=r10[9])
bacc <- replace_text_by_options(bacc,r10,names(bacc)[19])
#question 11 is multiple choice
r11 <- unique(bacc[[20]])
r11 <- obtain_unique_from_multiple(r11)
r11 <- c(a=r11[2],b1=r11[5],b2=r11[11],c2=r11[7],c3=r11[10],d=r11[6])
bacc <- replace_text_by_options(bacc,r11,names(bacc)[20])
#question 12 is simple choice
r12 <- unique(bacc[[21]])
r12 <- obtain_unique_from_multiple(r12)
r12 <- c(b=r12[3],c=r12[2],a=r12[1])
bacc <- replace_text_by_options(bacc,r12,names(bacc)[21])
#question 12b is multiple choice
r12b <- unique(bacc[[23]])
r12b <- obtain_unique_from_multiple(r12b)
r12b <- c(a1=r12b[1],a2=r12b[2],b=r12b[4],c=r12b[5],d=r12b[7],e1=r12b[6],e1=r12b[8])
bacc <- replace_text_by_options(bacc,r12b,names(bacc)[23])
#question 12a is multiple choice
r12a <- unique(bacc[[24]])
r12a <- obtain_unique_from_multiple(r12a)
r12a <- c(a=r12a[5],b=r12a[1],c=r12a[6],d=r12a[4],e=r12a[3],f=r12a[2])
bacc <- replace_text_by_options(bacc,r12a,names(bacc)[24])
#question about insurance is simple choice
insurance <- unique(bacc[[25]])
insurance <- obtain_unique_from_multiple(insurance)
insurance <- c(a=insurance[3],b=insurance[1],c=insurance[2])
bacc <- replace_text_by_options(bacc,insurance,names(bacc)[25])
