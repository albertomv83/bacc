#merge paper polls
a <- readRDS("registresA.RData")
b <- readRDS("registresB.RData")
#fix disparities
#question 4bis
#A  ->  B
#d  ->  c
#c  ->  NA
b[,`4bis-d`:=`4bis-c`]
b[,`4bis-c`:=NA]
#question 8
#A  ->  B
#c  ->  e
#d  ->  c
#e  ->  d
b[,extra:=`8e`]
b[,`8e`:=`8d`]
b[,`8d`:=`8c`]
b[,`8c`:=extra]
b[,extra:=NULL]
#question 12 bis 2
#A  ->  B
#b  ->  b
#b  ->  c
#c  ->  d
#d  ->  e
#e  ->  f
b[,`12bis2-b`:=ifelse(`12bis2-b`=='x' | `12bis2-c`=='x','x',NA)]
b[,`12bis2-c`:=`12bis2-d`]
b[,`12bis2-d`:=`12bis2-e`]
b[,`12bis2-e`:=`12bis2-f`]
b[,`12bis2-f`:=NA]
#question insurance
#A  ->  B
#b  ->  c
#c  ->  b
b[,extra:=segurb]
b[,segurb:=segurc]
b[,segurc:=extra]
b[,extra:=NULL]

#start merging
paper_polls <- rbind(a,b)
#noms
nom <- list()
paper_polls[,nom:=ifelse(is.na(nom),NA,
                  ifelse(is.na(cognom1),nom,
                  ifelse(is.na(cognom2),paste(nom,cognom1,sep=" "),
                          paste(nom,cognom1,cognom2,sep=" "))))]
nom$title <- question_titles[2]
nom$answers <- c(paper_polls[,nom],bacc[[2]])
#sexe
sexe <- list()
sexe$title <- question_titles[3]
sexe$answers <- factor(c(paper_polls[,sexe],sapply(tolower(bacc[[3]]),function(x) substr(x,1,1))))
#edat
edat <- list()
edat$title <- question_titles[4]
edat$answers <- as.numeric(c(paper_polls[,edat],bacc[[4]]))
#carnet
carnet <- list()
carnet$title <- question_titles[5]
carnet$answers <- factor(c(paper_polls[,carnet_conduir],bacc[[5]]))
bacc[[5]]



