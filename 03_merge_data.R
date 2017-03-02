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
sexe$options <- c(d="Dona",h="Home")
#edat
edat <- list()
edat$title <- question_titles[4]
edat$answers <- as.numeric(c(paper_polls[,edat],bacc[[4]]))
#carnet
carnet_conduir <- list()
carnet_conduir$title <- question_titles[5]
carnet_conduir$answers <- factor(c(paper_polls[,carnet_conduir],bacc[[5]]))
carnet_conduir$options <- c(s="Tinc carnet de conduir (B)",n="No tinc carnet")
#codi postal
codi_postal <- list()
codi_postal$title <- question_titles[6]
codi_postal$answers <- factor(c(paper_polls[,cp],str_pad(bacc[[6]], 5, pad = "0")))
#email
email <- list()
email$title <- question_titles[7]
email$answers <- c(paper_polls[,email],bacc[[7]])
#q1
q1 <- list()
q1$title <- question_titles[8]
q1$options <- r1
q1$answers <- c(join_paper_answers(paper_polls,'1',names(r1)),bacc[[8]])
#q2
q2 <- list()
q2$title <- question_titles[9]
q2$options <- r2
q2$answers <- c(join_paper_answers(paper_polls,'2',names(r2)),bacc[[9]])
#q3
q3 <- list()
q3$title <- question_titles[10]
r3mod <- r3[c('a1','b2','c2','d','e','f')]
names(r3mod) <- c("a","b","c","d","e","f")
q3$options <- r3mod
q3$answers <- c(join_paper_answers(paper_polls,'3',names(r3mod)),bacc[[10]])
#q4
q4 <- list()
q4$title <- question_titles[11]
q4$options <- r4
q4$answers <- c(join_paper_answers(paper_polls,'4',names(r4)),bacc[[11]])
q4$answers[q4$answers==""] <- NA
#q4bis
q4bis <- list()
q4bis$title <- question_titles[12]
q4bis$options <- r4bis
q4bis$answers <- c(join_paper_answers(paper_polls,'4bis-',names(r4bis)),bacc[[12]])
q4bis$answers[q4bis$answers==""] <- NA
q4bis$answers_exp <- strsplit(paste0(q4bis$answers[!is.na(q4bis$answers)],collapse=", "),", ",fixed=T)[[1]]
#q5
q5 <- list()
q5$title <- question_titles[13]
r5mod <- r5[c('a2','b3','c1','d','e')]
names(r5mod) <- c("a","b","c","d","e")
r5mod <- c(r5mod, f="Altres")
q5$options <- r5mod
p <- join_paper_answers(paper_polls,'5',names(r5mod))
p[nchar(p)>1]<-sapply(p[nchar(p)>1],function(x){paste0(strsplit(x,"")[[1]],collapse=", ")})
p[p==""]<-NA
online <- sapply(bacc[[13]],function(x){paste0(substitute_other_options(x,options=names(r5mod)),collapse=", ")})
q5$answers <- c(p,online)
q5$answers_exp <- strsplit(paste0(c(p,online),collapse=", "),", ",fixed=T)[[1]]
#q6
q6 <- list()
q6$title <- question_titles[14]
r6mod <- r6[c('a','b','c','d','e1')]
names(r6mod) <- c("a","b","c","d","e")
q6$options <- r6mod
q6$answers <- c(join_paper_answers(paper_polls,'6',names(r6mod)),bacc[[14]])
q6$answers[q6$answers==""] <- NA
#q7
q7 <- list()
q7$title <- question_titles[15]
r7mod <- r7[c('a1','b1','c','d')]
names(r7mod) <- c("a","b","c","d")
q7$options <- r7mod
q7$answers <- c(join_paper_answers(paper_polls,'7',names(r7mod)),bacc[[15]])
q7$answers[q7$answers==""] <- NA
#q8
q8 <- list()
q8$title <- question_titles[16]
r8mod <- r8[c('a','b','c','d','e1')]
names(r8mod) <- c("a","b","c","d","e")
r8mod <- c(r8mod, f="Altres")
q8$options <- r8mod
p <- join_paper_answers(paper_polls,'8',names(r8mod))
p[nchar(p)>1]<-sapply(p[nchar(p)>1],function(x){paste0(strsplit(x,"")[[1]],collapse=", ")})
p[p==""]<-NA
online <- sapply(bacc[[16]],function(x){paste0(substitute_other_options(x,options=names(r8mod)),collapse=", ")})
q8$answers <- c(p,online)
q8$answers_exp <- strsplit(paste0(c(p,online),collapse=", "),", ",fixed=T)[[1]]
#q10
q10 <- list()
q10$title <- question_titles[19]
r10mod <- r10[c('a2','b','c2','d2','e','f')]
names(r10mod) <- c("a","b","c","d","e","f")
r10mod <- c(r10mod, g="Altres")
q10$options <- r10mod
p <- join_paper_answers(paper_polls,'10',names(r10mod))
p[nchar(p)>1]<-sapply(p[nchar(p)>1],function(x){paste0(strsplit(x,"")[[1]],collapse=", ")})
p[p==""]<-NA
online <- sapply(bacc[[19]],function(x){paste0(substitute_other_options(x,options=names(r10mod)),collapse=", ")})
online[online=="NA"]<-NA
q10$answers <- c(p,online)
q10$answers_exp <- strsplit(paste0(c(p,online),collapse=", "),", ",fixed=T)[[1]]
#q11
q11 <- list()
q11$title <- question_titles[20]
r11mod <- r11[c('a','b2','c3','d')]
names(r11mod) <- c("a","b","c","d")
r11mod <- c(r11mod, e="Altres")
q11$options <- r11mod
p <- join_paper_answers(paper_polls,'11',names(r11mod))
p[nchar(p)>1]<-sapply(p[nchar(p)>1],function(x){paste0(strsplit(x,"")[[1]],collapse=", ")})
p[p==""]<-NA
online <- sapply(bacc[[20]],function(x){paste0(substitute_other_options(x,options=names(r11mod)),collapse=", ")})
online[online=="NA"]<-NA
q11$answers <- c(p,online)
q11$answers_exp <- strsplit(paste0(c(p,online),collapse=", "),", ",fixed=T)[[1]]
#q12
q12 <- list()
q12$title <- question_titles[21]
r12mod <- r12[c('a','b','c')]
names(r12mod) <- c("a","b","c")
q12$options <- r12mod
q12$answers <- c(join_paper_answers(paper_polls,'12',names(r12mod)),bacc[[21]])
q12$answers[q12$answers==""] <- NA
#q12bis1
q12bis1 <- list()
q12bis1$title <- question_titles[24]
q12bis1$options <- r12a
q12bis1$answers <- c(join_paper_answers(paper_polls,'12bis1-',names(r12a)),bacc[[24]])
q12bis1$answers[q12bis1$answers==""] <- NA
#leave only answers that relate to 12 option a
q12bis1$all_answers <- q12bis1$answers
q12bis1$answers <- q12bis1$answers[q12$answers=="a"]
#q12bis2
q12bis2 <- list()
q12bis2$title <- question_titles[23]
r12bmod <- r12b[c('a1','b','c','d','e1')]
names(r12bmod) <- c("a","b","c","d","e")
r12bmod <- c(r12bmod, f="Altres")
q12bis2$options <- r12bmod
p <- join_paper_answers(paper_polls,'12bis2-',names(r12bmod))
p[nchar(p)>1]<-sapply(p[nchar(p)>1],function(x){paste0(strsplit(x,"")[[1]],collapse=", ")})
p[p==""]<-NA
online <- sapply(bacc[[23]],function(x){paste0(substitute_other_options(x,options=names(r12bmod)),collapse=", ")})
online[online=="NA"]<-NA
q12bis2$answers <- c(p,online)
#leave only answers that relate to 12 option b
q12bis2$all_answers <- q12bis2$answers
q12bis2$answers <- q12bis2$answers[q12$answers=="b"]
q12bis2$answers_exp <- strsplit(paste0(q12bis2$answers,collapse=", "),", ",fixed=T)[[1]]


