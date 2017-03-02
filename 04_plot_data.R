plot_answer <- function(answer){
  if(!is.null(answer$answers_exp)){
    #is multiple choice
    data <- data.table(table(answer$answers_exp,exclude = NULL))
  }else{
    data <- data.table(table(answer$answers,exclude = NULL))
  }
  names(data)<-c("Resposta","Total")
  data[is.na(Resposta),Resposta:="NS/NC"]
  data[Resposta=="NA",Resposta:="NS/NC"]
  data <- data[,.(Total=sum(Total)),by=.(Resposta)]
  bp <- ggplot(data)
  bp <- bp + geom_bar(aes(x=Resposta,y=Total,fill=Resposta),stat="identity")
  bp <- bp + geom_text(aes(label = Total,x=Resposta, y = Total), stat= "identity", vjust = -.5)
  if(!is.null(answer$options)){
    labels <- sapply(answer$options,function(x){ifelse(nchar(x)>100,substr(x,1,100),x)})
    bp <- bp + scale_fill_brewer(type="qua", palette="Set2", labels = labels)
  }else{
    bp <- bp + scale_fill_brewer(type="qua",  palette="Set2", guide=FALSE)
  }
  bp <- bp + ggtitle(answer$title) + theme_classic()+ theme(legend.position = "top", 
                                                            legend.text = element_text(size = 8))
  bp
}

qlist <- list()
qlist$sexe <- sexe
qlist$carnet_conduir <- carnet_conduir
qlist$q1 <- q1
qlist$q2 <- q2
qlist$q3 <- q3
qlist$q4 <- q4
qlist$q5 <- q5
qlist$q6 <- q6
qlist$q7 <- q7
qlist$q8 <- q8
qlist$q10 <- q10
qlist$q11 <- q11
qlist$q12 <- q12
qlist$q12bis1 <- q12bis1
qlist$q12bis2 <- q12bis2

for (q in names(qlist)){
  p <- plot_answer(qlist[[q]])
  ggsave(paste0(q,".png"),p, width=400,height=210,units="mm")
}