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
    bp <- bp + scale_fill_discrete(labels = labels)
  }else{
    bp <- bp + scale_fill_discrete(guide=FALSE)
  }
  bp <- bp + ggtitle(answer$title) + theme_classic()
  bp
}