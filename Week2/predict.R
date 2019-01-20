# Constructinng the prediction model

constructQuery <- function( search = c('said', 'first', 'quarter' ,'profit'), ngrO = 5)
{
  
  if(length(search)+1<ngrO) {warning("Warning! length of search can't be lower than ngrO-1 \n")}
  ngrs    <- 1:ngrO
  
  #select
  #pselect <-paste(paste("w", ngrs,".word as wd", ngrs, sep = "", collapse = ", "), ", gram", ngrO, ".freq", sep = "")
  pselect <-paste(paste("w", ngrO,".word as wd", ngrO, sep = "", collapse = ", "), ", gram", ngrO, ".freq", sep = "")
  pfrom   <-paste("from gram", ngrO, sep = "")
  pjoin   <-paste("join words w", ngrs, " on (w", ngrs, ".id == gram",ngrO,".word", ngrs,")",sep = "",collapse = "\n" )
  #"where"
  psearch = paste("'", search, "'", sep = "")
  pwhere  <-paste("w",seq_along(psearch), ".word = ", psearch, sep = "", collapse = " and ")
  
  q <- paste("select", pselect, pfrom, pjoin, "where", pwhere, sep = "\n")
  
  return(q)
}


vocabCheck <- function(y)
{
  #Check input against vocabulary and drop all the words that are not in it.
  #Also, take only 4 last words, since the model is limited to 5-grams.
  wat <- paste( "'", y, "'", sep="", collapse =",")
  ans <- sqldf(paste("select word from words where word in (",wat,")"),dbname = dbname )
  #tryCatch(, error = browser())
  ans <- ans$word
  y <- y[y %in% ans]
  
  # if(length(y)>=5)
  # {
  #   print('oops')
  # }
  return(tail(y,4))
}


predict.word <-function(x)
{
  
  if (stri_length(x)==0){
    return(c('the', 'on', 'a'))
    }
  x<-tokenize(x)
  x <- gsub('\'', '\'\'', x[[1]], perl=T) 
  searchTerm <-vocabCheck(x)
  if (identical(searchTerm,character(0))){return(c('they', 'are', 'here'))}
  #searchTerm <- c('said', 'first', 'quarter' ,'profit')
  

  
  ngrO <- length(searchTerm)+1
  alp <-.4
  S <- data.frame(pred = character(),score = integer(),stringsAsFactors = F)
  for(i in ngrO:2)
  {
    wi <- sqldf(constructQuery(search = searchTerm, i),dbname =dbname)
    if(nrow(wi)<1){
      #just move on
      #return(c('i', 'am', 'fat'))
    }else{
      wi_1 <- sqldf(constructQuery(search = searchTerm, i-1),dbname =dbname)
      num<- paste("wd",i,sep="")
      s<-data.frame(pred = wi[[num]],score =(alp^(ngrO-i))*  wi$freq/wi_1$freq,stringsAsFactors = F)
      S<-rbind(S,s)
    }
    searchTerm <-searchTerm[-1] #tail(searchTerm,i-1)
  }
  
  S<-S[order(S$score,decreasing = T),]
  #})

  ret <- S[1:3,"pred"]
    if(any(is.na(ret)))
  {
      repl<-c("i","am","fat")
      ret[is.na(ret)]<-repl[is.na(ret)]
  }
  
  return(ret)
  
}


x <- "said first quarter profit"
x <- "said first"
#profvis({predict.word(x)})
 
# predict.word("said")
# wat <- character(0)
#predict.word(c('rt','hello'))
#writeLines( constructQuery(search = character(0), 1-1))
