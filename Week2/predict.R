# Constructinng the prediction model

library(stringi)

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

mostFreqWords <- function(n = 3)
{
  sqldf(paste("select word from words order by Id limit" ,n ), dbname = dbname)$word
}


predict.word <-function(x, n = 3)
{
  
  if (stri_length(x)==0){
    # return n most common words
    return(mostFreqWords(n))
    }
  x<-tokenize(x)
  x <- gsub('\'', '\'\'', x[[1]], perl=T) 
  searchTerm <-vocabCheck(x)
  if (identical(searchTerm,character(0))){
    return(mostFreqWords(n))}
  
  ngrO <- length(searchTerm)+1
  alp <-.4
  S <- data.table(pred = character(),score = integer(), ngram = numeric() ,stringsAsFactors = F)
  for(i in ngrO:2)
  {
    wi <- sqldf(constructQuery(search = searchTerm, i),dbname =dbname)
    if(nrow(wi)<1){
      #just move on
    }else{
      wi_1 <- sqldf(constructQuery(search = searchTerm, i-1),dbname =dbname)
      num<- paste("wd",i,sep="")
      s<-data.table(pred = wi[[num]],score =(wi$freq/wi_1$freq*alp^(ngrO-i)), ngram = rep(i,nrow(wi)),stringsAsFactors = F)
      S<-rbind(S,s)
    }
    searchTerm <-searchTerm[-1] #tail(searchTerm,i-1)
  }
  #The same predictions from different ngrams may end up in S. So largest score between them is selected. 
  #Also, ordered by descending score.
  #SSS <<- sqldf("select pred, max(score)as score, max(ngram) as ngram  from S group by pred order by score desc")
  SSS <<- S[order(-score),.(score = max(score)),by=.(pred)]
  #SSS <<- S[.(scr),.(scr = max(score)),by=.(pred)]


  ret <- SSS[1:n,pred]
    if(any(is.na(ret)))
  {
      replc<-mostFreqWords(n)
      ret[is.na(ret)]<-replc[is.na(ret)]
  }
  
  return(ret)
  
}


# x <- "said first quarter profit"
# x <- "said first"
#profvis({predict.word(x)})
 
# predict.word("said")
# wat <- character(0)
#predict.word(c('rt','hello'))
#writeLines( constructQuery(search = character(0), 1-1))
