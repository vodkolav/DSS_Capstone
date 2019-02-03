# Constructinng the prediction model

tokenize <- function(Lines, n=1)
{
  Encoding(Lines) <- "latin1"  #remove non-ascii chars 
  Lines <- iconv(Lines, "latin1", "ASCII", sub="")
  Lines <- gsub('_+', ' ', Lines, perl=T) #replace all underscores (including multiple _____) with whitespace
  Corp  <- corpus(char_tolower(Lines)) # everything to lowerCase and create corpus
  Corp  <- corpus_reshape(Corp, to = "sentences") # break corpus to single sentences, so that ngrams wont be created on the border of two sentences
  Toks  <- tokens(Corp, what = "word", remove_numbers = T, remove_punct = T, remove_symbols = T,
                  remove_separators = T, remove_twitter = T, remove_hyphens = T, remove_url = T)
  rm(Lines,Corp)              #stopwords("english"),
  Toks <- tokens_remove(Toks, c( profane, "rt")) # also remove ReTweet tag
  if(n>1){return(tokens_ngrams(Toks, n))}else{return(Toks)}
}

constructQuery <- function( search = c('said', 'first', 'quarter' ,'profit'), ngrO = 5, lim = 0)
{
  
  if(length(search)+1<ngrO) {warning("Warning! length of search can't be lower than ngrO-1 \n")}
  ngrs    <- 1:ngrO
  
  #select
  #pselect <-paste(paste("w", ngrs,".word as wd", ngrs, sep = "", collapse = ", "), ", gram", ngrO, ".freq", sep = "")
  pselect <-paste(paste("w", ngrO,".word as wd", ngrO, sep = "", collapse = ", "), ", gram", ngrO, ".freq", sep = "")
  #from
  pfrom   <-paste("gram", ngrO, sep = "")
  pjoin   <-paste("join words w", ngrs, " on (w", ngrs, ".id == gram",ngrO,".word", ngrs,")",sep = "",collapse = "\n" )
  #"where"
  psearch <- paste("\"", search, "\"", sep = "")
  pwhere  <-paste("w",seq_along(psearch), ".word = ", psearch, sep = "", collapse = " and ")
  plim    <- if(lim){paste("limit", lim)}else{""} 
  
  q <- paste("select", pselect,"from", pfrom, pjoin, "where", pwhere,"order by freq desc", plim , sep = "\n")
 # TODO: limit n with max freq
  return(q)
}

#writeLines( constructQuery())

vocabCheck <- function(y)
{
  #Check input against vocabulary and drop all the words that are not in it.
  #Also, take only 4 last words, since the model is limited to 5-grams.
  wat <- paste( "\"", y, "\"", sep="", collapse =",")
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
  x<-tokenize(x)[[1]]
  # x <- gsub('\'', '\'\'', x[[1]], perl=T) 
  searchTerm <-vocabCheck(x)
  if (identical(searchTerm,character(0))){
    return(mostFreqWords(n))}
  
  ngrO <- length(searchTerm)+1
  alp <-.4
  SS <- data.table(pred = character(),score = integer(), ngram = numeric())
  for(i in ngrO:2)
  {
    wi <- sqldf(constructQuery(search = searchTerm, i,n),dbname =dbname)
    if(nrow(wi)<1){
      #just move on
    }else{
      wi_1 <- sqldf(constructQuery(search = searchTerm, i-1,n),dbname =dbname)
      num<- paste("wd",i,sep="")
      S<-data.table(pred = wi[[num]],score =(wi$freq/wi_1$freq)*alp^(ngrO-i), ngram = rep(i,nrow(wi)))
      SS<-rbind(SS,S)
      if(nrow(SS)>n)
      {
        break
      }
    }
    searchTerm <-searchTerm[-1] #tail(searchTerm,i-1)
  }
  #The same predictions from different ngrams may end up in S. So largest score between them is selected. 
  #Also, ordered by descending score.
  #SSS <<- sqldf("select pred, max(score)as score, max(ngram) as ngram  from S group by pred order by score desc")

  if(nrow(SS)>0){SS <- SS[order(-score),.(score = max(score)), by=.(pred)]}
  #SSS <<- S[.(scr),.(scr = max(score)),by=.(pred)]

  sscore <- c(SS$score[-1],0)
  # if(any(!(SS$score >= sscore))) {
  #   stop("Error! SS is not sorted! ")}

  ret <- SS[1:n,pred]
    if(any(is.na(ret)))
  {
      replc<-mostFreqWords(n)
      ret[is.na(ret)]<-replc[is.na(ret)]
  }
  
  return(ret)
  
}


predict.dummy <- function(x,n=1){
  opts <- c("horticultural", "spiritual", "marital", "financial",
            "decade",  "morning", "month", "weekend",   
            "stress", "hunger", "happiness", "sleepiness")
  return(sample(opts,n))
}



# x <- "I like how the same people are in almost all of Adam Sandler's" 
# system.time({ a <- predict.word(x,10)})
# print(a)

# x <- "said first"
#profvis({predict.word(x)})
 
# predict.word("said")
# wat <- character(0)
#predict.word(c('rt','hello'))
#writeLines( constructQuery(search = character(0), 1-1))
