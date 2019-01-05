setwd('~/Studies/Coursera/10-Capstone/corpus/en_US/')

l <- determine_nlines('profanity_words.txt') #read profanity vocabulary and convert to lower
profane <-char_tolower(get_lines('profanity_words.txt', 1:l))
profane <<- profane[!is.na(profane)]


load(file = 'filesInfo.rData') #load information about files: specifically number of lines 

sample_and_toks <- function(filename ,line_numbers)
{
  
  if (is.na(get_lines(filename,1:2)[1])) # check that I can read from file
  {
    print(paste("can't read from file", filename))
    stop()
  }
  
  Lines <- get_lines(filename, line_numbers) #sample some lines from a file
  Encoding(Lines) <- "latin1"  #remove non-ascii chars 
  Lines <- iconv(Lines, "latin1", "ASCII", sub="")
  Lines <- gsub('_+', ' ', Lines, perl=T) #replace all underscores (including multiple _____) with whitespace
  Corp <- corpus(char_tolower(Lines)) # everything to lowerCase and create corpus
  
  Toks<- tokens(Corp, what = "word", remove_numbers = T, remove_punct = T, remove_symbols = T,
                remove_separators = T, remove_twitter = T, remove_hyphens = T, remove_url = T)
  rm(Lines,Corp)
  Toks <- tokens_remove(Toks, c(stopwords("english"), profane, "rt"))# also remove ReTweet tag
  
  return(Toks) #(cS)# 
}  

ngramize <-function (n, Toks, FreqBig)
{
  library(quanteda)
  library(data.table)
  library(dplyr)
  ngrams <- tokens_ngrams(Toks, n)
  DfmNgr <- dfm(ngrams, tolower = F)
  cS<-sort(colSums(DfmNgr), decreasing = T)
  FreqThis <- data.table(word = names(cS), freq = cS , stringsAsFactors = F, key = "word")
  #FreqThis <- FreqNgr1$gr2
  
  #separate single column word = "hello_my_dear_friend" ...
  #into n columns:  |word1 = "hello"| word2 = "my"| word3 = "dear"| word4 = "friend"|
  wordCols <- paste("word", 1:n, sep = '')
  FreqThis <- FreqThis %>% separate(col=word, sep = "_", into = wordCols ,remove =T)
  
  for (i in 1:n) #replace words in FreqThis with their Ids from an index of unique words.
  {
    wordi <- sqldf(paste("select Id, word from words, FreqThis where (words.word == FreqThis.word",i, ")", 
                         sep = ""), dbname ="ngrams")
    FreqThis[,(wordCols[i]) := wordi$Id]
  }

  attr(FreqThis,"table") <- paste("gram",n,sep = '')
  #merge new calculated ngrams into database
  merge_gram(FreqThis)
  
  
  #FreqBig<-merge(FreqBig[[paste("gr",n,sep = '')]],FreqThis, by.x = wordCols, by.y = wordCols, all =T, sort = T)
  # FreqBig<-merge(FreqBig,FreqThis, by.x = wordCols, by.y = wordCols, all =T, sort = T)
  # FreqBig[is.na(FreqBig)] <-0 #probably should do it with only numeric columns for speed
  # FreqBig <- mutate(FreqBig, freq = freq.x + freq.y)
  # 
  # return(select(FreqBig, -c("freq.x", "freq.y")))
}


# updateTable <- function(grB)
# {
#   sqldf("create table gram2 as select * from grA", dbname = "ngrams")

# }
# updateTable(grB)

selectStr <- function(keys) # build part of SQL where statement from data, eg: 'word1 = 4 and word2 = 734'
{
  wat <- paste(names(keys), "=" , keys)
  wat <- paste(wat, collapse = ' and ')
  return(wat)
}

merge_gram <- function(grB)
{
  table <- attr(grB,"table") # grB must arrive with attribute table set to ngram's table name, e.g gram2 / gram3, etc.
  for (i in 1:nrow(grB))
  {
    inc <- grB[i,]
    incKeys  <- select(inc,-c("freq"))
    existing <- sqldf(paste("select * from ",table," where ", selectStr(incKeys)), dbname = "ngrams")
    if (nrow(existing)==0)
    {
      sqldf(paste("insert into ",table," (", paste(names(inc), collapse = ","), ") values (",
                  paste(inc, collapse = ","),")"), dbname = "ngrams")
    }else
    {
      sqldf(paste("update ",table," set  freq = ", existing$freq + inc$freq , " where ", selectStr(incKeys)) , dbname = "ngrams")
    }
  }
}

#attr(grB,"order")





#prof = NULL
#prof <- profvis({

#User Parameters:
path <- "/home/michael/Studies/Coursera/10-Capstone/corpus/en_US/"
#currFile <- 'en_US.blogs.txt'
sampSize = 5e4  #1e4 # how much lines to sample in a chunk
ngrO = 1 #:5 # what Orders of ngrams you require

ngrams <- function(path, sampSize = 5e4, ngrO = 1, env = "test" )
{
  tic <- Sys.time()
  obs <- numeric()
  elapsed <- Sys.time()
  
  #A list of data.tables each of which holds ngrams of its order
  # a <- list(data.table(word = numeric(1:3), freq = numeric(), stringsAsFactors = F, key = "word"))
  # FreqNgr <- rep(a, length(ngrO))
  
  wordCols <- paste("word", 1:ngrO, sep = '')
  a <- data.table(freq = numeric()) 
  a[,(wordCols) := numeric(ngrO), by=freq]
  # FreqNgr <- list(a) # for compatibility I'll leave it as list of data.tables, but only one d.t will be there
  # names(FreqNgr) <- paste("gr",ngrO,sep = '') #stamp a names such as 'gr2', 'gr3' etc to each data.table in a list 
  #FreqNgr <- a
  
  
  #TODO: create table in database if doesnt exist
  
  
  for (currFile in files[-4,]$file)
  {
    #Derived parameters
    filename <- paste(path,  currFile, sep = '')
    
    sampSize <- ifelse(env=="test", 1e2, sampSize )
    nslices = floor(files[currFile,]$lines / sampSize) #upper boundary for processing loop
    #rng = (nslices-1):nslices # 10 # for quick tests 
    #rng = 0:nslices # 10 # for full file run. must start at zero
    
    rndslices <- ceiling(runif(1,0,nslices)) # smaple random place in data for testing
    rng <- if(env=="test") (rndslices-1):rndslices else 0:nslices 
    
    
    #cl <- makeSOCKcluster(rep("localhost", length(ngO)))
    
    for (i  in rng) # 900)#
    {
      lnums <- (i*sampSize+1):((i+1)*sampSize)
      
      toc <-difftime(Sys.time(), tic, units = "mins")
      toc <- paste("| ", sprintf("%.1f", toc ), " ", attr(toc, "units"), "elapsed")
      #56 is lenth of path
      print(paste("processing file ", substr(filename,56,nchar(filename)), "  lines ", 
                  lnums[1], " - ", tail(lnums,1),toc))
      
      Toks <- sample_and_toks(filename, lnums)
      ngramize(ngrO, Toks, FreqNgr)
      
      #FreqNgr <- parLapply(cl, ngO, ngramize, Toks, FreqNgr)
      # 
      # ob <-dim(FreqNgr)[1]
      # obs <- c(obs, ob)
      # print(paste(ob, "unique tokens| ", object.size(FreqNgr), " bytes"))
      elapsed <- c(elapsed, difftime(Sys.time(), tic, units = "mins"))
    }
    #stopCluster(cl)
    #}, prof_output = ".")
    
    
    #Freq1grBlogs <- FreqNgr
    #
    #FreqNgr["gr2"]
    #profvis(prof_input = "/home/michael/Studies/Coursera/10-Capstone/corpus/en_US/file4fce8ccb11c.Rprof")
    
  }
  #save(FreqNgr,obs, file = paste(path, ngrO, "grams." ,env, ".allFiles.rData", sep = ''))
  #return(list(FreqNgr,obs))
}

# tmp <- ngrams(path, sampSize = 5e2, ngrO = 2, env = "test" )
# FreqNgr <- tmp[[1]]
# obs <- tmp[[2]]
# plot(obs, type = 'l')

