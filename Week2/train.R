#load metadata about files: specifically number of lines is what I need
load(paste(data.dir,'filesInfo.rData',sep = "/")) 

#load profanity vocabulary and convert to lower
profane <-paste(data.dir,'profanity_words.txt', sep="/")
l <- determine_nlines(profane) 
profane <-char_tolower(get_lines(profane, 1:l))
save(profane, file = "../SlothKey/profane.rda")
list.files()
sample_text <- function(filename ,line_numbers)
{
  if (is.na(get_lines(filename,1:2)[1])) # check that I can read from file
  {
    stop(paste("can't read from file", filename))
  }
  Lines <- get_lines(filename, line_numbers) #sample some lines from a file
  return(Lines) #(cS)# 
}  


Freqs <-function (Toks)
{
  DfmNgr <- dfm(Toks, tolower = F)
  cS<-sort(colSums(DfmNgr), decreasing = T)
  FreqThis <- data.table(word = names(cS), freq = cS , stringsAsFactors = F, key = "word")
  return(FreqThis)
}

DBinsert <- function(FreqThis, tables, n)
{
  
  wordCols <- paste("word", 1:n, sep = '')
  #Split column word="hello_how_are_you" into columns word1="hello" word2="how" word3="are" word4="you"
  FreqThis <- FreqThis %>% separate(col=word, sep = "_", into = wordCols ,remove =T)

  #Find Ids for all word1, word2, etc in vocabulary and replace actual words with them
  for (i in 1:n) 
  {
    wordi <- sqldf(paste("select Id, word from words, FreqThis where (words.word == FreqThis.word",i, ")", 
                         sep = ""), dbname =dbname)
    if(nrow(FreqThis)!=nrow(wordi)){stop("Error! most likely failed to read profanity dictionary")}
    FreqThis[,(wordCols[i]) := wordi$Id]
    # FreqThis[wordCols[i]] <- wordi$Id
  }
  
  pcols <- paste(wordCols, collapse = ", ")
  
  #There are two tables in action: gram<n> and tmp. The data is moved alternatingly from one to another
  #At every iteration of train function new data is added to the bottom of "big" table 
  #then grouped by word1..wordn and each grop's freq summed 
  #and then the whole table is moved to "small" table while simultaneously being merged by selecting and aggregating  
  #At next iteration the same happens in reverse direction.
  ques <- c(paste("insert into", tables$big ,"select * from FreqThis"),
            paste("insert into", tables$small ,"(",pcols,", freq) ",
                  "select",pcols,", sum(freq) as freq",
                  "from", tables$big ,
                  "group by",pcols),
            paste("delete from", tables$big),
            paste("select count(word1) from ", tables$small))
  
  suppressWarnings(cnt<- sqldf(ques, dbname = dbname)[[1]])
  #obs <- c(obs,cnt) # save number of unique ngrams for the performance report
  print(paste(cnt,"Total Ngrams in" ,tables$small))
  return(list(swap(tables),cnt))
}

swap <- function(table)
{
  if (length(table)!=2) stop("table's length must be precisely 2!")
  tmp <- table[1]
  table[1] <- table[2]
  table[2] <- tmp
  return(table)
}

slices <- function(files,sampSize)
{
  #[currFile,]
  files <- files[!(files$file == "Total"),] #drop the last line (Total)
  return(floor(files$lines / sampSize)) #upper boundary for processing loop
}

train.word <- function(path, sampSize = 5e3, ngrO = 1, mode = "test" )
{
  if (mode=="test"){print("running in testing mode...")}
  print(paste("Database file ", dbname) )
  tic <- Sys.time()
  obs <- numeric()
  elapsed <- numeric()
  
  sampSize <- ifelse(mode=="test", 1e2, sampSize )
  
  # Total number of slices to process, depending on sample size
  totslices <- sum(slices(files, sampSize)) 
    
  # vector of colunms names, ex. word1, word2, etc
  wordCols <- paste("word", 1:ngrO, sep = '') 
  
  # name of table currently being created, ex. gram2
  gramN <- paste("gram", ngrO, sep = "") 
  
  # a pair of tables names in working: ex. gram4 and tmp, which will be alternately filled with newer data from one to another
  tables <- if(totslices%%2==0) {list(big = "tmp", small = gramN)} else {list(big = gramN, small = "tmp")}
  
  # substring of the create query that defines columns, e.g. word1 INT, word2 INT, word3 INT, word4 INT
  pcols <- paste(paste(wordCols, "INT"),  collapse = ", ")  
  
  # Creates gramN and temporary table , if not exists,  ex. 
  # CREATE TABLE IF NOT EXISTS  tmp   ( word1 INT, word2 INT, word3 INT, word4 INT , freq REAL)"  
  # CREATE TABLE IF NOT EXISTS  gram4 ( word1 INT, word2 INT, word3 INT, word4 INT , freq REAL)"
  # And clears them in case they already existed and had some data in them
  setupQrs <- c(paste("CREATE TABLE IF NOT EXISTS ", tables, "(", pcols, ", freq REAL)" ),
                  paste("delete from ", tables))
  
  suppressWarnings(sqldf(setupQrs, dbname = dbname))
  
  for (currFile in files[-nrow(files),]$file)
  {
    #Derived parameters
    filename <- paste(path,  currFile, sep = '/')
    nslices <- slices(files[currFile,], sampSize)
    rndslices <- ceiling(runif(1,0,nslices)) # select random line from data for testing
    rng <- if(mode=="test") (rndslices-2):rndslices else 0:nslices 
    
    for (i  in rng) 
    {
      lnums <- (i*sampSize+1):((i+1)*sampSize)
      toc <-difftime(Sys.time(), tic, units = "mins")
      toc <- paste("| ", sprintf("%.1f", toc ), " ", attr(toc, "units"), "elapsed")
      print(paste("processing file ", currFile , " lines ", lnums[1], " - ", tail(lnums,1),toc))
      
      # The actual processig steps
      Lines   <- sample_text(filename, lnums) 
      Toks    <- tokenize(Lines, ngrO)
      FreqNgr <- Freqs(Toks)
      rm(Toks,Lines)
      res <- DBinsert(FreqNgr, tables, ngrO)
      tables <- res[[1]]
      
      obs <- c(obs,res[[2]])
      elapsed <- c(elapsed, difftime(Sys.time(), tic, units = "mins"))
    }
  }
  suppressWarnings(sqldf("drop table tmp",dbname = dbname))
  
#plot(elapsed[-1], obs, type = 'l')
return(data.frame(elapsed = unclass(elapsed), objects =obs, ngrO = rep(ngrO, length(obs))))
}











