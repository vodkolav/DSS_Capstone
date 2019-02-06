
library(quanteda)
library(LaF)
library(data.table)
library(sqldf)
library(shiny)
library(stringi)

vu <- function(x){View(x)} 

# useful for concatenating parts of filepath
`%/%` = function(e1,e2) return(paste(e1,e2, sep = "/"))

#data.dir <- "/media/michael/Data"
data.dir <- "."

# db which has:1 to 5 grams, stopwords remain , freq=1 filtered out, indices created, 746 MB
dbname <- data.dir %/% "ngrams"

print(paste("ngrams database file: ",dbname))

load('profane.rda')
source('predict.R')

# Calculate total words in database and put them in newly created table metadata. 
# Used in the terminal case of the algorithm to calculate score of 1grams. pre-computed for speed.
# Doing it here to avoid re-uploading 750 Mb of updated databse to shinyapps.
a <- sqldf("SELECT name FROM sqlite_master WHERE type=\'table\' AND name=\'metadata\'", dbname = dbname)
if(nrow(a)==0)
{
  q <- c("CREATE TABLE \"metadata\" (`Name`	Varchar,`Val`	NUMERIC)",
         "insert into metadata  ( Name ,  Val)values (\'total_words\' , (select sum(freq)  from gram1))")
  sqldf(q, dbname = dbname)
}