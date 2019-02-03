
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

