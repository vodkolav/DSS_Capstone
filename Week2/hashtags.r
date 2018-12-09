# My attempt to create Twitter Hashtags segmentation algorithm (work in process)

# Principle: find increasingly longer subsets that get recognised by hunspell


check <- "10thingsaboutme"


#Example:
#      subst           fits   
#  [1,] ""              "FALSE"
#  [2,] "th"            "FALSE"
#  [3,] "thi"           "FALSE"
#  [4,] "thin"          "TRUE" 
#  [5,] "thing"         "TRUE" 
#  [6,] "things"        "TRUE"    <- take this
#  [7,] "thingsa"       "FALSE"
#  [8,] "thingsab"      "FALSE"
#  [9,] "thingsabo"     "FALSE"
# [10,] "thingsabou"    "FALSE"
# [11,] "thingsabout"   "FALSE"
# [12,] "thingsaboutm"  "FALSE"
# [13,] "thingsaboutme" "FALSE"



# check <- "Austinlifestylemagazine"
# subst = character(nchar(check))
# fits = logical(nchar(check))
# flag = F
# from = 1
# words = ""
# for (i in 2:nchar(check))
# {
#   #print(substr(check,1,i))
#   subst[i] <- substr(check,from,i)
#   this <- hunspell_check(subst[i], dict = dictionary("en_US"))
#   if(!this & flag)
#   {
#     words <- c(words, subst[i])
#     flag = F
#   }
#   fits[i] <-this
# }
# print(cbind(subst, fits))
# print(words)




capitalize <- function(word)
{
  substr(word,1,1) <- toupper(substr(word,1,1))
  return(word)
}

longestword <- function(check)
{
  fits = logical(nchar(check))
  for (i in 2:nchar(check))
  {
    #print(substr(check,1,i))
    subst <-capitalize(substr(check,from,i)) # need to capitalize because hunspell regards names 
                                             # that do not start with capital letter a mistake
    fits[i] <- hunspell_check(subst, dict = dictionary("en_US"))
  }
  end = max(which(fits))
  if(end == -Inf)
    end = nchar(check)
  return(end)
}

hashtagSeg <- function(check)
{
  from = 1
  if (substr(check,1,1)=="#") 
    check <- substr(check,2,nchar(check))
  to = nchar(check)
  words = character(0)
  lw = 0
  while(check != "")
  {
    lw <- longestword(check)
    words<- c(words, substr(check,1,lw))
    check <- substr(check,lw+1,to)    
    #from =  lw + 1    
    #print(from)
  }
  return(words)
}

#longestword("dfjsdfg")
#longestword("Austinlifestylemagazine")
#wds <- hashtagSeg("Austinlifestylemagazine")
#hashtagSeg("10thingsaboutme")


# i = 18
longestword(tag)
hashtagSeg(tag)
nchar(tag)
capitalize("hello")

#segs = lapply(htags[1:100], FUN = hashtagSeg)

  
  