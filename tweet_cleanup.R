
#-------------------------------------------------------------------------------------------------------------

tweet_cleanup <- function(some_text1){
  
  #Process to prepare the tweet for sentiment analysis
  
  #remove retwweeted entities
  some_text1=gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_text1)
  #remove @people
  some_text1=gsub("A\\w+", "", some_text1)
  #removing punctuation
  some_text1= gsub("[[:punct:]]","", some_text1) 
  #remove numbers
  some_text1=gsub("[[:digit:]]","",some_text1)
  #remove html links
  some_text1=gsub("http\\w+", "", some_text1)
  #remove unnecessary spaces
  some_text1=gsub("[ \t]{2,}", "", some_text1)
  some_text1=gsub("^\\s+|\\s+$", "", some_text1)
  
  #defining "tolower error handling" functions
  try.error=function(x)
  {
    #create missing value
    y=NA
    # tryCatch error
    try_error=tryCatch(tolower(x), error=function(e) e)
    #if not an error
    if(!inherits(try_error, "error"))
      y=tolower(x)
    return(y)
  }
  #lower case using try.error 
  some_text1=sapply(some_text1, try.error)
  
  #removing NAs in some text
  some_text1= some_text1[!is.na(some_text1)]
  names(some_text1)=NULL
  
  return (some_text1)
}

#-------------------------------------------------------------------------------------------------------------


# R-Complaint-Classifier
