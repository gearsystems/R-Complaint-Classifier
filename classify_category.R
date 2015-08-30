
#-------------------------------------------------------------------------------------------------------------

#Classification Algorithim on Bayesian Methods

classify_category <- function(textColumns,algorithm="bayes",pstrong=1.0,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv(file="data/CategoryDictionary.csv",header=FALSE) #dictionary
  
  counts <- list(roads=length(which(lexicon[,3]=="roads")),electricity=length(which(lexicon[,3]=="electricity")),water=length(which(lexicon[,3]=="water")),traffic=length(which(lexicon[,3]=="traffic")),total=nrow(lexicon))  #listing the dictionary words
  documents <- c()
  
  for (i in 1:nrow(matrix)) {  
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(water=0,electricity=0, roads=0, traffic=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- match(word,lexicon[,1], nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        scores[[category]] <- scores[[category]]+score
      }    
    }
    
    if (algorithm=="bayes") {   #The magic happens here 
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {  #when you don't want the magic to happen
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]  #assigning final score
    ratio <- abs(scores$roads/scores$electricity/scores$water/scores$traffic)
    if(isTRUE(all.equal(ratio,0.495830354533587)))best_fit <-"miscellaneous" # score value of all miscellaneous predictions
    tweets.df=ldply(some_tweets1[i], function(t) t$toDataFrame())
    screenname<-tweets.df$screenName
    documents <- rbind(documents,c(scores$roads,scores$electricity,scores$water,scores$traffic,abs(scores$roads/scores$electricity/scores$water/scores$traffic),best_fit, screenname)) #consolidating and assigning scores
    if (verbose) {
      print(paste("ROADS:",scores$water,"ELECTRICITY:",scores$electricity,"WATER",scores$water,"TRAFFIC:",scores$traffic))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("ROADS","ELECTRICITY","WATER","TRAFFIC","RATIO","BEST_FIT","SCREENNAME")
  return(documents)
  return()
}

#-------------------------------------------------------------------------------------------------------------

# R-Complaint-Classifier
