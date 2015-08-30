library(tm)
library(Rstem)

#-------------------------------------------------------------------------------------------------------------

#Setting up Twitter Authorization and scrapping tweets
setup_twitteROAuth( "XXXXXXXXXX", "XXXXXXXXXXX", "XXXXXXXXXXX","xXXXXXXXXX")

some_tweets1=searchTwitter("gearsystems", n=1,lang="en")
some_text1=sapply(some_tweets1, function(x) x$getText())

#-------------------------------------------------------------------------------------------------------------

#Cleaning up the Tweets

tweet_cleanup(some_text1)

#-------------------------------------------------------------------------------------------------------------

#Classifying tweets into given categories

class_pol= classify_polarity(some_text1, algorithm="bayes", verbose='FALSE'
#getting the polarity best fit
polarity=class_pol[,6]
screenname=class_pol[,7]

#creating a data frame
sent_df=data.frame(complaint_id="NONE", complaint_type=polarity, complaint_desc=some_text1, timestamp="NONE", complaint_resolved_status="NONE", location="NONE", username=screenname,contact_no="NONE" )

#viewing the top levels of the data frame 
head(sent_df, n=12)

#-------------------------------------------------------------------------------------------------------------

#Creating an output .csv file
filename<-"CategoryTweets.csv"
write.csv(sent_df, file= filename)


#-------------------------------------------------------------------------------------------------------------

# R-Complaint-Classifier
