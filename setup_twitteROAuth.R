library(twitteR)


setup_twitteROAuth <- function( key, secret, token, token_secret ) {
  
#Process to register ROAuth for a session
#-------------------------------------------------------------------------------------------------------------
api_key <- key

api_secret <- secret

access_token <- token

access_token_secret <- token_secret

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

}

#-------------------------------------------------------------------------------------------------------------
# R-Complaint-Classifier
