#Getting tweets from twitter
library(twitteR)
#Twitter-R Authentication
library(ROAuth)
#Text processing
library(stringr)
#Text mining
library(tm)
#Wordcloud creation
library(wordcloud)

#Connect to twitter API using your API tokens/keys
ConsumerKey <- "LXXXXXXX"
ConsumerSecret <- "bfXXXXXX"
AccessToken <- "1XXXXXX-XXXXXXXX"
AccessTokenSecret <- "XXXXXXXXXXX"
setup_twitter_oauth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)

#Search Tweets using Keyword or get all the tweets of a user
#tweets <- searchTwitter('#DataLove', n=10, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)

#Get tweets of celebrities and plot wordcloud

#Get twitter handles of the celebrities you wanna look up
screenName <- c('SrBachchan', 'imVkohli', 'AndrewYNg', 'sundarpichai')
#Check handles
checkHandles <- lookupUsers(screenName)
#Get user data 
UserData <- lapply(checkHandles, function(x) getUser(x))
#Convert to data frame for easy access
UserData <- twListToDF(UserData)
table(UserData$name, UserData$statusesCount) #Check Status Count
table(UserData$name, UserData$protected)   #Check Private Accounts

#Subset for public accounts
usernames <- subset(UserData, protected == FALSE)
usernames <- as.list(usernames$screenName)

#Get max 3200 tweets using the userTimeline function, Twitter only allows access to a users most recent 3240 tweets
#Include retweets, default is not to
x <- userTimeline('SrBachchan',n=3200,includeRts = TRUE)

#Convert tweets to dataframe for easy reading
AmitabhData <- twListToDF(x)

#Wait 5 minutes because of the API rate limits
Sys.sleep(300)

x <- lapply(usernames[c(2:3)], function(x) userTimeline(x,n=3200,includeRts = TRUE))

#Make each into a dataframe
Y <- lapply(x,function(x) twListToDF(x))

#Wait 5 minutes 
Sys.sleep(300)

x <- userTimeline('sundarpichai',n=3200,includeRts = TRUE)

SundarPichaiData <- twListToDF(x)

Amitabh.df <- data.frame(AmitabhData)
Andrew.df <- data.frame(Y[2])
Kohli.df <- data.frame(Y[1])
Sundar.df <- data.frame(SundarPichaiData)

#Now create a dataframe that combines all of the collected tweets for cleaning the text all at once 
tweets <- data.frame()

tweets <- Amitabh.df
tweets <- rbind(tweets,Andrew.df)
tweets <- rbind(tweets,Kohli.df)
tweets <- rbind(tweets,Sundar.df)

#Process these together using text mining package and create wordcould for each 
            iconv(tweets$text, from="UTF-8", to="ASCII", sub="")
#Clean text by removing graphic characters
            tweets$text=str_replace_all(tweets$text,"[^[:graph:]]", " ")
#Remove Junk Values and replacement words like fffd which appear because of encoding differences
            tweets$text <- gsub("[^[:alnum:]///' ]", "", tweets$text)
#Convert all text to lower case
            tweets$text <- tolower(tweets$text)
#Remove retweet keyword
            tweets$text <- gsub("rt", "", tweets$text)
#Remove Punctuations
            tweets$text <- gsub("[[:punct:]]", "", tweets$text)
#Remove links
            tweets$text <- gsub("http\\w+", "", tweets$text)
#Remove tabs
            tweets$text <- gsub("[ |\t]{2,}", "", tweets$text)
#Remove blankspaces at begining
            tweets$text <- gsub("^ ", "", tweets$text)
#Remove blankspaces at the end
            tweets$text <- gsub(" $", "", tweets$text)
#Can remove usernames from the tweets using the following code line but I preferred not to 
            tweets$text <- gsub("@\\w+", "", tweets$text)

#After preprocessing the data, subset for tweets for each handle
Kohli <- subset(tweets, screenName == "imVkohli", select = text)
Andrew <- subset(tweets, screenName == "AndrewYNg", select = text)
Amit <- subset(tweets, screenName == "SrBachchan", select = text)
Sundar <- subset(tweets, screenName == "sundarpichai", select = text)

#Create corpus of individual twitter handles 
Amit <- Corpus(VectorSource(Amit))
Kohli <- Corpus(VectorSource(Kohli))
Andrew <- Corpus(VectorSource(Andrew))
Sundar <- Corpus(VectorSource(Sundar))

#Remove English Stopwords (e.g. 'my', 'nodoby', 'do', 'today' etc.) from the tweets
Amit <- tm_map(Amit, removeWords, stopwords("en"))
Andrew <- tm_map(Andrew, removeWords, stopwords("en"))
Kohli <- tm_map(Kohli, removeWords, stopwords("en"))
Sundar <- tm_map(Sundar, removeWords, stopwords("en"))

#Remove numbers if necessary
Amit <- tm_map(Amit, removeNumbers)

#Create wordcloud using the wordcloud function and adjusting the parameters as per needs
wordcloud(Amit,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 100)
wordcloud(Sundar,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)
wordcloud(Andrew,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)
wordcloud(Kohli,min.freq = 2, scale=c(5,0.4),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)
