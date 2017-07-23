library(twitteR)
library(ROAuth)
library(stringr)
library(tm)
library(wordcloud)

#Connect to twitter API Method 1 
#Use the following link to get Twitter API
load("twitter authentication.Rdata")
setup_twitter_oauth(cred$consumerKey, cred$consumerSecret, cred$oauthKey, cred$oauthSecret)

#Connect to twitter API Method 2
#ConsumerKey <-	"LXXXXXXX"
#ConsumerSecret <- "bfXXXXXX"
#AccessToken <- "1XXXXXX-XXXXXXXX"
#AccessTokenSecret <- "XXXXXXXXXXX"
#setup_twitter_oauth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)

#Search Tweets using Keyword or get all the tweets of a user
#tweets <- searchTwitter('#DataLove', n=10, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)

#Get tweets of celebrities and plot wordcloud
screenName <- c('SrBachchan', 'imVkohli', 'AndrewYNg', 'sundarpichai')
checkHandles <- lookupUsers(screenName)
UserData <- lapply(checkHandles, function(x) getUser(x))
UserData <- twListToDF(UserData)
table(UserData$name, UserData$statusesCount) #Check Status Count
table(UserData$name, UserData$protected)   #Check Private Accounts

usernames <- subset(UserData, protected == FALSE)
usernames <- as.list(usernames$screenName)

x <- userTimeline('SrBachchan',n=3200,includeRts = TRUE)

#Convert tweets to dataframe
AmitabhData <- twListToDF(x)

#Wait 5 minutes  
Sys.sleep(300)

x <- lapply(usernames[c(2:3)], function(x) userTimeline(x,n=3200,includeRts = TRUE))

#Make each into a dataframe
Y <- lapply(x,function(x) twListToDF(x))

#Wait 5 minutes 
Sys.sleep(300)

x <- userTimeline('sundarpichai',n=3200,includeRts = TRUE)

#Make each into a dataframe
SundarPichaiData <- twListToDF(x)

Amitabh.df <- data.frame(AmitabhData)
Andrew.df <- data.frame(Y[2])
Kohli.df <- data.frame(Y[1])
Sundar.df <- data.frame(SundarPichaiData)

#Now create a dataframe that combines all of the collected tweets
tweets <- data.frame()

tweets <- Amitabh.df
tweets <- rbind(tweets,Andrew.df)
tweets <- rbind(tweets,Kohli.df)
tweets <- rbind(tweets,Sundar.df)

#Process these togeether using text mining package and create wordcould for each 
iconv(tweets$text, from="UTF-8", to="ASCII", sub="")
tweets$text=str_replace_all(tweets$text,"[^[:graph:]]", " ")

tweets$text <- gsub("[^[:alnum:]///' ]", "", tweets$text)
tweets$text <- tolower(tweets$text)
tweets$text <- gsub("rt", "", tweets$text)
tweets$text <- gsub("[[:punct:]]", "", tweets$text)
tweets$text <- gsub("http\\w+", "", tweets$text)
tweets$text <- gsub("[ |\t]{2,}", "", tweets$text)
tweets$text <- gsub("^ ", "", tweets$text)
tweets$text <- gsub(" $", "", tweets$text)
# Can remove usernames from the tweets using the following code line but I preferred not to 
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

#Remove English Stopwords from the tweets
Amit <- tm_map(Amit, removeWords, stopwords("en"))
Andrew <- tm_map(Andrew, removeWords, stopwords("en"))
Kohli <- tm_map(Kohli, removeWords, stopwords("en"))
Sundar <- tm_map(Sundar, removeWords, stopwords("en"))

#Remove numbers in necessary
Amit <- tm_map(Amit, removeNumbers)

#Create wordcloud using the 
wordcloud(Amit,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 100)
wordcloud(Sundar,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)
wordcloud(Andrew,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)
wordcloud(Kohli,min.freq = 2, scale=c(5,0.4),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 150)