##### Twitter Analysis ####

#### 1. load packages ####
Needed <- c("twitterR", "SentimentAnalysis", 'quanteda','tm','EGAnet','tidytext','wordcloud')
install.packages(Needed, dependencies = TRUE)
library(rtweet)
library(twitteR)
library(dplyr)
library(tidyr)
library(tidytext)
library(textdata)
library(lubridate)
library(SentimentAnalysis)
library(quanteda)
library(ggplot2)
library(tm)
library(devtools)
library(EGAnet)
library(forestmangr)
library(wordcloud)
library(syuzhet)
library(base64enc)
library(tidyverse)
library(tibble)
library(usmap)
library("ggplot2")


## 2. Set parameters for twitter app access
api_key <- 'xxxxxxxxxxxxxxxxxxxxxxxxxx'
api_secret_key <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_token <- 'xxxxxxxxxxxxxxxxx-xxxxxxxxxxxxxxxxxxx'
access_token_secret <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

# 2.1 authenticate via web browser

token <- create_token(
  app = "SocialListeningProject",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

## 2.2 Check to see if token is loaded

get_token()

#### 3. Obtain tweeets for a specific topic ####

key_word<- rtweet::search_tweets(
  q = "#battery",
  n=5000,
  include_rts = FALSE) # want to exclude retweets

key_word #output resulting twitter tweet_type_dfframe #twitter rest API limits all search to the past 6-9 days


#### 4. Transform tweets data ####

## 4.1 Remove retweets ##

key_word_organic <- key_word[key_word$is_retweet==FALSE,]

## 4.2 Remove replies

key_word_organic <- subset(key_word_organic,is.na(key_word_organic$reply_to_status_id))

key_word_df<-as.tweet_type_df.frame(key_word_organic)
saveRDS(key_word_df,file="key_word_tweet.rds")


## 4.3 keeping only the retweets
key_word_retweets<-key_word[key_word$is_retweet==TRUE,]
key_word_replies<-subset(key_word,!is.na(key_word$reply_to_status_id))

# 4.4 creating a tweet_type_df frame based on number of rows for each type of tweet
tweet_type_df<-data.frame(
  category=c("Organic","Retweets","Replies"),
  count=c(nrow(key_word_organic),nrow(key_word_retweets),nrow(key_word_replies)))


## 4.5 Setting up tweet_type_df for visualisation
tweet_type_df$fraction = tweet_type_df$count/sum(tweet_type_df$count)
tweet_type_df$percentage = tweet_type_df$count/sum(tweet_type_df$count)*100
tweet_type_df$ymax = cumsum(tweet_type_df$fraction)
tweet_type_df$ymin = c(0,head(tweet_type_df$ymax,n=-1))

# 4.6 Rounding the tweet_type_df to two decimal points
tweet_type_df<-round_df(tweet_type_df,2)

# 4.7 Specify what the legend should say
Type_of_tweet<-paste(tweet_type_df$category, tweet_type_df$percentage,"%")
ggplot(tweet_type_df,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Type_of_tweet))+
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.position="right")

#### 5. Visualise tweets ####

ts_plot(key_word_organic, by = "3 hours") +
  theme(plot.title = element_text(face = "bold")) + 
  labs(x = NULL, y= NULL,
       title = "Frequency of tweets from past 9 days", 
       subtitle = "Twitter status (tweet) counts aggregated using three hour intervals",
       caption = "\n Source: tweet_type_df collected from Twitter's rest API via retweet")


## 5.1 Who is tweeting ##
users<-search_users("#battery",n=1000)
users_df<-as.tweet_type_df.frame(users)

saveRDS(users_df,file="users_tweet.rds")
#just view the first 10 users
head(users, n=10)

## 5.2 how many locations are represented
length(unique(users$location))

sep_location <- str_split(users$location,',')

state_split<-word(string = sep_location, start = -1)

count<- c() #only US cities, states
for (i in state_split) {
  count <- append(count, gsub("[^A-Za-z]", "", i))
}
users$state<-count
users<-filter(users, state =='AL'|state =='AK'|state=='AZ'|state =='AR'|
                state =='CA'|state=='CZ'|state=='CO'|state=='CT'|state =='DE'|
                state =='DC'|state=='FL'|state=='GA'|state=='GU'|state=='HI'|
                state=='ID'|state=='IL'|state=='IN'|state=='IA'|state=='KS'|
                state=='KY'|state=='LA'|state=='ME'|state=='MD'|state=='MA'|
                state=='MI'|state=='MN'|state=='MS'|state=='MO'|state=='MT'|
                state=='NE'|state=='NV'|state=='NH'|state=='NJ'|state=='NM'|
                state=='NY'|state=='NC'|state=='ND'|state=='OH'|state=='OK'|
                state=='OR'|state=='PA'|state=='PR'|state=='RI'|state=='SC'|
                state=='SD'|state=='TN'|state=='TX'|state=='UT'|state=='VT'|
                state=='VI'|state=='VA'|state=='WA'|state=='WV'|state=='WI'|
                state=='WY')

users %>%
  count(location,sort=TRUE) %>%
  mutate(location=reorder(location,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x=location,y=n))+
  geom_col()+
  coord_flip()+
  labs(x="Location",
       y="Count",
       title="Twitter users - unique locations - Top 10 by usage")



## 5.3 most frequent words ##
key_word_organic$text <- gsub("https\\s*","",key_word_organic$text)
key_word_organic$text <- gsub("@\\s*","",key_word_organic$text)
key_word_organic$text  <-gsub("amp","",key_word_organic$text)
key_word_organic$text <-gsub("[[:punct:]]","",key_word_organic$text)


key_word_organic$text <-gsub("@[[:alpha:]]*","",key_word_organic$text)

#draw out only the text column because we are interested in cleaning it with tm
text_corpus<-Corpus(VectorSource(key_word_organic$text))

#remove words like car, selfdriving cars, autonomous vehicles as they are correlated to selfdriving cars
#rt = retweet, re = reply
text_corpus<-tm_map(text_corpus,removeWords, c("selfdrivingcars","driverlesscars","autonomousvehicles","cars","car","rt","re","vehicle","selfdriving","driverless","autonomous"))

#remove stop words
text_corpus<-tm_map(text_corpus,removeWords,stopwords("english"))

#remove punctuation
text_corpus<-tm_map(text_corpus,removePunctuation)
text_corpus<-tm_map(text_corpus,removeNumbers)



#find most frequent words

dtm<-TermDocumentMatrix(text_corpus)
m <-as.matrix(dtm)
v <-sort(rowSums(m),decreasing=TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,10)



#visualize
set.seed(1234)
wordcloud(words=d$word,freq=d$freq,min.freq=10,max.words=200,random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))


#convert cleansed corpus back into dataframe

key_word_tweets <- data.frame(text_clean = get("content",text_corpus),
                              stringsAsFactors=FALSE)

#process each set of tweets into tidy text or corpus objects..
tweets.key_word = key_word %>% select(screen_name,text)
tweets.key_word

head(tweets.key_word$text)
tweets.keyword1 <- key_word_organic %>%
  select(text) %>%
  unnest_tokens(word, text)

cleaned_key_word_organic <- tweets.keyword1 %>%
  anti_join(stop_words)

# TOP 10 words in #battery tweets
cleaned_key_word_organic %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y =n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(y = 'count', 
       x = 'unique words',
       title = 'unique word counts')

# Visualize in map

users_sum <- users %>%
  group_by(state) %>%
  summarise(state_sum = n()) #count the number of users by states

us_map <- usmap::us_map(region = 'states')

plot_usmap(
  data = users_sum, values = 'state_sum', color = 'red'
)+
  scale_fill_continuous(
    low = 'white', high = 'red', name = 'number of users(n)', label = scales ::comma
  )+
  labs(title = 'Number of Users by US States')+
  theme(legend.position = 'right')

#### 6. Sentiment analysis of tweets ####


##Converting tweets to ASCII To get rid of strange characters
key_word_tweets$text_clean<-iconv(key_word_tweets$text_clean,from="UTF-8",to="ASCII",sub="")

#removing mentions, in case needed
key_word_tweets$text_clean<-gsub("@\\w+","",key_word_tweets$text_clean)
tweet_sentiment<-get_nrc_sentiment((key_word_tweets$text_clean))
sentimentscores<-data.frame(colSums(tweet_sentiment[,]))
names(sentimentscores)<-"Score"
sentimentscores<-cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores)<-NULL

ggplot2::ggplot(data=sentimentscores)+
  geom_bar(mapping=aes(x=sentiment,y=Score),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()


## Only look at negative sentiment ##

key_word_sentiments<-analyzeSentiment(key_word_tweets$text_clean)

##select subset of measures
key_word_sentiments_subset<-dplyr::select(key_word_sentiments,
                                          SentimentGI,SentimentHE,
                                          SentimentLM,SentimentQDAP,
                                          WordCount)

#create a mean value for each tweet's sentiment level, leaving us with a single sentiment value for each tweet
key_word_sentiments_subset <-dplyr::mutate(key_word_sentiments_subset,
                                           mean_sentiment=rowMeans(key_word_sentiments_subset[,-5]))


# remove unnecessary sentiment measures
key_word_sentiments_subset<-dplyr::select(key_word_sentiments_subset,
                                          WordCount,
                                          mean_sentiment)

key_word_df<-cbind.data.frame(key_word_tweets,key_word_sentiments_subset)

#only keep negative sentiments
key_word_df_negative<-filter(key_word_df,mean_sentiment<0)

nrow(key_word_df_negative)


#topic analysis of negative sentiments
key_word_tokenized_list<-tokens(key_word_df_negative$text_clean)

key_word_dfm<-dfm(key_word_tokenized_list)

#turn this list object into a document feature matrix with teh dfm command. 
#use the colsums command to get the count of use for every word, which we assign to the vector "word_sums"
word_sums<-colSums(key_word_dfm)

length(word_sums)#number of words

#which are the most frequent words
freq_negative_words<-data.frame(word=names(word_sums),
                                freq=word_sums,
                                row.names=NULL,
                                stringsAsFactors = FALSE)

sorted_freq_neg<-freq_negative_words[order(freq_negative_words$freq,decreasing=TRUE),]

#remove odd characters
key_word_df_negative$text_clean<-sapply(key_word_df_negative$text_clean,
                                        function(row) iconv(row,"latin1","ASCII",sub=""))

neg_corpus_tm<-Corpus(VectorSource(key_word_df_negative$text_clean))

neg_tm<-DocumentTermMatrix(neg_corpus_tm)

neg_tm<-removeSparseTerms(neg_tm,0.98)

neg_df_cluster<-as.data.frame(as.matrix(neg_tm))

#Create clusters using exploratory graph analysis

ega_neg_key_word<-EGA(neg_df_cluster)

View(as.data.frame(ega_neg_key_word$dim.variables))
