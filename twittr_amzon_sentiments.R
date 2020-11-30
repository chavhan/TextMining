## Twitter analysis for NPL
library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)

library(tm)
library(slam)
install.packages("topicmodels")
library(topicmodels)


setup_twitter_oauth("eOtcKxxxxxxxxxxxxjjjjjssss", 
                    "TedjjjjjjdhhhhhhiiikkkkH2qppgD7wxxxxxxx",
                    "896758586243653632-6RCrlxxxxxxxxxxxx", # Access token
                    "QYLajXrqdbMYFKwno9itPNoxxxxxxxxxxxxxxxxxxxxx")  # Access token secret key

Tweets <- userTimeline('BarackObama', n = 1000) ## twitter account of barack obama 


TweetsDF <- twListToDF(Tweets)
View(TweetsDF)
write.csv(TweetsDF, "Tweets_sarf.csv")

getwd() ## to look where the file save 
obama_tweets <- obama_tweets[,2]
head(obama_tweets)
str(obama_tweets)
mydata.corpus <- Corpus(VectorSource(obama_tweets))   ##making or corpus
inspect(mydata.corpus)
corpus_clean <- tm_map(mydata.corpus, tolower)   ## all words should be lowercase 
inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean,removeNumbers)  ##  removing numbers 
stopwords("stop")
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords()) ## restricting words by stop txt
corpus_clean <- tm_map(corpus_clean,removePunctuation)  ## remove punctuation 
corpus_clean <- tm_map(corpus_clean,stripWhitespace)

str(corpus_clean)

mydata.tdm <- DocumentTermMatrix(corpus_clean)
dim(mydata.tdm)     ## 484 2609

## getting top 10 terms 

##dtm <- as.DocumentTermMatrix(mydata.)
rowTotals <- apply(mydata.tdm , 1, sum)
rowTotals
dtm.new   <- mydata.tdm[rowTotals> 0, ]
library(NLP)
lda <- LDA(dtm.new, 10) # find 10 topics
term <- terms(lda, 10) # first 5 terms of every topic
term


####################### Emotion mining ##############################
install.packages("syuzhet")
library("syuzhet")

str(obama_tweets)
#my_example_text <- readLines(obama_tweets)
s_v <- get_sentences(obama_tweets)
class(s_v)
str(s_v)
(s_v)


sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
sentiment_vector

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)
sentiment_vector
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(sentiment_vector)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

ft_values

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_data

# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])


# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

## Taking production from amazon site 
library(rvest)
library(XML)
library(magrittr)


aurl <- "https://www.amazon.in/product-reviews/B086CGNG4T/ref=cm_cr_arp_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
amazon_reviews <- NULL
for (i in 1:5){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  murl
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
  View (amazon_reviews)
}
write.table(amazon_reviews,"sonybravia108.txt",row.names = F)

raw_amazon <- readLines('path-to-file/textMining/amazon/sonybravia108.txt')
head(raw_amazon)
class(raw_amazon)
mydata1.corpus <- Corpus(VectorSource(raw_amazon))   ##making or corpus
inspect(mydata1.corpus)
corpus_clean1 <- tm_map(mydata1.corpus, tolower)   ## all words should be lowercase 
inspect(corpus_clean1)
corpus_clean1 <- tm_map(corpus_clean1,removeNumbers)  ##  removing numbers 
stopwords('en')
?stopwords
corpus_clean1 <- tm_map(corpus_clean1,removeWords,stopwords()) ## restricting words by stop txt
corpus_clean1 <- tm_map(corpus_clean1,removePunctuation)  ## remove punctuation 
corpus_clean1 <- tm_map(corpus_clean1,stripWhitespace)

s_v1 <- get_sentences(raw_amazon)
class(s_v1)
str(s_v1)
(s_v1)

sentiment_vector1 <- get_sentiment(s_v1, method = "bing")
head(sentiment_vector1)
sentiment_vector1

sentiment_vector2 <- get_sentiment(s_v, method = "syuzhet")
head(sentiment_vector2)
sentiment_vector2

sum(sentiment_vector2)
mean(sentiment_vector2)
summary(sentiment_vector2)

# plot
plot(sentiment_vector2, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence",
abline(h = 0, col = "red"))

# To extract the sentence with the most negative emotional valence
negative1 <- s_v1[which.min(sentiment_vector2)]
negative1

# and to extract the most positive sentence
positive1 <- s_v1[which.max(sentiment_vector2)]
positive1



# percentage based figures
percent_vals <- get_percentage_values(sentiment_vector2)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

# categorize each sentence by eight emotions
nrc_data1 <- get_nrc_sentiment(s_v1)
nrc_data1

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data1))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:10)

## wordcloud on twitter and amazon

##########################  twitter Word Cloud   #################################3 
twitter.tdm <- TermDocumentMatrix(corpus_clean,
                                  control = list(minwordLength = c(1,Inf))
                                  )
findFreqTerms(twitter.tdm,lowfreq = 10)
twitter.termFrequency <- rowSums(as.matrix(twitter.tdm))
twitter.termFrequency <- subset(twitter.termFrequency,twitter.termFrequency>=10)
library(ggplot2)
barplot(twitter.termFrequency,las = 2, col = rainbow(20))

library(wordcloud)
m <- as.matrix(twitter.tdm)
twitter.wordFrequency <- sort(rowSums(m),decreasing = T)
wordcloud(words = names(twitter.wordFrequency), freq = twitter.wordFrequency, min.freq = 10, random.order = F, colors = rainbow(20))

pos <-  scan(file.choose(), what = "character", comment.char = ';')
neg <-  scan(file.choose(), what = "character", comment.char = ';')

## positive words cloud 
pos.twitter.match <- match(names(twitter.wordFrequency),pos)
pos.twitter.match <- !is.na(pos.twitter.match)
freq.pos.twitter <- twitter.wordFrequency[pos.twitter.match]
p.twiter.names <- names(freq.pos.twitter)
p.twiter.names
wordcloud(p.twiter.names, freq = twitter.wordFrequency, min.freq = 10, random.order = F, 
          colors = rainbow(20))
## negative words cloud 
neg.twitter.match <- match(names(twitter.wordFrequency),neg)
neg.twitter.match <- !is.na(neg.twitter.match)
freq.neg.twitter <- twitter.wordFrequency[neg.twitter.match]
n.twiter.names <- names(freq.neg.twitter)
n.twiter.names
wordcloud(n.twiter.names, freq = twitter.wordFrequency, min.freq = 10, random.order = F, 
          colors = rainbow(20))

##########################  Amazon Word Cloud   #################################3 
amazon.tdm <- TermDocumentMatrix(corpus_clean1,
                                 control = list(minwordLength = c(1,Inf))
)
findFreqTerms(amazon.tdm,lowfreq = 10)
amazon.termFrequency <- rowSums(as.matrix(amazon.tdm))
amazon.termFrequency <- subset(amazon.termFrequency,amazon.termFrequency>=10)
library(ggplot2)
barplot(amazon.termFrequency,las = 2, col = rainbow(20))

library(wordcloud)
m1 <- as.matrix(amazon.tdm)
amazon.wordFrequency <- sort(rowSums(m1),decreasing = T)
wordcloud(words = names(amazon.wordFrequency), freq = amazon.wordFrequency, min.freq = 10, random.order = F, colors = rainbow(20))


## positive words cloud 
pos.amazon.match <- match(names(amazon.wordFrequency),pos)
pos.amazon.match <- !is.na(pos.amazon.match)
freq.pos.amazon <- amazon.wordFrequency[pos.amazon.match]
p.amazon.names <- names(freq.pos.amazon)
p.amazon.names
wordcloud(p.amazon.names, freq = amazon.wordFrequency, min.freq = 10, random.order = F, 
          colors = rainbow(20))
## negative words cloud 
neg.amazon.match <- match(names(amazon.wordFrequency),neg)
neg.amazon.match <- !is.na(neg.amazon.match)
freq.neg.amazon <- amazon.wordFrequency[neg.amazon.match]
n.amazon.names <- names(freq.neg.amazon)
n.amazon.names
wordcloud(n.amazon.names, freq = amazon.wordFrequency, min.freq = 10, random.order = F, 
          colors = rainbow(20))