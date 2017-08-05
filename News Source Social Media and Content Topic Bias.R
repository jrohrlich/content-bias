
# An Analysis of News Source Social Media and Content Topic Bias

# Jordan G. Rohrlich, 2017
# PHIL 4500 Final Project - University of Virginia, Corcoran Department of Philosophy

source('..path/textman.R') # helper functions based on Jockers 2014

library(rtweet)
library(scales)
library(htmlwidgets)
library(syuzhet)
library(mallet)
library(wordcloud)
library(stringr)
library(ggplot2)
library(plotly)


## app name on Twitter API
appname <- "___"
## api key
key <- "	___"
## api secret
secret <- "___"
## access token
token <- "___"
token_secret <- "	___"

# set environment variable
home_directory <- path.expand("~/working_directory")
twitoken <- file.path(home_directory, "twitter_token.rds")
saveRDS(token, file = twitoken)


# Identify news sources to analyze
sourcenames <- c( 
  "OccupyDemocrats",
  "HuffingtonPost",
  "TheAtlantic",
  "voxdotcom",
  "nytimes",
  "AP",
  "CNN", 
  "politico",
  "WSJ",
  "TheEconomist",
  "FoxNews",
  "RealClearNews",
  "BreitbartNews", 
  "BuzzFeed", 
  "TheOnion")

# df of user info
users.df <- lookup_users(sourcenames)

# iterating through sources, adding data to single data frame and list

sources.df <- NULL

for (source in sourcenames) {
  data <- get_timeline(source, n=2000)
  data[ which(data$is_retweet==FALSE), ] # remove RTs
  sources.df <- rbind(sources.df, data)
  print(paste("Completed: ", source)) # shows how far the fetching has progressed
}

# See oldest tweets from fetched set (April 8th is latest, subset all starting then)
for (source in sourcenames) {
  print(sources.df[which(sources.df$screen_name==source), 3][2000])
}

# save externally
sources.df <- subset(sources.df, as.Date(sources.df$created_at) > as.Date("2017-04-08")) # make sure all sources have tweets starting at same time

############## LIMIT EXCESS TWEET NUMBERS

write.csv(file="newsdata", x=sources.df)
sources.df <- subset(read.csv("newsdata"), select=screen_name:bounding_box_type) # read in data, cuts index column

# check number of observations after subsetting (most around or above 700 observations)
for (item in sourcenames) { 
  print(nrow(subset(sources.df, sources.df$screen_name==item)))
}

# collect linked articles from tweets
links.l <- list()
for (source in sourcenames) {
  text <- sources.df[which(sources.df$screen_name==source), 5]
  text <- paste(text, collapse=" ")
  # extract expressions using stringr
  text.links <- str_extract_all(text, "https://t.co\\S{11}") # aggregates hyperlinks
  # vector of tweets for source, filtered by source and date
  links.l[[source]] <- text.links 
  # clean to get rid of bad characters (links and non-words)
}
# need equal weight of corpus. Lowest link number is 200 (RealClearNews), cut link vectors down to approximately 200
for (i in seq(1:length(links.l))) {
  interval <- floor(length(links.l[[i]][[1]])/200)  # ratio of tweet excess
  links.l[[i]] <- links.l[[i]][[1]][seq(interval, length(links.l[[i]][[1]]), interval)] # new link vector of selected links
}

# We now have:
# An AGGREGATE DATA FRAME, a DATA FRAME of user data, and a LIST OF ARTICLE LINKS


# WEB ARTICLE SCRAPING AND CREATION OF TOPIC CORPUS


# get article words
library(rvest)

corpus <- list()

scraper.pattern <- list("OccupyDemocrats"= ".post-content p",
                        "HuffingtonPost"= ".content-list-component p",
                        "TheAtlantic"= ".article-body p",
                        "voxdotcom"= ".c-entry-content p",
                        "nytimes"= ".story-body-text",
                        "AP"= ".articleBody p",
                        "CNN"= ".zn-body__paragraph",
                        "politico"= ".story-text p",
                        "WSJ"= ".module p",
                        "TheEconomist"= ".blog-post__text p",
                        "FoxNews"= ".article-body p",
                        "RealClearNews"= ".article-body-text p",
                        "BreitbartNews"= ".entry-content p",
                        "BuzzFeed"= ".subbuzz h3",
                        "TheOnion"= ".content-text p")

for (i in seq(from=1, to=200)) {  
  for (source in sourcenames) {
    article <- read_html(links.l[[source]][i])
    
    # Scrape body text from html elements
    body <- article %>%
      html_nodes(scraper.pattern[[source]]) %>%
      html_text()
    
    # entire article into single string
    whole.body <- paste(body, collapse=" ")
    
    # split on non characters/numbers/etc
    whole.body <- gsub("[^A-Za-z1-9'# ]", " ", whole.body)
    
    # combine article texts into corpus for each news source
    corpus[[source]] <- c(corpus[[source]], tolower(whole.body))
    corpus[[source]]
    print(paste("Scraped: ", as.character(i), " out of ", source)) ###as.character(length(links.l[[source]]))))
  }
  print(paste("COMPLETED: ", as.character(i)))
}

# if success, save data
corpus.clone.v1 <- corpus
# if not, reset from backup
corpus <- corpus.clone.v1







# DISPERSION PLOTS FOR KEY TERMS ---------------

# only use sources with > 1000
high.count.sources <- c( 
  "TheAtlantic",
  "nytimes",
  "AP",
  "CNN", 
  "politico",
  "FoxNews",
  "WSJ",
  "TheEconomist")

terms <- c("trump", "russia", "bomb")
par(mfrow=c(5, 1))
term <- "russia"
# make dispersion plots for each term and each source
for (term in terms) {
  for (source in high.count.sources) {
    
    ### corpus.v <-  corpus[[source]] # for full article data
    tweets.v <-  sources.df[which(sources.df$screen_name==source), 5] # for only tweet data
    words.v <- textman_lines_to_words(tweets.v)
    
    # save externally
    
    png(paste(term, source, 'tweets.png')) 
    textman_get_word_distro_plot(words.v, term, title_suffix = paste("in", source, "tweets"))  # for tweets
    ##png(paste(term, source, 'articles.png')) 
    ##textman_get_word_distro_plot(words.v, term, title_suffix = paste("in", source, "articles"))  # for full article data
    dev.off()
  }
}









# SENTIMENT ANALYSIS AND ENGAGEMENT CORRELATION ---------------

# create data matrix with:
# source, 
# sentiment versus retweet correlation, 
# sum of tweet sentiment,
# number of total followers

sentiment.correlation.df <- data.frame()

for (source in sourcenames) {
  # vector of tweets for source
  text.v <- sources.df[which(sources.df$screen_name==source), 5] # FOR TWEETS
  # clean to get rid of bad characters (links and non-words)
  text.v <- gsub("[\\S]*http\\S+", " ", text.v)
  text.v <- gsub("[^A-z'\"]", " ", text.v) #NEED TO ADD APOSTROPHES
  # sentiment values
  sentiment.values.v <- get_sentiment(text.v, method="syuzhet")
  # sum for this source
  sent.sum <- sum(sentiment.values.v)
  # vector of number of retweets
  retweet.v <- sources.df[which(sources.df$screen_name==source), 6]
  # assiciation between sentiment and engagement
  sent.cor <- cor(sentiment.values.v,retweet.v)
  # bind to data frame
  entry.df <- data.frame(source=source, total_sentiment=sent.sum, correlation=sent.cor)
  sentiment.correlation.df <- rbind(sentiment.correlation.df, entry.df)
}

# average correlation of tweet sentiment to retweet
tweet.sent.cor <- mean(sentiment.correlation.df[,3]) # -0.0458

# add follower counts for sources
sentiment.correlation.df <- cbind(sentiment.correlation.df, users.df[ , 7])
colnames(sentiment.correlation.df)[4]<- "Followers"

# relationship between sentiment of tweet and retweets
plot(sentiment.correlation.df[,1], sentiment.correlation.df[,3]) # uncorrelated, ranging from -0.15 to 0.05

# correlation between source total sentiment and total follower count
cor.test(sentiment.correlation.df[,2], sentiment.correlation.df[,4]) # significant at p=0.1

write.csv(file="sentiment", x=sentiment.correlation.df)

## No correlation between tweet sentiment and retweets
## Weak to no correlation between total sentiment and followers











# CLUSTERING ---------------------------------------------

# Pull words, pull apart into bag of used words for each
getWordTable <- function(source){
  user.text <- source[, "text"] #sapply for each tweet in list
  user.text <- paste(user.text, collapse=" ")
  user.text <- gsub("[^\\s]*http\\S+", " ", user.text) #subs out hyperlinks
  user.words.raw <- unlist(strsplit(user.text, "\\W"))
  user.words <- tolower(user.words.raw[which(user.words.raw != "")])
  #stopwords.v <- scan("stopwords.txt", what="character", sep="\n")
  #non.stopwords.v <- user.words[! user.words %in% stopwords.v] # only takes keywords for measure distance
  user.freqs.t <- sort(table(non.stopwords.v), decreasing=TRUE)
  user.freqs.rel.t <- sort(100*(user.freqs.t/sum(user.freqs.t)))
}

# make sure list is formed
sources.dfs.l <- list()
for (source in sourcenames) {
  data <- sources.df[ which(sources.df$screen_name==source), ]
  sources.dfs.l[[source]] <- data
}

# get word frequencies for each source, organize in list
sources.freqs.l <- list()

# FOR TWEETS
for (source in sourcenames) {
  sources.freqs.l[[source]] <- getWordTable(sources.dfs.l[[source]]) # puts source df into word table function
}

# FOR FULL ARTICLES
for (source in sourcenames) {
  sources.freqs.l[[source]] <- getWordTable(corpus[[source]])
}


# turn into list of frequencies
freqs.l <- mapply(
  data.frame,
  ID=seq_along(sources.freqs.l),
  sources.freqs.l,
  SIMPLIFY=FALSE, 
  MoreArgs=list(stringsAsFactors=FALSE)
)
sources.freqs.df <- do.call(rbind,freqs.l)

# saves data so it dosn't need to be requeried from Twitter API
write.csv(file="sourcesFreqData", x=sources.freqs.df)
sources.freqs.df <- subset(read.csv("sourcesFreqData"), select=ID:Freq)

# reshape into frequency matrix, apply threshold
result <- xtabs(Freq ~ ID+non.stopwords.v, data=sources.freqs.df)
final.m <- apply(result, 2, as.numeric)
dim(final.m)
thresh = 0.05
smaller.m <- final.m[, apply(final.m,2, mean) >= thresh]
dim(smaller.m) # See dimensions of smaller matrix


# Little Matrix into Eucl. Distance Object
dm <- dist(smaller.m)

# Distance Object into Hier. Cluster Object
cluster <- hclust(dm, method = "complete")
cluster$labels <- names(sources.freqs.l)

# produce and save plot
png('dendrogram-tweet-data-keywords.png')
plot(cluster)
rect.hclust(cluster, k=5, border="red") 
dev.off()








# TOPIC MODELLING ---------------------------------------------

#if sources.dfs.l not created yet:
sources.dfs.l <- list()
for (source in sourcenames) {
  data <- sources.df[ which(sources.df$screen_name==source), ]
  sources.dfs.l[[source]] <- data
}

# Modeling with wrapped functions

docs.l <- list()

# TWEET DATA
for (source in sourcenames) {
  docs.l[[source]] <- sources.df[which(sources.df$screen_name==source), 5]
}

# FULL ARTICLE DATA
docs.l <- corpus

source('..path/textman.R') # helper functions based on Jockers 2014

# Create a corpus of documents
corpus.df = textman_get_doc_corpus(docs.l)

# Generate the model
topic_model = textman_get_topic_model(corpus.df)

# Grab some info from the model
topic_info = textman_get_topic_model_info(topic_model)

# Better list of labels
better_topic_labels = textman_get_topic_labels(topic_info)
print(better_topic_labels)


# rename topics subjectively based on Topic Model output
topicnames <- c("SCOTUS", 
                "French Election", 
                "Police/Violence", 
                "O'Reilley", 
                "Baldwin SNL",
                "Politics/People",
                "Curt Schilling", # red sox pitcher running for MA senate
                "Trump Administration",
                "North Korea",
                "Syria/Russia/Trump",
                "Education",
                "Healthcare",
                "Jobs Market",
                "Tech/Transportation",
                "Energy Politics",
                "Partisan Politics",
                "Easter Bombings",
                "Trump",
                "Marijuana Research",
                "Turkish Referendum")

# Reproduce topic data as dataframe of sources and words by freq

# use full article data, don't trim for keywords
getWordTable <- function(source){
  #user.text <- source[, "text"] #sapply for each tweet in list, if using tweets
  user.text <- paste(source, collapse=" ")
  user.text <- gsub("[^\\s]*http\\S+", " ", user.text) #subs out hyperlinks
  user.words.raw <- unlist(strsplit(user.text, "\\W"))
  user.words <- tolower(user.words.raw[which(user.words.raw != "")])
  user.freqs.t <- sort(table(non.stopwords.v), decreasing=TRUE)
  user.freqs.rel.t <- sort(100*(user.freqs.t/sum(user.freqs.t)))
}
for (source in sourcenames) {
  sources.freqs.l[[source]] <- getWordTable(corpus[[source]])
}
freqs.l <- mapply(
  data.frame,
  ID=seq_along(sources.freqs.l),
  sources.freqs.l,
  SIMPLIFY=FALSE,
  MoreArgs=list(stringsAsFactors=FALSE)
)
sources.freqs.df <- do.call(rbind,freqs.l)

# saves data so it dosn't need to be reproduced
write.csv(file="TopicModelsourcesFreqData", x=sources.freqs.df)
sources.freqs.df <- subset(read.csv("TopicModelsourcesFreqData"), select=ID:Freq)

# reshape into frequency matrix
result <- xtabs(Freq ~ ID+non.stopwords.v, data=sources.freqs.df)
corpusfreqs.m <- apply(result, 2, as.numeric) # matrix of all words of topics with freqs
dim(corpusfreqs.m)

# iterate through topics, create column vector of distance scores with sources
scores.m <- NULL
for (t in seq(1:20)) { 
  topic.scores.v <- NULL
  for (i in seq(1:15)) { # find distance between a source and a topic, for each source
    topic.wordfreqs.v <- better_topic_labels[[t]][,2] # topic freqs
    source.wordfreqs.v <- corpusfreqs.m[i,better_topic_labels[[t]]$words] # source freqs for top words in topic
    topic.to.source.m <- rbind(topic.wordfreqs.v, source.wordfreqs.v)
    score <- dist(topic.to.source.m)[1] # distance between source freqs and topic freqs
    topic.scores.v <- c(topic.scores.v, score)
  }
  scores.m <- rbind(scores.m, topic.scores.v) # matrix with sources as rows, topics by col, dist as value
}

# rename for convenience
colnames(scores.df)<- sourcenames
rownames(scores.df)<- topicnames

# reshape longways, create vector of all scores
df.test_data <- expand.grid(Sources = sourcenames
                            ,Topics = topicnames)
scores.v <- NULL
for (i in seq(1:20)) {
  topic.scores.v <- scores.m[i,]
  scores.v <- c(scores.v, topic.scores.v)
}

df.test_data$score <- scores.v

# PLOT HEATMAP
png('topic_sources_heatmap.png')

ggplot(data = df.test_data , aes(x = Sources, y = Topics)) +
  geom_tile(aes(fill = score), colour="white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.text.x = element_text(angle = 320, hjust = 0, colour = "grey50"))

dev.off()



