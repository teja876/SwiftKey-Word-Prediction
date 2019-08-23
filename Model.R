set.seed(123)

#reading twitter data
con <- file("en_US.twitter.txt", 'r')
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

twitter <- iconv(twitter , "UTF-8", "ASCII", sub = "")

#sampling 5% of twitter data
lines.count <- length(twitter)
samples <- rbinom(lines.count, 1, 0.05)==1
twitter.sample <- twitter[samples]

set.seed(123)
#reading news data
dir <- paste0("","")
con <- file(paste0(dir,"en_US.news.txt"), open = "rb")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

news <- iconv(news , "UTF-8", "ASCII", sub = "")


#sampling 5% of news data
lines.count <- length(news)
samples <- rbinom(lines.count, 1, 0.05)==1
news.sample <- news[samples]

set.seed(123)
#reading blogs data
con <- file("en_US.blogs.txt", 'r')
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

blogs <- iconv(blogs , "UTF-8", "ASCII", sub = "")


#sampling 5% of blogs data
lines.count <- length(blogs)
samples <- rbinom(lines.count, 1, 0.05)==1
blogs.sample <- blogs[samples]


con <- file("ProfaneWords.txt" , 'r')
Profane.words <- readLines(con)
close(con)

# summary statistics
summary <- data.frame('File' = c('twitter', 'news', 'blogs'),
                      'File size' = sapply(list(twitter, news, blogs), function(x){format(object.size(x), "MB")}),
                      'Lines' = sapply(list(twitter, news, blogs), function(x){length(x)}),
                      'chars' = sapply(list(twitter, news, blogs), function(x){sum(nchar(x))}))

summary


remove(con)
remove(twitter)
remove(samples)
remove(blogs)
remove(news)
remove(dir)
remove(lines.count)
remove(blogs.sample)
remove(news.sample)
remove(twitter.sample)
# remove(doc.vec)

total.sample <- c(blogs.sample, news.sample, twitter.sample)
#total.sample1 <- twitter.sample




library('tm')
library('SnowballC')
library('slam')
library('RWeka')
library(ggplot2)
library(tidytext)
library(dplyr)
library(tidyr)


doc.corpus <- VCorpus(VectorSource(total.sample))
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, tolower)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removePunctuation)
#doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, removeWords, Profane.words)
#doc.corpus <- tm_map(doc.corpus, stemDocument, language = "english")
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
doc.corpus <- tm_map(doc.corpus, PlainTextDocument)




unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

unigrams <- TermDocumentMatrix(doc.corpus, control = list(tokenize = unigramTokenizer))
bigrams <- TermDocumentMatrix(doc.corpus, control = list(tokenize = bigramTokenizer))
trigrams <- TermDocumentMatrix(doc.corpus, control = list(tokenize = trigramTokenizer))
quadgrams <- TermDocumentMatrix(doc.corpus, control = list(tokenize = quadgramTokenizer))

remove(doc.corpus)

# top.terms <- findFreqTerms(unigrams, 50)
# UniTermFreq <- rowSums(as.matrix(unigrams[top.terms,]))
# UniTermFreq <- data.frame(word = names(UniTermFreq), 
#                           frequency = UniTermFreq)
# UniTermFreq <- UniTermFreq[order(-UniTermFreq$frequency), ]
# 
# 
# 
# top.terms <- findFreqTerms(bigrams, 3)
# BiTermFreq <- rowSums(as.matrix(bigrams[top.terms,]))
# BiTermFreq <- data.frame(word = names(BiTermFreq), 
#                           frequency = BiTermFreq)
# BiTermFreq <- BiTermFreq[order(-BiTermFreq$frequency), ]
# 
# top.terms <- findFreqTerms(trigrams, 2)
# TriTermFreq <- rowSums(as.matrix(trigrams[top.terms,]))
# TriTermFreq <- data.frame(word = names(TriTermFreq), 
#                          frequency = TriTermFreq)
# TriTermFreq <- TriTermFreq[order(-TriTermFreq$frequency), ]
# 
# top.terms <- findFreqTerms(quadgrams, 2)
# QuadTermFreq <- rowSums(as.matrix(quadgrams[top.terms,]))
# QuadTermFreq <- data.frame(word = names(QuadTermFreq), 
#                           frequency = QuadTermFreq)
# QuadTermFreq <- QuadTermFreq[order(-QuadTermFreq$frequency), ]





df.uni <- tidy(unigrams)
df.uni <- summarise(group_by(df.uni, term), count = sum(count))
df.uni <- df.uni[order(-df.uni$count),]

df.bi <- tidy(bigrams)
df.bi <- summarise(group_by(df.bi, term), count = sum(count))
df.bi <- df.bi[order(-df.bi$count),]

df.tri <- tidy(trigrams)
df.tri <- summarise(group_by(df.tri, term), count = sum(count))
df.tri <- df.tri[order(-df.tri$count),]

df.quad <- tidy(quadgrams)
df.quad <- summarise(group_by(df.quad, term), count = sum(count))
df.quad <- df.quad[order(-df.quad$count),]

df.bi <- df.bi %>% separate(term, c('w1', 'w2'), sep = " ", remove = FALSE)
df.tri <- df.tri %>% separate(term, c('w1', 'w2', 'w3'),
                              sep = " ", remove = FALSE)
df.quad <- df.quad %>% separate(term, c('w1', 'w2', 'w3', 'w4'),
                                sep = " ", remove = FALSE)

# Visualizing Unigrams
g <- ggplot(df.uni[1:50,],
            aes(x = reorder(term, -count), y  = count))
g <- g + geom_bar(stat = "identity")
g <- g + labs(x = "UniGrams", y = "Frequency",
              title = "Frequency of top unigrams")
g <- g + theme(axis.text.x = element_text(angle = 90))
g

qplot(log(count),
      data = df.uni,bins = 50,
      main = "Histogram of frequencies of Unigrams",
      xlab = "Frequency",
      ylab = "count")

# Visualizing Bigrams
g <- ggplot(df.bi[1:50,],
            aes(x = reorder(term, -count), y  = count))
g <- g + geom_bar(stat = "identity")
g <- g + labs(x = "BiGrams", y = "Frequency",
              title = "Frequency of top bigrams")
g <- g + theme(axis.text.x = element_text(angle = 90))
g

qplot(log(count),
      data = df.bi, bins = 50,
      main = "Histogram of frequencies of Bigrams",
      xlab = "Frequency",
      ylab = "count")

# visualizing Trigrams
g <- ggplot(df.tri[1:50,],
            aes(x = reorder(term, -count), y  = count))
g <- g + geom_bar(stat = "identity")
g <- g + labs(x = "TriGrams", y = "Frequency",
              title = "Frequency of top trigrams")
g <- g + theme(axis.text.x = element_text(angle = 90))
g

qplot(log(count),
      data = df.tri, bins = 50,
      main = "Histogram of frequencies of trigrams",
      xlab = "Frequency",
      ylab = "count")

# Visualizing quadgrams
g <- ggplot(df.quad[1:50,],
            aes(x = reorder(term, -count), y  = count))
g <- g + geom_bar(stat = "identity")
g <- g + labs(x = "QuadGrams", y = "Frequency",
              title = "Frequency of top quadgrams")
g <- g + theme(axis.text.x = element_text(angle = 90))
g

qplot(log(count),
      data = df.quad, bins = 50,
      main = "Histogram of frequencies of quadgrams",
      xlab = "Frequency",
      ylab = "count")


write.csv(df.uni, "./WordPrediction/unigram.csv")
df.uni <- read.csv("./WordPrediction/unigram.csv", stringsAsFactors = F)
saveRDS(df.uni, "./WordPrediction/unigram.rds")

write.csv(df.bi, "./WordPrediction/bigram.csv")
df.bi <- read.csv("./WordPrediction/bigram.csv", stringsAsFactors = F)
saveRDS(df.bi, "./WordPrediction/bigram.rds")

write.csv(df.tri, "./WordPrediction/trigram.csv")
df.tri <- read.csv("./WordPrediction/trigram.csv", stringsAsFactors = F)
saveRDS(df.tri, "./WordPrediction/trigram.rds")

write.csv(df.quad, "./WordPrediction/quadgram.csv")
df.quad <- read.csv("./WordPrediction/quadgram.csv", stringsAsFactors = F)
saveRDS(df.quad, "./WordPrediction/quadgram.rds")


# split <- function(x){
#   splitwords <- unlist(strsplit(x, " "))
#   len <- length(splitwords)
#   if(len == 2){
#     df.bi$w1 <- splitwords[1]
#     df.bi$w2 <- splitwords[2]
#   }
#   if(len == 3){
#     df.tri$w1 <- splitwords[1]
#     df.tri$w2 <- splitwords[2]
#     df.tri$w3 <- splitwords[3]
#   }
#   if(len == 4){
#     df.quad$w1 <- splitwords[1]
#     df.quad$w2 <- splitwords[2]
#     df.quad$w3 <- splitwords[3]
#     df.quad$w4 <- splitwords[4]
#   }
# }




#predict.function <- function(x){
  
  input <- removePunctuation(x)
  input <- stripWhitespace(removeNumbers(tolower(input)))
  input <- strsplit(input, " ")[[1]]
  input.len <- length(input)
  
  if(input.len >= 3)
  {
    get.index <- which(input[input.len-2] == df.quad$w1 &
                         input[input.len-1] == df.quad$w2 &
                         input[input.len] == df.quad$w3)
    if(length(get.index) != 0)
    {
      pred.df <- df.quad[get.index, ]
      pred.df <- pred.df[order(-pred.df$count),]
      pred.word <- pred.df$w4[1]
      pred.by <- 4
    }
    else{
      get.index <- which(input[input.len-1] == df.tri$w1 &
                           input[input.len] == df.tri$w2)
      if(length(get.index) != 0){
        pred.df <- df.tri[get.index,]
        pred.df <- pred.df[order(-pred.df$count), ]
        pred.word <- pred.df$w3[1]
        pred.by <- 3
      }
      else{
        get.index <- which(input[input.len] == df.bi$w1)
        if(length(get.index) != 0){
          pred.df <- df.bi[get.index, ]
          pred.df <- pred.df[order(-pred.df$count),]
          pred.word <- pred.df$w2[1]
          pred.by <- 2
        }
        else{
          pred.word <- df.uni$term[1]
          pred.by <- 1
        }
      }
    }
  }
  else{
    check <- 0
    if(input.len == 1){
      get.index <- which(input[input.len] == df.bi$w1)
      if(length(get.index) != 0){
        pred.df <- df.bi[get.index, ]
        pred.df <- pred.df[order(-pred.df$count),]
        pred.word <- pred.df$w2[1]
        pred.by <- 2
        check <- 1
      }
    }
    if(input.len == 2)
    {
      get.index <- which(input[input.len-1] == df.tri$w1 &
                           input[input.len] == df.tri$w2)
      if(length(get.index) != 0){
        pred.df <- df.tri[get.index,]
        pred.df <- pred.df[order(-pred.df$count), ]
        pred.word <- pred.df$w3[1]
        pred.by <- 3
        check <- 1
      }
      else{
        get.index <- which(input[input.len] == df.bi$w1)
        if(length(get.index) != 0){
          pred.df <- df.bi[get.index, ]
          pred.df <- pred.df[order(-pred.df$count),]
          pred.word <- pred.df$w2[1]
          pred.by <- 2
          check <- 1
        }
      }
    }
    if(check == 0){
      pred.word <- df.uni$term[1]
      pred.by <- 1
    }
  }
  if(input.len == 0)
  {
    pred.word <- "NULL"
    pred.by <- "NULL"
  }
  if(pred.by == 1){
    pred.by <- "next word is predicted by Unigram"
  }
  if(pred.by == 2){
    pred.by <- "next word is predicted by Biigram"
  }
  if(pred.by == 3){
    pred.by <- "next word is predicted by Triigram"
  }
  if(pred.by == 4){
    pred.by <- "next word is predicted by Quadgram"
  }
  result <- c(pred.word, pred.by)
  return(result)
}

#result <- predict.function("Mother's")


































