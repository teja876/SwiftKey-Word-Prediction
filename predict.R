predict.function <- function(x){
  
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
    pred.by <- "next word is predicted by Bigram"
  }
  if(pred.by == 3){
    pred.by <- "next word is predicted by Trigram"
  }
  if(pred.by == 4){
    pred.by <- "next word is predicted by Quadgram"
  }
  result <- c(pred.word, pred.by)
  return(result)
}
