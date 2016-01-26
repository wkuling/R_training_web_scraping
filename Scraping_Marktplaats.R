# Cleanup
rm(list=ls())

# Load all necessary libraries
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)
library(tm)
library(data.table)
library(dplyr)
library(tidyr)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(xgboost)
library(Ckmeans.1d.dp)
library(taucharts)

# Global variable
if(Sys.getenv("Corp_key") == "HP73FQ") {
  woonplaatsen <- read.csv2("~/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))
  # Just in case there are proxy problems
  Sys.setenv(http_proxy="")
  Sys.setenv(https_proxy="")
  Sys.unsetenv("http_proxy")
  Sys.setenv(no_proxy="*")
  } else {
  woonplaatsen <- read.csv2("/Users/wendellkuling/Repos/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))
}

# Function definitions
scraper <- function(url) {
  # Function that scrapes all pages starting from URL with currentpage = 1 to last-page
  # Args: URL from Marktplaats, ending on 'currentPage=' (to be scraped)
  # Returns: dataframe with observations from all pages from URL to last-page
  webpage <- read_html(url)
  output <- data.frame()  
  
  numberpages <- webpage %>%
    html_nodes('a:nth-child(13)') %>%
    html_text() %>%
    as.numeric()

  for (pagenumber in 1:numberpages) {
    
    url_to_use <- gsub('currentPage=1', paste0('currentPage=',pagenumber), url)
    
    print(url_to_use)
    
    webpage <- read_html(url_to_use)
  
    allfields <- webpage %>%
      html_nodes('.column-location .location-name , .price-and-thumb-container .ellipsis , .group-1 .mp-listing-attributes , .group-1 .mp-listing-title , .group-0 .mp-listing-attributes , .group-0 .mp-listing-title') %>%
      html_text()
    
    allDF <- data.frame(value = allfields,stringsAsFactors = F)  %>%
      mutate(type = ifelse((nchar(value)==4) & (grepl("\\d{4}",value)), "jaartal", NA)) %>%
      mutate(type = ifelse((substr(value, start = nchar(value)-1, stop = nchar(value)) == "km" &
                              nchar(value) <= 12 &
                              grepl("\\d", value) == T), "kilometrage", type)) %>%
      mutate(type = ifelse((str_count(value, "\n") == 2) & (grepl("€", value) | grepl("?,??", value)), "prijs", type))  %>%
      mutate(type = ifelse(is.na(type) & lapply(strsplit(value, ","), function(x) x[[1]]) %in% woonplaatsen$Naam, "plaats", type)) %>%
      mutate(type = ifelse(is.na(type), "beschrijving", type))
    
    output <- rbind(output, allDF)
  }  
  
  # Groeperen van advertenties
    
  output_wide <- output %>%
    mutate(adnr = ifelse(type == "beschrijving", 1, 0)) %>%
    mutate(adnr = cumsum(adnr)) %>%
    group_by(adnr) %>%
    spread(type, value)

  return(output_wide)
}

cleaner <- function(DF) {
  # Function that cleans 'raw' dataframe to dataframe with right format
  # Args: DF: scraped dataframe in 'raw' format with columns 'prijs', jaartal', 'kilometrage', 'plaats' and 'beschrijving'
  # Returns: cleaned dataframe (unchanged names, only format changed)
  # Note: all rows which contain 'NA' values are removed by this function
  
  DF$prijs <- DF$prijs %>%
    gsub("[^0-9,]",'', .) %>% 
    gsub(",", ".", .) %>% 
    as.numeric()

  DF$jaartal <- DF$jaartal %>%
    as.numeric()
  
  DF$kilometrage <- DF$kilometrage %>%
    gsub("[^0-9]",'', .) %>% 
    as.numeric()
  
  DF$plaats <- DF$plaats %>%
    strsplit(",") %>%
    lapply(function(x) x[[1]])
  
  DF$beschrijving <- DF$beschrijving %>%
    tolower()
  
  DF <- DF[rowSums(is.na(DF)) == 0, ] %>%
    filter(!beschrijving %like% "maand") %>%
    filter(prijs > 500)
  
return(DF)
}  

bow <- function(data, top=50) {
# return dataframe which includes bag-of-words (top tokens of beschrijving)
# TODO: document function ;)
  
  # Get descriptions
  d = data$beschrijving
  # Load the data as a corpus
  docs <- Corpus(VectorSource(d))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("het", "een","van","bij","voor","met","binnen","o.a.","de")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Build a matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # Select top 50 occuring words
  d <- d[1:top,]
  # Create list of words
  words <- d$word
  
  # Add columns for each word
  for(word in words) {
    data[word] <- ifelse(tolower(data$beschrijving) %like% word,1,0)
  }
return(data)
}


# Main code

demourl <- 'http://www.marktplaats.nl/z/auto-s/bmw/3-serie.html?categoryId=96&attributes=model%2C3-Serie&currentPage=1'

df <- demourl %>% 
  scraper() %>% 
  cleaner() %>%
  bow()


# Splitting to train and test set
idx = sample(nrow(df),floor(nrow(df)*.2))
test = df[idx,]
train = df[-idx,]

ytest = test$prijs
ytrain = train$prijs

test <- test %>% select(-adnr, -prijs, -beschrijving, -plaats)
train <- train %>% select(-adnr, -prijs, -beschrijving, -plaats)

trainMatrix <- train %>% as.matrix
testMatrix <- test %>% as.matrix

# Let's start by estimating a XGBoost model
  
param <- list(booster = "gbtree", objective = "reg:linear", 
              max.depth = 25, eta = 0.1, nthread = 2, nround = 2,  
              min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8,num_parallel_tree = 1)
  
bst = xgboost(param=param, data = trainMatrix, label = ytrain, nrounds=200)

names <- dimnames(trainMatrix)[[2]]

### Compute feature importance matrix
# importance_matrix <- xgb.importance(names, model = bst)

### Nice graph
# xgb.plot.importance(importance_matrix[1:10,])

pred <- predict(bst, testMatrix)

### Plotting the predictions versus actuals
comparedf <- data.frame(pred = pred, act = ytest)

ggplot(comparedf, aes(x=act, y=pred)) + geom_point(shape=1) + geom_smooth(method=lm)

RMSE <- sqrt(mean((ytest-pred)^2))

print(RMSE)



plotdata <- df[idx,]
plotdata$pred <- pred
plotdata$loot <- round((plotdata$pred - plotdata$prijs),-2)

tauchart(plotdata) %>%
  tau_point("prijs","pred") %>%
  tau_tooltip(c("loot", "beschrijving", "jaartal", "kilometrage", "prijs", "plaats")) %>%
  tau_color_538(n=6)




      #d$word <- as.character(d$word) 
      #set.seed(1234)
      #suppressWarnings(wordcloud(words = d$word, freq = d$freq, min.freq = 1,
      #      max.words=200, random.order=FALSE, rot.per=0.35,
      #      colors=brewer.pal(8,"Set1")))




