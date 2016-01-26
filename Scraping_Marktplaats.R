
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

# Just in case there are proxy problems
Sys.setenv(http_proxy="")
Sys.setenv(https_proxy="")
Sys.unsetenv("http_proxy")
Sys.setenv(no_proxy="*")

# Global variable
#woonplaatsen <- read.csv2("/Users/wendellkuling/Repos/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))
woonplaatsen <- read.csv2("~/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))


testurl <- 'http://www.marktplaats.nl/z/auto-s/bmw/3-serie.html?categoryId=96&attributes=model%2C3-Serie&currentPage=1'


# TODO: build loop that gets all the data

scraper <- function(url) {
  
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
      mutate(type = ifelse((str_count(value, "\n") == 2) & (grepl("â‚¬", value) | grepl("â,¬Â", value)), "prijs", type))  %>%
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
 
data <- scraper(testurl)

# TODO: setting proper format and types

prices <- beamers %>%
  html_nodes('.price-and-thumb-container .ellipsis') %>%
  html_text() %>%
  gsub("[^0-9,]",'', .) %>% 
  gsub(",", ".", .) %>% 
  as.numeric()

years <- beamers %>%
  html_nodes('.defaultSnippet .mp-listing-attributes:nth-child(1)') %>%
  html_text() %>%
  # gsub("[^0-9,]",'', .) %>% 
  # gsub(",", ".", .) %>% 
  as.numeric()

kms <- beamers %>%
  html_nodes('.mp-listing-attributes:nth-child(3)') %>%
  html_text() %>%
  gsub("[^0-9]",'', .) %>% 
  as.numeric()





library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

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
d <- d[1:50,]
# Create list of words
words <- d$word

# Add columns for each word
for(word in words) {
  data[word] <- ifelse(tolower(data$beschrijving) %like% word,1,0)
}

      #d$word <- as.character(d$word) 
      #set.seed(1234)
      #suppressWarnings(wordcloud(words = d$word, freq = d$freq, min.freq = 1,
      #      max.words=200, random.order=FALSE, rot.per=0.35,
      #      colors=brewer.pal(8,"Set1")))





