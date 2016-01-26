# Load all necessary libraries

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)

testurl <- 'http://www.marktplaats.nl/z.html?categoryId=96&attributes=S%2C610&attributes=S%2C10882&searchOnTitleAndDescription=true&postcode=4207PV&startDateFrom=ALWAYS&lastPage=l1Cars'

beamers <- read_html(testurl)

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

allfields <- beamers %>%
  html_nodes('.column-location .location-name , .price-and-thumb-container .ellipsis , .group-1 .mp-listing-attributes , .group-1 .mp-listing-title , .group-0 .mp-listing-attributes , .group-0 .mp-listing-title') %>%
  html_text()
  # gsub("[^0-9]",'', .) %>% 
  # as.numeric()

woonplaatsen <- read.csv2("/Users/wendellkuling/Repos/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))

allDF <- data.frame(value = allfields,stringsAsFactors = F)  %>%
  mutate(type = ifelse((nchar(value)==4) & (grepl("\\d{4}",value)), "jaartal", NA)) %>%
  mutate(type = ifelse((substr(value, start = nchar(value)-1, stop = nchar(value)) == "km" &
                          nchar(value) <= 12 &
                          grepl("\\d", value) == T), "kilometrage", type)) %>%
  mutate(type = ifelse((str_count(value, "\n") == 2) & grepl("€", value), "prijs", type))  %>%
  mutate(type = ifelse(is.na(type) & lapply(strsplit(value, ","), function(x) x[[1]]) %in% woonplaatsen$Naam, "plaats", type)) %>%
  mutate(type = ifelse(is.na(type), "beschrijving", type))

# Groeperen van advertenties

allDF_wide <- allDF %>%
  mutate(adnr = ifelse(type == "beschrijving", 1, 0)) %>%
  mutate(adnr = cumsum(adnr)) %>%
  group_by(adnr) %>%
  spread(type, value)

# TODO: setting proper format and types

