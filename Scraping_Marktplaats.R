# Load all necessary libraries
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)

# Global variables
woonplaatsen <- read.csv2("/Users/wendellkuling/Repos/R_training_web_scraping/Woonplaatsen_NL.csv",stringsAsFactors = F, col.names = c("Naam"))

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
      mutate(type = ifelse((str_count(value, "\n") == 2) & grepl("€", value), "prijs", type))  %>%
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
  # Returns: cleaned dataframe (unchanged names, only format changed) (beschrijving is unchanged)
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
  
  DF <- DF[rowSums(is.na(data)) == 0,]
  
return(DF)
}  

# Main code
 
demourl <- 'http://www.marktplaats.nl/z/auto-s/bmw/3-serie.html?categoryId=96&attributes=model%2C3-Serie&currentPage=1'

demourl %>% scraper() %>% cleaner()


