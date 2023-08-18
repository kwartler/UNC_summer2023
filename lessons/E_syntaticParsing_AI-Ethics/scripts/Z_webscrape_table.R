#' Title: Example for a table
#' Purpose: Scrape a table of information
#' Author: Ted Kwartler
#' Date: Aug 17, 2023

pg <- 'https://338canada.com/alberta/polls.htm'
pageData <- pg %>% read_html(pg) %>%
  html_nodes('.hideifmobile') %>%
  html_table()

# Examine what we got
str(pageData)

# This container had an empty table so we select the second one
pageData <- as.data.frame(pageData[[2]])

# End
