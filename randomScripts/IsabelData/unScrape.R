#' UN
#' TK
#' Aug 17, 2023
#'

# Library
library(rvest)

#I Found 29 pages with information
# https://www.unfpa.org/search/content?search_api_fulltext=demographic%20dividend&page=29

# Construct all URLS
allUN <- paste0('https://www.unfpa.org/search/content?search_api_fulltext=demographic%20dividend&page=',1:29)

# Get all URLS that are press
allRelevantResults <- list()
for(i in 1:length(allUN)){
  print(i)
  pg <- read_html(allUN[i])

  # Results URLS
  unResults <- pg %>% html_nodes('.views-field-title')%>%
    html_nodes('a') %>% html_attr('href')
  allRelevantResults[[i]] <- unResults
}

# Complete & save the URLS
allRelevantVector         <- unlist(allRelevantResults)
allRelevantVectorComplete <- paste0('https://www.unfpa.org',
                                    allRelevantVector)
writeLines(allRelevantVector,  '~/Desktop/UNC_summer2023/randomScripts/IsabelData/UN_searchResultsURLS.txt')

# Looks like there are press, news and event  etc. pages.
# Each needs a different scrape
pageType <- strsplit(allRelevantVector, '/')
pageType <- unlist(lapply(pageType, '[',2))

unique(pageType)

# Put info together
pgVecDF <- data.frame(urlPg = allRelevantVectorComplete,
                      pageType = pageType)

# If I had more time I would write custom functions but we can just do this in a loop
# Loop over each row in the data frame
dateList <- list()
for (i in 1:nrow(pgVecDF)) {
  print(i)
  oneURL <- pgVecDF$urlPg[i]
  pageType <- pgVecDF$pageType[i]

  # Perform different web scraping based on the page type
  switch(pageType,
         "events" = { #done
           tmp <- read_html(oneURL)
           eventDate <- tmp %>% html_nodes('.gp-header__content') %>% html_text()
           if(length(eventDate)==0){
             warning(paste(oneURL, 'is a world humanitarian day, no date?'))
             finalDate <- 'world humanitarian day has no clear date?'
           } else {
             eventDate <- capture.output(cat(eventDate))
             eventDateIDX <- grepl(paste(month.name, collapse = '|'),eventDate)
             finalDate <- trimws(eventDate[eventDateIDX])
           }

         },
         "news" = { #done
           tmp <- read_html(oneURL)
           newsDate <- tmp %>% html_nodes('.news-header__content') %>% html_text()
           newsDate <- capture.output(cat(newsDate))
           newsDateIDX <- grepl(paste(month.name, collapse = '|'),newsDate)
           if(sum(newsDateIDX)==0){
             newsDateIDX <- grepl(paste(month.abb, collapse = '|'),newsDate)
           }
           finalDate <- trimws(newsDate[newsDateIDX])
         },
         "press" = { #done
           tmp <- read_html(oneURL)
           pressDate <- tmp %>% html_nodes('.news-header__content') %>% html_text()
           pressDate <- capture.output(cat(pressDate))
           pressDateIDX <- grepl(paste(month.name, collapse = '|'),pressDate)
           if(sum(pressDateIDX)==0){
             pressDateIDX <- grepl(paste(month.abb, collapse = '|'),pressDate)
           }
           finalDate <- trimws(pressDate[pressDateIDX])
         },
         "demographic-divident" = { #done
           finalDate <- '404 page result!'
           warning(paste('there is a 404 page not found at', oneURL))
         },
         "demographic-dividend-atlas" = { #done
           finalDate <- 'just a jpg without dates'
           warning(paste(oneURL, 'is just a banner jpg of demographic dividend atlas'))
         },
         "publications" = { #done
           tmp <- read_html(oneURL)
           resourceDate <- tmp %>% html_nodes('.publication-header__content') %>% html_text()
           resourceDate <- capture.output(cat(resourceDate))
           resourceIDX  <- grep('Publication date:',resourceDate)
           finalDate    <- trimws(resourceDate[resourceIDX])
           warning(paste('there is a publication url at ',
                         oneURL,
                         'you should download the pdf or add download.file() to the script'))
         },
         "evaluation-unfpa-support-population-dynamics-and-data" = { #done
           tmp        <- read_html(oneURL)
           tmpDate    <- tmp %>% html_nodes('.contentrow')%>% html_text()
           tmpDate    <- capture.output(cat(tmpDate))
           tmpDateIDX <- grepl("\\b(19|20)\\d{2}\\b", tmpDate)
           finalDate  <-trimws(tmpDate[tmpDateIDX])

         },
         "resources" = { #done
           tmp <- read_html(oneURL)
           resourceDate <- tmp %>% html_nodes('.publication-header__content') %>% html_text()
           resourceDate <- capture.output(cat(resourceDate))
           resourceIDX  <- grepl("\\b(19|20)\\d{2}\\b", resourceDate)
           finalDate    <- trimws(resourceDate[resourceIDX][1])
           },
         "updates" = { #done
           tmp <- read_html(oneURL)
           updateDate <- tmp %>% html_nodes('p') %>% html_text()
           updateDateIDX <- grepl(paste(c(month.name,month.abb), collapse = '|'), updateDate)
           updateDate <- updateDate[updateDateIDX][1]
           finalDate <- trimws(updateDate)
         },
         "donor" = { #done
           finalDate <- 'donor page mentioning demo-dividend, no date'
           warning(paste(oneURL, 'is a donor page so, no date!'))
         },
         "data" = { #done
           finalDate <- 'data aggregation page, no date'
           warning(paste(oneURL, 'is data page without a single publish date.'))
         },
         "sdg" = { #done
           finalDate <- 'decade of action page, no date'
           warning(paste(oneURL, 'is a Decade of Action page without dates.'))
         }

  )
  if(length(finalDate)==0){
    print('issue identified')
    print(i)
    print(oneURL)
  }
  response <- data.frame(oneURL, pageType, finalDate)
  dateList[[i]] <- response
}

# All info
allInfo <- do.call(rbind, dateList)

# Now let's write a loop to get each page content
saveIndividualFiles <- T
allRelevantText     <- list()
for(i in 1:nrow(allInfo)){
  print(i)
  onePg <- allInfo$oneURL[i]
  tmpPG <- read_html(onePg)

  tmp <- tmpPG %>% html_nodes('.page-content') %>% html_text()

  # Initially we can cutoff anything after "Related Content"
  rawTxt <- capture.output(cat(tmp))
  rawTxt <- trimws(rawTxt)
  cutoffLine <- grep('\\bRelated Content\\b', rawTxt)
  if(length(cutoffLine) !=0){
    rawTxt <- trimws(paste(rawTxt[1:cutoffLine-1], collapse = ' '))
    rawTxt <- gsub("<.*?>", "", rawTxt) #text inside <...>
    rawTxt <- gsub("\\{.*?\\}", "", rawTxt) # text inside {...}
    txt    <- rawTxt
  } else {
    rawTxt <- gsub("<.*?>", "", rawTxt)
    rawTxt <- gsub("\\{.*?\\}", "", rawTxt)
    txt    <- paste(rawTxt, collapse = ' ')
  }

  # Now that it's collapsed, remove legacy javascript functions
  txt <- gsub("<!--.*?//-->\\s*", "", txt)
  txt <- gsub("-->\\s*", "", txt)

  response <- data.frame(url    = onePg[i],
                         pgType = allInfo$pageType[i],
                         date   = allInfo$finalDate[i],
                         text   = txt)
  allRelevantText[[i]] <- response
  if(saveIndividualFiles==T){
    fileName <- tail(unlist(strsplit(allRelevantVector[i], '/')),1)
    nam <- paste0('~/Desktop/UNC_summer2023/randomScripts/IsabelData/updatedIndividualFiles/',
                  make.names(fileName),'.csv')
    write.csv(response, nam, row.names = F)
  }
}

# Organize & Save
allRelevantTextDF <- do.call(rbind, allRelevantText)

write.csv(allRelevantTextDF,
          '~/Desktop/UNC_summer2023/randomScripts/IsabelData/FINAL_allRelevantTextDF.csv',
          row.names = F)


# End
