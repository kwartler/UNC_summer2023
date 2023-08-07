# UNC_summer2023

## Tentative Schedule

| Time                   | Class                       | Topics                       |  Optional Assignments/Reading                     |  
|------------------------|-----------------------------|------------------------------|---------------------------------------------------|
| 9:30-12;12:45-3:45     | Aug14 Mon                   | NLP Intro, R Basics          | Chapter 1                                         |  
| 9:30-12;12:45-3:45     | Aug15 Tue                   | BOW, DTM, Visuals            |  “Homework” HW1-Basics of R Coding Chapter 2, 3   |   
| 9:30-12;12:45-3:45     | Aug16 Wed                   | Basic Doc classification     |  "Homework" HW2 Load, clean documents & frequency |   
| 9:30-12;12:45-3:45     | Aug17 Thu                   | Sentiment, APIs, GPT         |                                                   | 
| 4-6pm                  | Aug17 Thu (optional lab)    | Bring your own text!         |                                                   |  
| 9:30-12;12:45-3:45     | Aug18 Fri                   | UDPipe, OpenNLP, NLP Ethics  | "Homework" HW3 Create a document classifier       |  

# R installation
[base-R](https://cran.r-project.org/)

# R studio
[r-studio](https://posit.co/download/rstudio-desktop/)

# Installing Git
[Windows Installation](https://git-scm.com/download/win)
Mac Installation:
 - [Brew](https://brew.sh/); follow terminal instructions
 - [Brew install git](https://git-scm.com/download/mac); copy installation command into terminal

## R Packages to install

```
# Easiest Method to run in your console
install.packages('pacman')
pacman::p_load(caret,cld2,clue,cluster,dplyr,dygraphs,echarts4r,fairness,ggplot2,ggthemes,ggwordcloud,glmnet,googleLanguageR,httr,hunspell,
jsonlite,kmed,lda,LDAvis,leaflet,lexicon,lsa,lubridate,mapproj,maps,mgsub,MLmetrics,ModelMetrics,openNLP,plotrix,plyr,pROC,
qdap,radarchart,RColorBrewer,reshape2,rvest,SentimentAnalysis,sentimentr,skmeans,spelling,stringi,stringr,
tesseract,text2vec,textcat,textdata,tidyr,tidytext,tm,treemap,udpipe,vtreat,wordcloud,wordcloud2,xts,yardstick,zoo)

# You can install packages individually such as below if pacman fails.
install.packages('tm')

# Or using base functions use a nested `c()`
install.packages(c("lda", "LDAvis", "treemap"))

```

```
# There is one additional package we will install on day 1 from a local .tar.gz file.
# Try this code with an updated path to your own copy of the file
install.packages("~/Desktop/GSERM_ICPSR/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")

# Or you can try the original datacube for it, though it often fails.
install.packages('openNLPmodels.en', repo= 'http://datacube.wu.ac.at/')
```

*some students, particularly on mac, have issues installing `qdap`.  This is due to java installation problems.  If that is the case, just remove it from the above code and we can work around it.*
