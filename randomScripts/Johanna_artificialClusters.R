# TK
# Articifial Cluster Examination
# Aug 17

# This is a toy data set, so we set a parameter to randomize
# the artificial groups or not.
rando <- T

# Libs
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(irlba)
library(factoextra)
library(ggthemes)
library(Matrix)

# Stopwords
stops  <- c(stopwords('SMART'), 'amp')

# Custom Functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}


# Data
coffee     <- read.csv('https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/A_Setup_Intro_Basics/data/coffeeVector.csv')
chardonnay <- read.csv('https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/B_Basic_Visuals/data/chardonnay.csv')
beer       <- read.csv('https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/B_Basic_Visuals/HW/beer.csv')

# Combine to make it like a mixed data set like usual
# Use artificial clusters as an example
allTxt <- c(coffee$x, chardonnay$text, beer$text)

# Set the artificial groups
if(rando == T){
  grp = sample(letters[1:3], length(allTxt), replace = T)
  } else {
    grp = c(rep('1',1000),rep('2',1000),rep('3',1000))
}

allTxt <- data.frame(text = allTxt, grp = grp)
head(allTxt)

# Enforce encoding
allTxt$text <- stringi::stri_encode(allTxt$text, "", "UTF-8")

# Let's create a skmeans model
# Apply tm functions
txtCorp <- VCorpus(VectorSource(allTxt$text))
txtCorp <- cleanCorpus(txtCorp, stops)
txtCorpDTM <- DocumentTermMatrix(txtCorp)

# You can't have blank documents which can occur sometimes so let's check
drops <- slam::row_sums(txtCorpDTM)==0
sum(drops)

# If there are blanks you have to drop them; you may have to use as.matrix() first depending on the package version
txtCorpDTM <- txtCorpDTM[!drops,]

# Apply Spherical K-Means
nClusters <- 3
txtSKMeans <- skmeans(x = txtCorpDTM,
                      nClusters,
                      m = 1,
                      control = list(nruns = 5, verbose = T))

# Maybe we look across skmeans and artifical to see alignment
# IF rando==T then this will show no lift and lots of overlap
# skmean clusters are rows, columns are artifical groups
table(txtSKMeans$cluster, allTxt$grp)
round(prop.table(table(txtSKMeans$cluster, allTxt$grp)),2)


# Get PCA of the original text DTM
txtCorpPCA <- prcomp_irlba(as.matrix(txtCorpDTM),
                           n = 2,
                           center = TRUE, scale. = TRUE)

# Append the approximated PCA results and skmeans cluster assignments
plotDF         <- as.data.frame(txtCorpPCA$x)
names(plotDF)  <- c("PC1", "PC2")
plotDF$cluster <- txtSKMeans$cluster


# The differences in plots are small but noticeable.
# SKMEANS - cluster assignment
fviz_cluster(list(data = plotDF[,1:2], cluster = plotDF$cluster),
             ellipse.type = "confidence", geom= 'point') +
  theme_gdocs()

# ARTIFICIAL - cluster assignment
fviz_cluster(list(data = plotDF[,1:2], cluster = allTxt$grp),
             ellipse.type = "confidence", geom= 'point') +
  theme_gdocs()

# Now let's examine sihouette plots
# SKMEANS silhouette plot
sk <- silhouette(txtSKMeans)
plot(sk, col=1:3, border=NA)
dev.off()

# ARTIFICIAL silhouette plot
methods('silhouette') #classes it accepts

# Try to overwrite the cluster assignments inside the object
fakeSKMeans <- txtSKMeans
fakeSKMeans$cluster <- allTxt$grp
sk <- silhouette(fakeSKMeans)
plot(sk, col=1:3, border=NA)

### As expected the silhouettes actually are negative,
# and very shallow when rando==T.
# This shows our artificial groups have little separation
# as if they are clusters from an unsupervised method.
# If rando==F, then you see good separation as expected

# Using artificial clusters, lets make a comparison cloud
protoTypical <- t(cl_prototypes(fakeSKMeans))
head(protoTypical)

# Here no matter rando T/F the big differences among word observations
comparison.cloud(protoTypical,
                 max.words=75,
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(protoTypical),"Dark2"),
                 scale=c(3,0.1))

# Now that we sub'ed in our fake clusters
# we can try some other methods in the object.
# The trick is that its on a very large matrix
# so could be hard to implement
# Within-Cluster Sum of Squares (WCSS) - Inside cluster cohesiveness
# Jaccard - Overlap of clusters
# Davies-Bouldin Index - Separation of clusters
#

# End
