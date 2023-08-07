# Author: TK
# date: Aug 7, 2023
# purpose: build some plots with diamonds data set

# libraries
library(ggplot2)
library(ggthemes)

# bring in my data
data("diamonds")

# basic EDA, look at top 6, structure, summary stats, and column names
names(diamonds)

# Pivot price by cut to get the average using aggregate()
priceByCut <- aggregate()


# Find the largest table diamond, max and which.max


# stop using a tibble and make it a data frame
class(diamonds)
diamonds <- as.data.frame(diamonds)

# tally by the color column


# quick and dirty barplot of the tally nesting table and barplot


# subset by clarity to be VVS2 which is clearest
plotDF

# ggplot to make a red distribution
ggplot(data = ____, aes(x = ____)) +
  geom_density(color = "___") +
  theme_few() +
  ggtitle('Clear Diamonds Price Distribution')

# Facet Wrap example with clarity~. formula
ggplot(data = ____, aes(x=____)) +
  geom_density(color = '___') +
  facet_wrap(___) +
  theme_few()

# Scatter plot - relationship between carat and price in a sample
smallerDiamondsIDX <- sample(1:nrow(diamonds), 20000)
ggplot(diamonds[smallerDiamondsIDX,], aes(x = carat, y = price)) +
  geom_point(color = 'black', alpha= 0.05) +
  geom_smooth(method = 'loess')

# Get the correlation of carat and price

# Double subset AND conditions
# clarity == 'VVS2'
# cut == 'Premium'
# color== 'J'
# carat >=1.2
smallDF <- ____(____,  ____ )

# Get the average price of this subset
mean(smallDF$price)
# End
