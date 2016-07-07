
#set directory and import libraries
setwd("/Users/martijncligge/Documents/R tutorial") 

library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(car)

#import data sets and combine data sets 
whitewine <- read.csv("winequality-white.csv", header = TRUE, sep = ';' )
whitewine$colour <- 'red'
redwine <- read.csv("winequality-red.csv", header = TRUE, sep = ';')
redwine$colour <- 'white'
wine <- rbind.data.frame(whitewine,redwine )

#filtering out outliers
wine <- subset.data.frame(wine, wine$residual.sugar < 50)
wine <- subset.data.frame(wine, wine$free.sulfur.dioxide < 200)
wine <- subset.data.frame(wine, wine$density < 1.01)
wine <- subset.data.frame(wine, wine$chlorides < 0.5)
wine <- subset.data.frame(wine, wine$citric.acid < 1.5)



names <- colnames(wine[1:12])

#normalize scores
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
normwine <- as.data.frame(lapply(wine[1:11], normalize))

normwine <- cbind.data.frame(normwine,wine$quality )
set.seed(1234)
names(normwine) <- names

# cluster quality training set
FactQ <- as.factor(normwine$quality)
normwine<- cbind(normwine, FactQ)
temp <- recode(normwine$FactQ, "c('3','4','5')='10'; c('6')='20'; else='40'")
Ptemp <- recode(temp, "c('10')='5'; ('20')='6'; else='7'")
normwine$FactQ <- Ptemp


ind <- sample(2, nrow(wine), replace=TRUE, prob=c(0.75, 0.25))
normwine.training <- normwine[ind==1, 1:13]
normwine.test <- normwine[ind==2, 1:13]
normwine.testlabels <- normwine[ind==2, 13]


# make decision tree
# fit <- rpart(FactQ ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
#               free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol 
#             , data=normwine.training, method="class",  control=rpart.control(cp=0.008))

#fancyRpartPlot(fit)
# make random forest 
fitrandom <- randomForest(FactQ  ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                            free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol 
                          , data=normwine.training,importance=TRUE, ntree=3000)


varImpPlot(fitrandom)



#calculate prediction decision tree
#Prediction <- predict(fit, normwine.test, type = "class")

#calculate prediction random forest tree
Predictionrandom <- predict(fitrandom, normwine.test, type = "class")

# MSE decision tree
#sqrt( sum( (as.integer(as.character(normwine.testlabels)) - as.integer(as.character(Prediction)))^2 , na.rm = TRUE ) / 1599)

#(as.integer(as.character(normwine.testlabels)) - as.integer(as.character(Prediction)))^2

# MSE random forest
sqrt( sum( (as.integer(as.character(normwine.testlabels)) - as.integer(as.character(Predictionrandom)))^2 , na.rm = TRUE ) / 1599)

(as.integer(as.character(normwine.testlabels)) - as.integer(as.character(Predictionrandom)))^2
