setwd("C:\\Users\\Martijn Cligge\\Documents\\School\\R") 


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

whitewine <- read.csv("winequality-white.csv", header = TRUE, sep = ';' )
whitewine$colour <- 'red'
redwine <- read.csv("winequality-red.csv", header = TRUE, sep = ';')
redwine$colour <- 'white'

wine <- rbind.data.frame(whitewine,redwine )

summary(wine)
names <- colnames(wine[1:12])

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
normwine <- as.data.frame(lapply(wine[1:11], normalize))

normwine <- cbind.data.frame(normwine,wine$quality )
set.seed(1234)
names(normwine) <- names


ind <- sample(2, nrow(wine), replace=TRUE, prob=c(0.75, 0.25))
normwine.training <- normwine[ind==1, 1:12]
normwine.test <- normwine[ind==2, 1:12]

fit <- rpart(as.factor(quality) ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                      free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol 
                    , data=normwine.training, method="class",  control=rpart.control(cp=0.0045))

fancyRpartPlot(fit)

Prediction <- predict(fit, normwine.test, type = "class")

submit <- data.frame( quality = Prediction)
write.csv(submit, file = "winetest.csv", row.names = FALSE)
