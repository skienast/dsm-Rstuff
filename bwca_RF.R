#Install and load packages
#install.packages("raster",dependencies=T)
#install.packages("randomForest",dependencies=T)
#install.packages("ff", dependencies=T)
library(raster)
library(randomForest)

#set working directory
setwd("E:/BWCA/modeling/bwca_whole/randomforest")

#set up dataset and read in data
data = "bwca_pedons_R_6class.csv"
d6<-read.csv(data,header=T)
str(d6)
head(d6)

#set class column to a factor and check class values
d6$class<-as.factor(d6$class)
str(d6)
levels(d6$class)

##run random forest on d6 training data
d6.rf<-randomForest(class ~ ., data=d6, ntree=1000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d6.rf
#look at variable importance plot
varImpPlot(d6.rf)
#look at variable importance matrix per class
importance(d6.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d6.rf)
#change number of trees based on plot resuts
d6.rf<-randomForest(class ~ ., data=d6, ntree=5000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#change number of trees based on plot resuts
d6.rf<-randomForest(class ~ ., data=d6, ntree=3000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#change number of trees based on plot resuts
d6.rf<-randomForest(class ~ ., data=d6, ntree=4000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run again with optimal number of trees and top variables as reported in var importance
d6.rf<-randomForest(class ~ bwca_bgw2 + landsat_B1 + dep_cst + bwca_bgw1 + relpos3, data=d6, ntree=4000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)

#run with original number of trees and variables, but limit sample size to smallest class size
d6.rf<-randomForest(class ~ ., data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with more trees and variables, but limit sample size to smallest class
d6.rf<-randomForest(class ~ ., data=d6, ntree=10000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with optimal number of trees 5000, limit sample size to smallest class
d6.rf<-randomForest(class ~ ., data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)

#run with optimal number of trees 5000, limit sample size to smallest class, top variables as reported in variable importance matrix
d6.rf<-randomForest(class ~ bwca_bgw2 + landsat_B1 + dep_cst + slp25 + relpos3 + bwca_pca6, data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with optimal number of trees 5000, limit sample size to smallest class, top variables as reported in variable importance plot gini
d6.rf<-randomForest(class ~ landsat_B1 + dep_cst + slp25 + relpos3 + slp25, data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with optimal number of trees 5000, limit sample size to smallest class, top variables as reported in variable importance plot MDA
d6.rf<-randomForest(class ~ landsat_B1 + dep_cst + bwca_bgw2 + relpos3 + bwca_bgw1, data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with optimal number of trees 5000, limit sample size to smallest class, top variables as reported in variable importance plot MDA and importance matrix (pca6)
d6.rf<-randomForest(class ~ landsat_B1 + dep_cst + bwca_bgw2 + relpos3 + bwca_bgw1 + bwca_pca6, data=d6, ntree=5000, sampsize = c(11,11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run with optimal number of trees 5000, top variables as reported in variable importance plot MDA
d6.rf<-randomForest(class ~ landsat_B1 + dep_cst + bwca_bgw2 + relpos3 + bwca_bgw1, data=d6, ntree=5000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)



##collapse class 1 and 2 and try RF with 5 classes
#assign d6 to a new variable d5 which will contain 5 classes
d5 <- d6
str(d5)
head(d5)

#combine classes 1 and 2 which were >.5 in the separability analysis 1=very shallow dry till 2=shallow dry till, combined class 50 with 48 observations
levels(d5$class)[which(levels(d5$class)== "1")] <- "50"
levels(d5$class)[which(levels(d5$class)== "2")] <- "50"
levels(d5$class)


#run random forest on d5 training data
d5.rf<-randomForest(class ~ ., data=d5, ntree=10000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d5.rf
#look at variable importance plot
varImpPlot(d5.rf)
#look at variable importance matrix per class
importance(d5.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d5.rf)
#run random forest on d5 training data, 30000 trees
d5.rf<-randomForest(class ~ ., data=d5, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d5 training data, 18000 trees
d5.rf<-randomForest(class ~ ., data=d5, ntree=18000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d5 training data, 30000 trees, limit sample size to smallest class
d5.rf<-randomForest(class ~ ., data=d5, ntree=30000, sampsize = c(11,11,11,11,11), importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d5 training data, 30000 trees
d5.rf<-randomForest(class ~ ., data=d5, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d5 training data, 30000 trees, variables selected from varimp plot
d5.rf<-randomForest(class ~ landsat_B1 + dep_cst + bwca_bgw2 + relpos3 + bwca_bgw1, data=d5, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)

##example of predicting classes in d5 and outputting probability surface for class 6
predict(d5.rf, newdata = rstack, '6', type = 'prob')



##try RF with original 11 class data plus canopy ht
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#run random forest on d11 training data
d11.rf<-randomForest(class ~ ., data=d11, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d11.rf
#look at variable importance plot
varImpPlot(d11.rf)
#look at variable importance matrix per class
importance(d11.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d11.rf)
#run random forest on d11 training data, 30000 trees, top six from varimp plot
d11.rf<-randomForest(class ~ slp25 + dep_cst + bwca_bgw2 + relpos13 + surf_ax + canopy_ht, data=d11, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data, 30000 trees, top four from varimp plot
d11.rf<-randomForest(class ~ slp25 + dep_cst + surf_ax + canopy_ht, data=d11, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data, 30000 trees, top ten from varimp plot
d11.rf<-randomForest(class ~ slp25 + dep_cst + bwca_bgw2 + relpos13 + surf_ax + canopy_ht + bwca_bgw2 + bwca_bgw1 + relpos3 + landsat_B1 + mincurv100, data=d11, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data, 30000 trees, top six from varimp plot, and sample size constraint
d11.rf<-randomForest(class ~ slp25 + dep_cst + bwca_bgw2 + relpos13 + surf_ax + canopy_ht, data=d11, ntree=30000, sampsize = c(8,8,8,8,8,8,8,8,8,8,8), importance=TRUE, proximity=TRUE, keep.forest=TRUE)


##try RF with 6 class data plus canopy ht
#set up dataset and read in data
data = "bwca_pedons_R_6class_canopyht.csv"
d6c<-read.csv(data,header=T)
str(d6c)
head(d6c)

#set class column to a factor and check class values
d6c$class<-as.factor(d6c$class)
str(d6c)
levels(d6c$class)

#run random forest on d6c training data
d6c.rf<-randomForest(class ~ ., data=d6c, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d6c.rf
#look at variable importance plot
varImpPlot(d6c.rf)
#look at variable importance matrix per class
importance(d6c.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d6c.rf)
#run random forest on d6c training data with top three variables from varimp plot
d6c.rf<-randomForest(class ~ canopy_ht + dep_cst + bwca_bgw2, data=d6c, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d6c training data with top eight variables from varimp plot
d6c.rf<-randomForest(class ~ canopy_ht + dep_cst + bwca_bgw2 +bwca_bgw1 + landsat_B1 + relpos3 + relpos13 + mincurv100, data=d6c, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d6c training data with top six variables from varimp plot
d6c.rf<-randomForest(class ~ canopy_ht + dep_cst + bwca_bgw2 +bwca_bgw1 + landsat_B1 + relpos3, data=d6c, ntree=30000, importance=TRUE, proximity=TRUE, keep.forest=TRUE)

