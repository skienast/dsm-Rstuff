#Install and load packages
#Install and load packages
install.packages("raster",dependencies=T)
#install.packages("randomForest",dependencies=T)
#install.packages("ff", dependencies=T)
library(raster)
library(rgdal)
library(randomForest)

#set working directory
setwd("E:/BWCA/modeling/bwca_whole/randomforest")


##try RF with original 11 class data, all variables, modeling each class separately##

##Class 32 = mod deep wet till & shallow wet till, 11 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)


#reassign class values to separate class 32 
levels(d11$class)[which(levels(d11$class)== "1")] <- "0"
levels(d11$class)[which(levels(d11$class)== "2")] <- "0"
levels(d11$class)[which(levels(d11$class)== "3")] <- "0"
levels(d11$class)[which(levels(d11$class)== "4")] <- "0"
levels(d11$class)[which(levels(d11$class)== "6")] <- "0"
levels(d11$class)[which(levels(d11$class)== "11")] <- "0"
levels(d11$class)[which(levels(d11$class)== "13")] <- "0"
levels(d11$class)[which(levels(d11$class)== "15")] <- "0"
levels(d11$class)[which(levels(d11$class)== "30")] <- "0"
levels(d11$class)[which(levels(d11$class)== "31")] <- "0"
levels(d11$class)

#run random forest on d11 training data
d32.rf<-randomForest(class ~ ., data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data and top 7 variables from varimp
#d32.rf<-randomForest(class ~ bwca_bgw1 + slp25 + bwca_bgw2 + bwca_ndvi + surf_ax + bwca_pca6 + landsat_B5, data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d32.rf
#look at variable importance plot
varImpPlot(d32.rf)
#look at variable importance matrix per class
importance(d32.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d32.rf)



##Class 31 = dry lacustrine & dry lacustrine, mantled, 8 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reassign class values to separate class 31 
levels(d11$class)[which(levels(d11$class)== "1")] <- "0"
levels(d11$class)[which(levels(d11$class)== "2")] <- "0"
levels(d11$class)[which(levels(d11$class)== "3")] <- "0"
levels(d11$class)[which(levels(d11$class)== "4")] <- "0"
levels(d11$class)[which(levels(d11$class)== "6")] <- "0"
levels(d11$class)[which(levels(d11$class)== "11")] <- "0"
levels(d11$class)[which(levels(d11$class)== "13")] <- "0"
levels(d11$class)[which(levels(d11$class)== "15")] <- "0"
levels(d11$class)[which(levels(d11$class)== "30")] <- "0"
levels(d11$class)[which(levels(d11$class)== "32")] <- "0"
levels(d11$class)

#run random forest on d11 training data
d31.rf<-randomForest(class ~ ., data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data and top 8 variables
#d31.rf<-randomForest(class ~ surf_ax + slp25 + relpos13 + dep_cst +mincurv100 + relpos3 + canopy_ht + bwca_bgw1, data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d31.rf
#look at variable importance plot
varImpPlot(d31.rf)
#look at variable importance matrix per class
importance(d31.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d31.rf)



##Class 30 = organics, 11 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reassign class values to separate class 30 
levels(d11$class)[which(levels(d11$class)== "1")] <- "0"
levels(d11$class)[which(levels(d11$class)== "2")] <- "0"
levels(d11$class)[which(levels(d11$class)== "3")] <- "0"
levels(d11$class)[which(levels(d11$class)== "4")] <- "0"
levels(d11$class)[which(levels(d11$class)== "6")] <- "0"
levels(d11$class)[which(levels(d11$class)== "11")] <- "0"
levels(d11$class)[which(levels(d11$class)== "13")] <- "0"
levels(d11$class)[which(levels(d11$class)== "15")] <- "0"
levels(d11$class)[which(levels(d11$class)== "31")] <- "0"
levels(d11$class)[which(levels(d11$class)== "32")] <- "0"
levels(d11$class)

#run random forest on d11 training data
#d30.rf<-randomForest(class ~ ., data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 4 variables from varimp
d30.rf<-randomForest(class ~ slp25 + surf_ax + dep_cst + bwca_ndvi, data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d30.rf
#look at variable importance plot
varImpPlot(d30.rf)
#look at variable importance matrix per class
importance(d30.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d30.rf)



##Class 15 = outwash, 8 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reassign class values to separate class 15 
levels(d11$class)[which(levels(d11$class)== "1")] <- "0"
levels(d11$class)[which(levels(d11$class)== "2")] <- "0"
levels(d11$class)[which(levels(d11$class)== "3")] <- "0"
levels(d11$class)[which(levels(d11$class)== "4")] <- "0"
levels(d11$class)[which(levels(d11$class)== "6")] <- "0"
levels(d11$class)[which(levels(d11$class)== "11")] <- "0"
levels(d11$class)[which(levels(d11$class)== "13")] <- "0"
levels(d11$class)[which(levels(d11$class)== "30")] <- "0"
levels(d11$class)[which(levels(d11$class)== "31")] <- "0"
levels(d11$class)[which(levels(d11$class)== "32")] <- "0"
levels(d11$class)

#run random forest on d11 training data
d15.rf<-randomForest(class ~ ., data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 6 variables from varimp
#d15.rf<-randomForest(class ~ slp25 + mincurv100 + bwca_bgw2 +bwca_bgw1 + landsat_B5 + surf_ax, data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d15.rf
#look at variable importance plot
varImpPlot(d15.rf)
#look at variable importance matrix per class
importance(d15.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d15.rf)



##Class 13 = wet lacustrine, mantled, 11 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reassign class values to separate class 13 
levels(d11$class)[which(levels(d11$class)== "1")] <- "0"
levels(d11$class)[which(levels(d11$class)== "2")] <- "0"
levels(d11$class)[which(levels(d11$class)== "3")] <- "0"
levels(d11$class)[which(levels(d11$class)== "4")] <- "0"
levels(d11$class)[which(levels(d11$class)== "6")] <- "0"
levels(d11$class)[which(levels(d11$class)== "11")] <- "0"
levels(d11$class)[which(levels(d11$class)== "15")] <- "0"
levels(d11$class)[which(levels(d11$class)== "30")] <- "0"
levels(d11$class)[which(levels(d11$class)== "31")] <- "0"
levels(d11$class)[which(levels(d11$class)== "32")] <- "0"
levels(d11$class)

#run random forest on d11 training data
#d13.rf<-randomForest(class ~ ., data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 5 variables from varimp
d13.rf<-randomForest(class ~ slp25 + relpos3 + dep_cst +relpos13 + surf_ax, data=d11, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d13.rf
#look at variable importance plot
varImpPlot(d13.rf)
#look at variable importance matrix per class
importance(d15.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d13.rf)




##Class 11 = wet lacustrine, 16 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '11', 11, 0)
str(new.class)
str(d11)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
d11.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 5 variables from varimp
#d11.rf<-randomForest(new.class ~ slp25 + relpos3 + dep_cst +relpos13 + surf_ax, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d11.rf
#look at variable importance plot
varImpPlot(d11.rf)
#look at variable importance matrix per class
importance(d11.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d11.rf)




##Class 6 = deep wet till, 24 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '6', 6, 0)
str(new.class)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
d6.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 10 variables from varimp
#d6.rf<-randomForest(new.class ~ slp25 + relpos3 + dep_cst + bwca_ndvi + surf_ax + landsat_B5 + landsat_B1 + relpos13 + bwca_bgw1 + bwca_bgw2, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d6.rf
#look at variable importance plot
varImpPlot(d6.rf)
#look at variable importance matrix per class
importance(d6.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d6.rf)




##Class 4 = deep dry till, 45 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '4', 4, 0)
str(new.class)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
d4.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 11 variables from varimp
#d4.rf<-randomForest(new.class ~ slp25 + dep_cst + surf_ax + landsat_B1 + relpos13 + bwca_bgw1 + bwca_bgw2 + relpos3 + landsat_B5 + mincurv100 + canopy_ht, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d4.rf
#look at variable importance plot
varImpPlot(d4.rf)
#look at variable importance matrix per class
importance(d4.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d4.rf)



##Class 3 = mod deep dry till, 25 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '3', 3, 0)
str(new.class)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
#d3.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top  9 variables from varimp
d3.rf<-randomForest(new.class ~ slp25 + landsat_B5 + surf_ax + landsat_B1 + bwca_bgw1 + bwca_bgw2 + dep_cst + relpos3 + bwca_pca6, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d3.rf
#look at variable importance plot
varImpPlot(d3.rf)
#look at variable importance matrix per class
importance(d3.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d3.rf)




##Class 2 = shallow dry till, 21 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '2', 2, 0)
str(new.class)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
d2.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 4 variables from varimp
#d2.rf<-randomForest(new.class ~ slp25 + landsat_B5 + surf_ax + bwca_bgw1, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d2.rf
#look at variable importance plot
varImpPlot(d2.rf)
#look at variable importance matrix per class
importance(d2.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d2.rf)



##Class 1 = very shallow dry till, 27 observations
#set up dataset and read in data
data = "bwca_pedons_R_11class_canopyht.csv"
d11<-read.csv(data,header=T)
str(d11)
head(d11)

#set class column to a factor and check class values
d11$class<-as.factor(d11$class)
str(d11)
levels(d11$class)

#reclassify
new.class <- ifelse(d11$class == '1', 1, 0)
str(new.class)

#delete class column append new var column
d11n <- cbind(d11[,-1], new.class)
str(d11n)
d11n$new.class<-as.factor(d11n$new.class)
str(d11n)
levels(d11n$new.class)

#run random forest on d11 training data
d1.rf<-randomForest(new.class ~ ., data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#run random forest on d11 training data with top 6 variables from varimp
#d1.rf<-randomForest(new.class ~ canopy_ht + landsat_B1 + surf_ax + bwca_bgw1 + bwca_bgw2 + dep_cst, data=d11n, ntree=500, importance=TRUE, proximity=TRUE, keep.forest=TRUE)
#call variable to see error and confusion matrix
d1.rf
#look at variable importance plot
varImpPlot(d1.rf)
#look at variable importance matrix per class
importance(d1.rf)
#look at plot for error vs number of trees to see where error levels out
plot(d1.rf)




##Predict single classes and probabilities##


#read in predictor raster layers - covariates
bwca_bgw1 <- raster("./covar_fire_extent/bwca_bgw1.img")
bwca_bgw2 <- raster("./covar_fire_extent/bwca_bgw2.img")
landsat_B1 <- raster("./covar_fire_extent/landsat_B1.img")
landsat_B5 <- raster("./covar_fire_extent/landsat_B5.img")
mincurv100 <- raster("./covar_fire_extent/mincurv100.img")
bwca_ndvi <- raster("./covar_fire_extent/bwca_ndvi.img")
bwca_pca6 <- raster("./covar_fire_extent/bwca_pca6.img")
dep_cst <- raster("./covar_fire_extent/dep_cst.img")
relpos13 <- raster("./covar_fire_extent/relpos13.img")
relpos3 <- raster("./covar_fire_extent/relpos3.img")
slp25 <- raster("./covar_fire_extent/slp25.img")
surf_ax <- raster("./covar_fire_extent/surf_ax.img")
canopy_ht <- raster("./covar_fire_extent/canopy_ht.img")

#stack covariate rasters and write to raster
bwca.covar <- brick (bwca_bgw1, bwca_bgw2, landsat_B1, landsat_B5, mincurv100, bwca_ndvi, bwca_pca6, dep_cst, relpos13, relpos3, slp25, surf_ax, canopy_ht)
writeRaster(bwca.covar, "./covar_fire_extent/covar_brick.img", progress = "text")

#remove uneeded variables from memory
rm(list=c("bwca_bgw1","bwca_bgw2","bwca_dep_cst","bwca_landsat_b1","bwca_landsat_b5","bwca_mincurv100","bwca_ndvi","bwca_pca6","bwca_relpos13","bwca_relpos3","bwca_slp25","bwca_surf_ax","canopy_ht","dep_cst","landsat_B1","landsat_B5","mincurv100","relpos13","relpos3","slp25","surf_ax","new.class","new.var")) 

#read in saved raster and name layers as in original brick
bwca.covar <- brick ("./covar_fire_extent/covar_brick.img")
names (bwca.covar) <- c("bwca_bgw1", "bwca_bgw2", "landsat_B1", "landsat_B5", "mincurv100", "bwca_ndvi", "bwca_pca6", "dep_cst", "relpos13", "relpos3", "slp25", "surf_ax", "canopy_ht")



#set chunk size (on Brook's computer with 32G RAM used 10G for each)
rasterOptions(chunksize = 10e+09, maxmemory = 10e+09)


#predict random forest class probability and classified layers
#write probability and class rasters
#class 1
prob_d1_rf <- predict(bwca.covar, d1.rf, '1', type="prob", index = 2, progress = "text") #index = 2 to get probability of presence in output
class_d1_rf <- predict(bwca.covar, d1.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d1_rf, "./predicted/class1_prob.img")
writeRaster(class_d1_rf, "./predicted/class1.img", NAflag=0)


#class 2
prob_d2_rf <- predict(bwca.covar, d2.rf, '2', type="prob", index = 2, progress = "text")
class_d2_rf <- predict(bwca.covar, d2.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d2_rf, "./predicted/class2_prob.img")
writeRaster(class_d2_rf, "./predicted/class2.img", NAflag=0)


#class 3
prob_d3_rf <- predict(bwca.covar, d3.rf, '3', type="prob", index = 2, progress = "text")
class_d3_rf <- predict(bwca.covar, d3.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d3_rf, "./predicted/class3_prob.img")
writeRaster(class_d3_rf, "./predicted/class3.img", NAflag=0)

    #remove uneeded variables from memory
    rm(list=c("prob_d1_rf","class_d1_rf","prob_d2_rf","class_d2_rf","prob_d3_rf","class_d3_rf") 
   
        gc() #garbage collection dumps hold on memory


#class 4
prob_d4_rf <- predict(bwca.covar, d4.rf, '4', type="prob", index = 2, progress = "text")
class_d4_rf <- predict(bwca.covar, d4.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d4_rf, "./predicted/class4_prob.img")
writeRaster(class_d4_rf, "./predicted/class4.img", NAflag=0)


#class 6
prob_d6_rf <- predict(bwca.covar, d6.rf, '6', type="prob", index = 2, progress = "text")
class_d6_rf <- predict(bwca.covar, d6.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d6_rf, "./predicted/class6_prob.img")
writeRaster(class_d6_rf, "./predicted/class6.img", NAflag=0)


#class 11
prob_d11_rf <- predict(bwca.covar, d11.rf, '11', type="prob", index = 2, progress = "text")
class_d11_rf <- predict(bwca.covar, d11.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d11_rf, "./predicted/class11_prob.img")
writeRaster(class_d11_rf, "./predicted/class11.img", NAflag=0)

    #remove uneeded variables from memory
    rm(list=c("prob_d4_rf","class_d4_rf","prob_d6_rf","class_d6_rf","prob_d11_rf","class_d11_rf") 
      
        gc() 

#class 13
prob_d13_rf <- predict(bwca.covar, d13.rf, '13', type="prob", index = 2, progress = "text")
class_d13_rf <- predict(bwca.covar, d13.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d13_rf, "./predicted/class13_prob.img")
writeRaster(class_d13_rf, "./predicted/class13.img", NAflag=0)


#class 15
prob_d15_rf <- predict(bwca.covar, d15.rf, '15', type="prob", index = 2, progress = "text")
class_d15_rf <- predict(bwca.covar, d15.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d15_rf, "./predicted/class15_prob.img")
writeRaster(class_d15_rf, "./predicted/class15.img", NAflag=0)


#class 30
prob_d30_rf <- predict(bwca.covar, d30.rf, '30', type="prob", index = 2, progress = "text")
class_d30_rf <- predict(bwca.covar, d30.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d30_rf, "./predicted/class30_prob.img")
writeRaster(class_d30_rf, "./predicted/class30.img", NAflag=0)

      #remove uneeded variables from memory
      rm(list=c("prob_d13_rf","class_d13_rf","prob_d15_rf","class_d15_rf","prob_d30_rf","class_d30_rf") 
         
         gc() 

#class 31
prob_d31_rf <- predict(bwca.covar, d31.rf, '31', type="prob", index = 2, progress = "text")
class_d31_rf <- predict(bwca.covar, d31.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d31_rf, "./predicted/class31_prob.img")
writeRaster(class_d31_rf, "./predicted/class31.img", NAflag=0)


#class 32
prob_d32_rf <- predict(bwca.covar, d32.rf, '32', type="prob", index = 2, progress = "text")
class_d32_rf <- predict(bwca.covar, d32.rf, type="response", datatype="INT1U", progress = "text")
writeRaster(prob_d32_rf, "./predicted/class32_prob.img")
writeRaster(class_d32_rf, "./predicted/class32.img", NAflag=0)


    #remove uneeded variables from memory
    rm(list=c("prob_d31_rf","class_d31_rf","prob_d32_rf","class_d32_rf") 
            
          gc() 



##Assigning class with highest probability to each pixel for final classified image##

#stacked probability layers in Imagine after running stats and clipped to small test area

#set working directory
setwd("H:/BWCA/Edrivebackup/BWCA/modeling/bwca_whole/randomforest/predicted/probability")       

#create brick from Imagine layerstack of probability layers      
bwca.prob <- brick ("./rf_prob_layerstack.img")  
#plot to make sure it worked     
spplot(bwca.prob)

#Brick = raster brick, Will return the max value for each pixel in the brick
#brmax <- max(brick) 
brmax <- max(bwca.prob)
writeRaster(brmax, "./max_prob_bwca.img", overwrite=T) 
#plot to make sure it worked      
spplot(brmax) #plot to make sure it worked
      
#return the index (layer) for which the max value occurs    
#maxLev <- which.max(stack)  
maxLev <- which.max(bwca.prob) #errored out for whole bwca...too big, will try subsetting prob stack
writeRaster(maxLev, "./max_class_bwca.img", datatype="INT1U", NAflag=0, overwrite=T)
       
       
       
       
