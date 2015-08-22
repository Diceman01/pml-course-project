require(caret)
set.seed(125)

#load train data (train.orig)
if(!"pml-training.csv" %in% list.files()){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
}
if(!"train.orig" %in% ls()){
  train.orig <- read.csv("pml-training.csv") 
}

#load test data (test.orig)
if(!"pml-testing.csv" %in% list.files()){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
}
if(!"test.orig" %in% ls()){
  test.orig <- read.csv("pml-testing.csv")
}

#helper functions
prop.na <- function(x){sum(is.na(x))/length(x)}

clean.data <- function(data){
  
  ##drop columns with NAs
  x <- data.frame(sapply(data, prop.na))
  vars_to_drop <- rownames(subset(x,x > 0))
  data <- data[, !names(data) %in% vars_to_drop]
  
  ##drop factor columns
  result <- NULL
  for (i in 1:length(data)){
    if(!is.factor(data[,i])){
      result <- data.frame(cbind(result, as.numeric(data[,i])))
      names(result)[length(result)] <- names(data)[i]
    }
  }
  
  
  require(dplyr)
  ##drop the X variable, which appears to be only a serial number
  result <- select(result, -X)
  ##drop the timestamps, because we are making predictions against the movement, not the time the movement took place
  result <- select(result, -raw_timestamp_part_1)
  result <- select(result, -raw_timestamp_part_2)
  ##drop the num_window variable, because my hunch is that it is not descriptive of the movement
  result <- select(result, -num_window)
  
  
  result
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#clean the data via a helper function (so that train and test can be put through the same cleaning steps)
train.clean <- clean.data(train.orig)
test.clean <- clean.data(test.orig)

#partition the data
inTrain <- createDataPartition(train.orig$classe, p = 0.75, list = FALSE)

#create new training and testing subsets from the clean training set
train.subset <- train.clean[inTrain,]
test.subset <- train.clean[-inTrain,]

#build the model on the new smaller training set and set ntree = 250.
model <- train(train.orig$classe[inTrain] ~ ., method = "rf", ntree = 250, data = train.subset)

#demonstrate model performance statistics
##confusion Matrix for training set
confusionMatrix(predict(model$finalModel, newdata = test.subset), train.orig[-inTrain,"classe"])
##expected out of sample error rate
model$finalModel

evaluateModel <- function(){
  model$finalModel
  print(confusionMatrix(predict(model$finalModel, newdata = test.subset), train.orig[-inTrain,"classe"]))
  varImpPlot(model$finalModel, cex = 0.7)
  writeLines("\nModel Times:\n")
  print(model$times)
  writeLines("Number of training rows: ")
  dim(train.subset)[1]
}

predictions <- predict(model$finalModel, newdata = test.clean)
pml_write_files(predictions)

#discuss why i made the choices I did
##bootstrapping and out-of-sample estimating is built into random forests
##plot the varImps

#copy/customize project submission code