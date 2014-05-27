rm(list = ls()) #Clear all previous work
#setwd("C:/Users/Leti/Desktop/UD") set working directory
data<-read.csv("leaf.csv") #load the data
#Set variables names
colnames(data)<-c("Class","Specimen_Number","Eccentricity","Aspect_Ratio","Elongation","Solidity","Stochastic_Convexity","Isoperimetric_Factor","Maximal_Indentation_Depth","Lobedness","Average_Intensity","Average_Contrast","Smoothness","Third_moment","Uniformity","Entropy")


nrow(data) # this data has 339 rows

head(data) # look at the first few

#scale the data, except column 1
new_data <- scale(data[,c(-1)])
leaf_data_norm<-data.frame(data[,1],new_data)

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
        if (!is.null(seed)) set.seed(seed)
        index <- 1:nrow(dataframe)
        trainindex <- sample(index, trunc(length(index)*2/3))
        trainset <- dataframe[trainindex, ]
        testset <- dataframe[-trainindex, ]
        list(trainset=trainset,testset=testset)
}

splits <- splitdf(leaf_data_norm) #apply the function setting seed=800

str(splits)
#it returns a list - two data frames called trainset and testset

lapply(splits,nrow)
# number of observations in each data frame

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

#definition of the training function
PlantClass_train<-function(){
        train_in = training[,-1]
        train_out = as.factor(training[,1])
        class_model <- train(train_in,train_out,'nb',
        trControl = trainControl(method = "cv"))
        return (class_model)
}

#definition of the test function
PlantClass_test<-function(test_data){
        test_in = testing[,-1]
        test_out = as.factor(testing[,1])
        predict(class_model$finalModel,test_in)
        prediction <- predict(class_model$finalModel,test_in)$class
        return (class_model)
}

#testing the functions
class_model <- PlantClass_train()
prediction <- PlantClass_test(testing)
print(prediction)
prediction
