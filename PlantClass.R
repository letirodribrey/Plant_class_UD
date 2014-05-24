setwd("C:/Users/Leti/Desktop/UD") #set working directory
data<-read.csv("leaf.csv") #load the data
#Set variables names
colnames(datos)<-c("Class","Specimen_Number","Eccentricity","Aspect_Ratio","Elongation","Solidity","Stochastic_Convexity","Isoperimetric_Factor","Maximal_Indentation_Depth","Lobedness","Average_Intensity","Average_Contrast","Smoothness","Third_moment","Uniformity","Entropy")


nrow(datos) # this data has 339 rows

head(datos) # look at the first few

#scale the data, except column 1
new_datos <- scale(datos[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]) 
leaf_data_norm<-data.frame(datos[,1],new_datos)

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splits <- splitdf(datos, seed=800) #apply the function setting seed=800

str(splits)
#it returns a list - two data frames called trainset and testset

lapply(splits,nrow)
# number of observations in each data frame

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

plant_class <- funtion(training.dataset,test.dataset,output.filename) {
  training.table<-read.table(training.dataset)
  test.table<-read.table(test.dataset)
  
  # Retrieve class and features from training data
  training.class<-training.table[,1]
  training.features<-training.table[,-1]
  remove(training.table)
  
  #funtion for calculating priors
  calculate.priors<-funtion(class.vector) {
    prior<-c()
    for (class in unique(class.vector)) {
      priors<-rbind(priors, c(class,length(class.vector[class.vector==class])/length(class.vector)))
      colnames(priors)<-c("classification","probability")   
    }
    return (priors)  
  }
  priors<-calculate.prior(training.class)
  
  #learn the features by calculating likelihood
  likelihood.list<-list()
  #calculate CPD by feature
  for (i in 1:dim(training.features)[2]) {
    feature.values<-training.features[,i]
    unique.feature.values<-unique(feature.values)
    likelihood.matrix<-matrix(rep(NA),nrow=dim(priors)[1], ncol=length(unique,feature.values))
    colnames(likelihood.matrix)<-unique.feature.values
    rownames(likelihood.matrix)<-priors[,"classification"]
    for (item in unique.feature.values) {
      likelihood.item<-vector()
      for (class in priors[,"classification"]) {
        feature.value.inclass<-feature.values[training.class==class]
        likelihood.value<-lencht(fature.value.inclass[feature.value.inclass==item])/length(feature.value.inclass)
        likelihood.item<-c(likelihood.item,likelihood.value)
      }
      likelihood.matrix[,item]<-likelihood.item
    }
    likelihood.list[[i]]<-likelihood.matrix
  }
  
  #Predict class for the test dataset
  #retrieve the features and target class of the testing dataset
  test.feature<-test.table[,-1]
  test.target.class<-test.table[,1]
  test.predict.class<-rep(NA,length(test.target.class))
  remove(test.table)
  
  #Calculate posterior for each test data record
  for (i in 1:dim(test.features)[1] {
    record<-test.features[i,]
    posterior<-vector()
    #calculate posteriors for each possible class of that record
    for (class in priors[,"classification"]) {
      #initializate posterior as the prior value os that class
      posterior.value<-as.numeric(priors[priors[,"classification"]==class,2])
      likelihood.v<-c()
      for (item in 1:length(record)) {
        likelihood.value<-likelihood.list[[item]][class,as.character(record[1,item])]
        likelihood.v<-c(likelihood.v,likelihood.value)
        posterior.value<-as.numeric(posterior.value)*as.numeric(likelihood.value)
      }
      posterior<-rbind(posterior,c(class,posterior.value))
    }
    predict.class<-posterior[posterior[,2]==max(as.numeric(posterior[,2])),1]
    test.predict.class[i]<-predict.class 
  }
  accuracy<-length(test.predict.class[test.predict.class==test.target.class])/length(test.target.class)
  test.output<-cbind(test.features,test.target.class,test.predict.class)
  
  #print resultand export to file
  file.con<-file(output.filename)
  write("Naive Bayes Classification Results\n",file=output.filename)
  #write("\n",file=output.filename, append=TRUE) NOT DONE!!
  #for (i in 1:length())
  #write(paste("The prior probability for P(y=",i,") is: ", priors[priors[,1]=="i",2],sep=""),file=output.filename,append=TRUE)
  
  write("Next, print the likelihood value for all the features\n",file=output.filename,append=TRUE)
  for (i in 1:length(likelihood.list)) {
    write(paste("\nLikelihood for feature", i, "\n"),file=output.filename,append=TRUE)
    write.table(data.frame(likelihood.list[[i]]),file=output.filename,eol="\n",append=TRUE)
  }
  write("\n\nNext, print the test recors with target class and predicted class\n",file=output.filename,append=TRUE)
  write.table(test.output, file=output.filename,eol="\n",append=TRUE)
  write(paste("\n\nFinally, with the above Naive Bayes classifier, the prediction accuracy is",accuracy,"\n"),file=output.filename,append=TRUE)
  cat("\nCompleted! Output in file: ",output.filename,"\n")
}
plant_class(training,testing,"plant.class")
