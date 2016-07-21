library(h2o)
localH2O<-h2o.init(nthreads=-1,ip = "127.0.0.1", port = 54321,max_mem_size='4g')

# Two ways to load frames into h2o 
# loaded data into h2o UI by directly uploading data file in interface and after that load same h2o frame in R using getFrame(ID) function

# or upload from R directly as I have done, as shown below:
CrimeData <- h2o.importFile("FilteredCrimeData") # As my census data is from 2008-2012 I'm loading crime data of that year only which I already filtered and saved as csv so loading filtered crime data as below.
CensusData <- h2o.importFile("./Downloads/Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv")

# cleaning up crime data and removing unwanted NA's
CrimeData <- na.omit(CrimeData)

# For joining two frames in H2O, renaming 'Community Area' column to match with Crime data's same column
names(CensusData) <- c("Community.Area",names(CensusData)[2:9])

#Joining two data frames using h2o.merge
totalData <- h2o.merge(CensusData, CrimeData)

#dropping unwanted columns
totalData <-totalData[,-c(2,3,4,6,12,13,14,15,16,17,18,20,23)]

# creating function that will create train, test and hold set and also run all 3 model that we want to compare. At the end it will print R2 on training, testing set of all models
predictFunction <- function(data){
   r <- h2o.runif(data)
   train <- data[ r < 0.6,]
   test <- data[ (r >= 0.6) & (r < 0.9),]
   hold <- data[ r >= 0.9,]
   print("Created train and test set")
   
   myY <- "Arrest"
   myX <- setdiff(names(train), myY)
   
   # Run GBM
   print("Running Gradient Boosting Model:===========")
   gbm <- h2o.gbm(x = myX, build_tree_one_node = T, y = myY,
                  training_frame = train,
                  validation_frame = test,
                  ntrees = 100,
                  max_depth = 6,
                  learn_rate = 0.1)
   
   # Run DRF
   print("Running Distributed Random Forest Model:===========")
   drf <- h2o.randomForest(x = myX, y = myY,
                           training_frame = train,
                           validation_frame = test,
                           ntrees = 250,
                           max_depth = 30)
   
   # Run GLM
   print("Running Generallize Linear Model:===========")
   glm <- h2o.glm(x = myX, y = myY,
                  training_frame = train,
                  validation_frame = test,
                  lambda = 1e-5,
                  family = "binomial")
   
   # R2 score on training and validation set & report:
   print("Calculating r2 on Training and Testing:===========")
   train_r2_gbm <- h2o.r2(gbm)
   test_r2_gbm <- h2o.r2(gbm, valid = TRUE)
   print(paste0("GBM R2 train= ", train_r2_gbm, "R2 Test= ", test_r2_gbm))
   
   train_r2_drf <- h2o.r2(drf)
   test_r2_drf <- h2o.r2(drf, valid = TRUE)
   print(paste0("DRF R2 train= ", train_r2_drf, "R2 Test= ", test_r2_drf))
   
   train_r2_glm <- h2o.r2(glm)
   test_r2_glm <- h2o.r2(glm, valid = TRUE)
   print(paste0("glm R2 train= ", train_r2_glm, "R2 Test= ", test_r2_glm))
}

# now save all model into local so that we can use it again.
h2o.saveModel(gbm, path="./Users/Heet/projects/Crimeprediction/Models")
h2o.saveModel(glm, path="./Users/Heet/projects/Crimeprediction/Models")
h2o.saveModel(drf, path="./Users/Heet/projects/Crimeprediction/Models")




