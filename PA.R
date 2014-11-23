# Practical Machine Learning
# Peer Assessment
# Jorge - November - 2014

library(caret)
library(randomForest)
library(e1071)

# Load Training and Testing files
URL_file_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URL_file_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url = URL_file_training, destfile = "pml-training.csv",method = "curl")
download.file(url = URL_file_testing, destfile = "pml-testing.csv",method = "curl")

my_file_training <- read.csv("pml-training.csv",header = TRUE,  na.strings = c("NA",""))
my_file_testing <- read.csv("pml-testing.csv",header = TRUE,  na.strings = c("NA",""))

# Numbers of Rows and Columns - Before Clean
dim(my_file_training)
dim(my_file_testing)

# Clean data - Remove NULL and Text Columns
my_file_training <- my_file_training[,(colSums(is.na(my_file_training)) == 0)]
my_file_testing <- my_file_testing[,(colSums(is.na(my_file_testing)) == 0)]

my_file_training <- my_file_training[,-c(1:7)]
my_file_testing <- my_file_testing[,-c(1:7)]

# Numbers of Rows and Columns - After Clean
dim(my_file_training)
dim(my_file_testing)

# Dataset Partition
my_partition <- createDataPartition(my_file_training$classe, p = 0.70, list = FALSE)
my_training <- my_file_training[my_partition,]
my_validation <- my_file_training[-my_partition,]

# Fit Model
set.seed(123)
my_fit <- train(classe ~ ., method = "rf", data = my_training, trControl = trainControl(method = "cv", number = 4))

# Predict Model
my_predict_validation <- predict(my_fit, newdata = my_validation)
confusionMatrix(my_predict_validation,my_validation$classe)

# Final Result - Test Data
my_result <- predict(my_fit$finalModel, newdata = my_file_testing)
my_result <- as.character(my_result)
my_result

# Create Final Files
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(my_result)

# Run the RMD - rmarkdown::run('PA.Rmd')