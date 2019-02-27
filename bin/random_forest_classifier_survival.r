setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("PRROC")
library("e1071")
library("randomForest")
library("class")
library("gmodels")
library("formula.tools")

source("./confusion_matrix_rates.r")
source("./utils.r")

# args = commandArgs(trailingOnly=TRUE)
# thisNtree <- as.integer(args[1])

# thisNtree <- 5000

FIXED_FEATURE_NUM <- 27

threshold <- 0.5
# fileName <- "../data/dataset_edited_without_time_NORM.csv"
fileName <- "../data/sepsis_severity_dataset_edited_2019-02-11.csv"
patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

patients_data_original <- patients_data

targetName <- "ADDED.survival"
patients_data$"Vasopressors" <- NULL
patients_data$"SOFA.score" <- NULL
col_idx <- grep(targetName, names(patients_data))
patients_data <- patients_data[, c((1:ncol(patients_data))[-col_idx], col_idx)]
colnames(patients_data)

if (FIXED_FEATURE_NUM != ncol(patients_data)) {
    stop("Error: the number of features is different from ", FIXED_FEATURE_NUM, "\n The program will stop here.\n", sep="")
}


cat("[Randomizing the rows]\n")
patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

totalElements <- dim(patients_data)[1]

subsets_size <- totalElements

target_index <- dim(patients_data)[2]

target_label <- colnames(patients_data[target_index])
cat("target_label = ", target_label, "\n", sep="")

if (subsets_size != totalElements) {
    cat("ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
    cat(" containing only ", subsets_size, " elements \n", sep="")
    cat(" instead of ", totalElements, " elements \n", sep="")
}

patients_data <- patients_data[1:subsets_size, ]

dataset_dim_retriever(patients_data)
imbalance_retriever(patients_data[,target_index])

training_set_perc <- 60
INPUT_PERC_POS <- 50
cat("[training set = ", training_set_perc,"%]\n", sep="")
cat("[test set = ", (100-training_set_perc),"%]\n", sep="")

artificialBalance <- FALSE
balancedFlag <- FALSE # flag that sets everything to 50% 50% ratio

if (artificialBalance == TRUE) {


    train_data_balancer_output <- train_data_balancer(patients_data, target_index, training_set_perc, INPUT_PERC_POS, balancedFlag)

    patients_data_train <- train_data_balancer_output[[1]]
    patients_data_test <- train_data_balancer_output[[2]]
    
     # Creating the subsets for the targets
    patients_data_train_labels <- patients_data_train[, target_index] # NEW
     patients_data_test_labels <- patients_data_test[, target_index]   # NEW

} else {


   # the training set is the first 60% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perc/100) # NEW

    # the test set is the last 40% of the whole dataset
    test_set_first_index <- training_set_last_index+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

    cat("[Creating the training set and test set for the values]\n")
    patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
    patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW
    
    cat("[training set dimensions: ", dim(patients_data_train)[1], " patients]\n")

    cat("[test set dimensions: ", dim(patients_data_test)[1], " patients]\n")

    cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
    patients_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
    patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

}


dataset_dim_retriever(patients_data_train)
imbalance_retriever(patients_data_train[,target_index])


cat("\n[Training the random forest classifier on the training set]\n")

allFeaturesFormula <- as.formula(paste(colnames(patients_data)[target_index], '.', sep=' ~ ' ))
selectedFormula <- allFeaturesFormula
cat("\nFeatures used in this prediction: \t", as.character(selectedFormula), "\n\n", sep="")

thisNtree <- 100

rf_new <- randomForest(selectedFormula, data=patients_data_train, importance=TRUE, proximity=TRUE, nTree=thisNtree)



cat("\n[Applying the trained random forest classifier on the test set]\n")
patients_data_test_PRED <- predict(rf_new, patients_data_test, type="response")

confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED, "@@@ Test set @@@")

# mcc_outcome <- mcc(patients_data_test_labels, patients_data_test_PRED_binary)



