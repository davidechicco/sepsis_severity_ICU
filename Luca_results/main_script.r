setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("PRROC", "class", "gmodels", "formula.tools", "Metrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("PRROC")
library("class")
library("gmodels")
library("formula.tools")
library("Metrics")

source("./confusion_matrix_rates.r")
source("./utils.r")

threshold <- 0.5

### Dataset analysis

datasetName <- "../sepsis_severity_dataset_col_norm_edited.csv"
dataset_dataframe <- read.csv(file=datasetName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("datasetName = ", datasetName, "\n", sep="")


dataset_dim_retriever(dataset_dataframe)
imbalance_retriever(dataset_dataframe$ADDED.survival, "ADDED.survival")
imbalance_retriever(dataset_dataframe$Vasopressors..sepsis.sock., "Vasopressors..sepsis.sock.")

# [Imbalance of this dataset] target = ADDED.survival
# [class: 0  #elements = 178]	      48.90%
# [class: 1  #elements = 186]	      51.10%

# [Imbalance of this dataset] target = Vasopressors..sepsis.sock.
# [class: 0  #elements = 297]       81.59%
# [class: 1  #elements =    67]      18.41%



### Results by Luca

#
# ADDED survival confusion matrix
#
survivalFileName <- "./ADDED_survival_TruePredicted.txt"
survival_vector <- read.csv(file=survivalFileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("survivalFileName = ", survivalFileName, "\n", sep="")

colnames(survival_vector) <- c("ground_truth", "predicted_values")

survival_ground_truth <- survival_vector[,1]
survival_predicted_values <- survival_vector[,2]

confusion_matrix_rates(survival_ground_truth, survival_predicted_values, "@@@ ADDED.survival ::  Confusion matrix @@@")

#  @@@ ADDED.survival @@@ 	 MCC 	 F1_score 	 accuracy 	 TP_rate 	 TN_rate 	 PR AUC 	 ROC AUC
#  @@@ ADDED.survival @@@     +0.38  	  0.70  	  0.69  	          0.73  	     0.64 		     0.64 		     0.69 



#
# SOFA score regression scores
# 
sofaScorefileName <- "./SOFA_score_TruePredicted.txt"
sofa_score_vector <- read.csv(file=sofaScorefileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("sofaScorefileName = ", sofaScorefileName, "\n", sep="")

colnames(sofa_score_vector) <- c("ground_truth", "predicted_values")

sofa_score_ground_truth <- sofa_score_vector[,1]
sofa_score_predicted_values <- sofa_score_vector[,2]

thisRMSE <- rmse(sofa_score_ground_truth, sofa_score_predicted_values)
thisMAE <- mae(sofa_score_ground_truth, sofa_score_predicted_values)
thisMSE <- mse(sofa_score_ground_truth, sofa_score_predicted_values)

cat("  @@@ SOFA score ::  RMSE \t MAE \t MSE\n")
cat("  @@@ SOFA score :: ", dec_two(thisRMSE), " \t ", dec_two(thisMAE), " \t ", dec_two(thisMSE), " \n", sep="")

# @@@ SOFA score ::      RMSE 	     MAE 	       MSE
# @@@ SOFA score ::         1.92 	      1.34 	       3.67

#
# Sepsis shock confusion matrix
#
sepsisShockfileName <- "./Vasopressors_sepsis_shock_TruePredicted.txt"
sepsis_shock_vector <- read.csv(file=sepsisShockfileName, head=TRUE, sep=",", stringsAsFactors=FALSE)
cat("sepsisShockfileName = ", sepsisShockfileName, "\n", sep="")

colnames(sepsis_shock_vector) <- c("ground_truth", "predicted_values")

sepsis_shock_ground_truth <- sepsis_shock_vector[,1]
sepsis_shock_predicted_values <- sepsis_shock_vector[,2]

confusion_matrix_rates(sepsis_shock_ground_truth, sepsis_shock_predicted_values, "@@@ Sepsis shock :: Confusion matrix @@@")

# @@@ Sepsis shock @@@ 	 MCC 	   F1_score accuracy 	 TP_rate 	 TN_rate 	 PR AUC 	 ROC AUC
# @@@ Sepsis shock @@@        +0.23  	  0.24  	  0.82  	    0.15  	         0.97 		    0.28 		    0.56 


