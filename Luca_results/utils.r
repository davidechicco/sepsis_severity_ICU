options(stringsAsFactors = FALSE)

# function that prints two decimals of a number
dec_two <- function(x) {
  return (format(round(x, 2), nsmall = 2));
}

# function that prints two decimals of a number with sign
signed_dec_two <- function(x) {

   sign <- "+"
   if (x<0) { sign <- "-";  }   
 
  return (paste(sign, (format(round(x, 2), nsmall = 2)), sep=""))
}



# Function that reads in a vector made of binary values and prints the imbalance rates
dataset_dim_retriever <- function(thisDataset)
{
  cat("[Dataset size]\n")
  cat("number of data instances (rows) =", dim(thisDataset)[1], "\n")
  cat("number of features (columns) =", dim(thisDataset)[2], "\n\n")
}

# Function that reads in a vector made of binary values and prints the imbalance rates
imbalance_retriever <- function(thisVector, targetName)
{
  lun <- length(table(thisVector))
  if (lun != 2) {
  
    print("This vector is not binary. The imbalance_retriever() function will stop here");
    return ;
  
  }  


  cat("\n [Imbalance of this dataset] target = ", targetName,"\n", sep="")
  number_of_elements_of_first_class <- unname(table(thisVector)[1])
  name_of_elements_of_first_class <- names(table(thisVector)[1])
  cat("[class: ",name_of_elements_of_first_class, "  #elements = ", number_of_elements_of_first_class, "]\t", sep="")
  cat(dec_two(unname(table(thisVector))[1]*100/length(thisVector)),"%\n", sep="")
  
  number_of_elements_of_second_class <-unname(table(thisVector)[2])
  name_of_elements_of_second_class <-names(table(thisVector)[2])
  cat("[class: ",name_of_elements_of_second_class, "  #elements = ", number_of_elements_of_second_class, "]\t", sep="")
  cat(dec_two(unname(table(thisVector))[2]*100/length(thisVector)),"%\n", sep="")
  
  cat("\n\n")

}


# Function that returns a more balanced training set
train_data_balancer <- function(thisDataset, target_index, training_set_perc, INPUT_PERC_POS, balancedFlag) {

    cat("\ntrain_data_balancer() function\n")
    
    thisDatasetSize <- dim(thisDataset)[1]
 
    training_set_numb_of_ele <- round(training_set_perc*thisDatasetSize/100,0)
    cat("\nThe training set will contain ", training_set_numb_of_ele, " items (", training_set_perc, "%) of the data instances\n", sep="")

    test_set_perc <- 100-training_set_perc
    test_set_numb_of_ele <- thisDatasetSize - training_set_numb_of_ele
    cat("The test set will contain ", test_set_numb_of_ele, " items (", test_set_perc, "%) of the data instances\n", sep="")
 
    # Split negative subset and positive subset
    positive_subset <- (thisDataset[is.element(thisDataset[,target_index], 1),])
    negative_subset <- (thisDataset[is.element(thisDataset[,target_index], 0),])
    
    # shuffle again
    positive_subset <- positive_subset[sample(nrow(positive_subset)),] 
    negative_subset <- negative_subset[sample(nrow(negative_subset)),] 
    
    positiveSetSize <- dim(positive_subset)[1]
    negativeSetSize <- dim(negative_subset)[1]
    
    cat("\noriginal \n", sep="")
    cat("positiveSetSize = ", positiveSetSize, "\n", sep="")
    cat("negativeSetSize = ", negativeSetSize, "\n", sep="")
    
    # if balancedFlag then 50% positives and 50% negatives 
    if (balancedFlag == TRUE) {
    
        minorClassSize <- min(positiveSetSize,negativeSetSize)        
    
        positive_subset <- positive_subset[1:minorClassSize,]
        negative_subset <- negative_subset[1:minorClassSize,]
    
        positiveSetSize <- dim(positive_subset)[1]
        negativeSetSize <- dim(negative_subset)[1]
        cat("\n(balancedFlag == TRUE) \n", sep="")
            
       cat("positiveSetSize = ", positiveSetSize, "\n", sep="")
       cat("negativeSetSize = ", negativeSetSize, "\n", sep="")
        
        training_set_numb_of_ele <- round((positiveSetSize+negativeSetSize)*training_set_perc/100,0)
        test_set_numb_of_ele <- (positiveSetSize+negativeSetSize) - training_set_numb_of_ele
        
       cat("\nThe training set will contain ", training_set_numb_of_ele, " items (", training_set_perc, "%) of the data instances\n", sep="")
       cat("The test set will contain ", test_set_numb_of_ele, " items (", test_set_perc, "%) of the data instances\n", sep="")
    }
    

 
    title <- "Positive dataset"
    #dataset_dim_retriever(positive_subset, title)
    #imbalance_retriever(positive_subset[ , target_index], title)

    title <- "Negative dataset"
    #dataset_dim_retriever(negative_subset, title)
    #imbalance_retriever(negative_subset[ , target_index], title)
    
    # cat("\nThe training set will contain ", training_set_numb_of_ele, " items", sep="")
    # cat("\nThe test set will contain ", test_set_numb_of_ele, " items \n", sep="")
    
    # newTrainingSet <- 50% positive_subset & 50% negative_subset 
    # from index 1 to 81 (that is training_set_numb_of_ele/2 ) of positive_subset
    # and from index 1 to 81 (that is training_set_numb_of_ele/2 ) of negative_subset

    train_set_num_of_positives <- round(training_set_numb_of_ele*(INPUT_PERC_POS/100), 0)
    # cat("INPUT_PERC_POS = ", INPUT_PERC_POS, "%\n", sep="")
    # cat("train_set_num_of_positives = ", train_set_num_of_positives, "\n", sep="")
    train_set_num_of_negatives <- round(training_set_numb_of_ele - train_set_num_of_positives,0)
    trainPosComponent <- positive_subset[(1:train_set_num_of_positives), ]
    trainNegComponent <- negative_subset[(1:train_set_num_of_negatives), ]
    newTrainingSetTemp <- rbind(trainPosComponent, trainNegComponent)
    newTrainingSet <- newTrainingSetTemp[sample(nrow(newTrainingSetTemp)),]
    
    title <- "New training set"
    # dataset_dim_retriever(newTrainingSet, title)
    # imbalance_retriever(newTrainingSet[ , target_index], title)
    
    # newTestSet <- all the rest
    # from index 82 (that is training_set_numb_of_ele/2 + 1) to the end of positive_subset
    # and from index 82 (that is training_set_numb_of_ele/2 + 1) to the end of negative_subset
    
#     cat("train_set_num_of_positives +1 = ", train_set_num_of_positives+1, "\n", sep="")
#     cat("positiveSetSize = ", positiveSetSize, "\n", sep="")
#     cat("train_set_num_of_negatives +1 = ", train_set_num_of_negatives+1, "\n", sep="")
#     cat("negativeSetSize = ", negativeSetSize, "\n", sep="")

    testPosComponent <- positive_subset[((train_set_num_of_positives+1):positiveSetSize), ]
    testNegComponent <- negative_subset[((train_set_num_of_negatives+1):negativeSetSize), ]
    
    # print("dim(testPosComponent)")
    # print(dim(testPosComponent))
    # print("dim(testNegComponent)")
    # print(dim(testNegComponent))
    newTestSetTemp <- rbind(testPosComponent, testNegComponent)
    newTestSet <- newTestSetTemp[sample(nrow(newTestSetTemp)),]
    
    title <- "New test set"
    # dataset_dim_retriever(newTestSet, title)
    # imbalance_retriever(newTestSet[ , target_index], title)
    
    return (list(newTrainingSet, newTestSet))
}
