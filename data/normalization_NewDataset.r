# install.packages("class")
# install.packages("gmodels")

# function that normalizes
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
  

# function that converts a string 
# https://stats.stackexchange.com/a/17995
fromStringToNumeric <- function(x_array) {

   new_x <- as.factor(x_array)
   levels(new_x) <- 1:length(levels(new_x))
   new_x_num <- as.numeric(new_x)

   return (new_x_num)
}

num_to_return <- 1
exe_num <- sample(1:as.numeric(Sys.time()), num_to_return)
cat("exe_num = ", exe_num, "\n")

cat("[Reading the data file]\n")
patients_data <- read.csv("./respiration_fio2.csv", stringsAsFactors = FALSE) 


num_of_columns_original <- dim(patients_data)[2]
num_of_instances <- dim(patients_data)[1]
num_of_features_original <- num_of_columns_original - 1

patients_data_original <- patients_data

colnames(patients_data)

patients_data_num <- patients_data

num_of_columns <- dim(patients_data_num)[2]


for(i in 1:(num_of_columns))
{
  patients_data_num[,i] <- fromStringToNumeric(patients_data_num[,i])
}


cat("[Normalizing the values of the data file (except the target column)]\n")
patients_data_norm <- as.data.frame(lapply(patients_data_num[1:num_of_columns], normalize))
colnames(patients_data_norm)

outputFileName <- paste( "./respiration_fio2_norm_", toString(num_to_return), ".csv", sep="")
cat("outputFileName = ", outputFileName, "\n", sep="")
write.table(patients_data_norm, file =outputFileName, row.names=FALSE, na="", col.names=TRUE, sep=",")

