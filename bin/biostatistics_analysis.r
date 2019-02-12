setwd(".")
options(stringsAsFactors = FALSE)

source("./utils.r")


patients_data <- read.csv(file="sepsis_severity_dataset_edited_2019-02-11.csv",head=TRUE,sep=",",stringsAsFactors=FALSE)

patients_data_original <- patients_data

patients_data <- patients_data[ , order(names(patients_data))]

for(i in 1:(ncol(patients_data))) { 

    cat("\n\n", colnames(patients_data)[i], ": \n", sep="") 
    print(table(patients_data[,i]))
    print(summary(patients_data[,i])) 

}


# Vaspopressors as target

targetYesValue <- 1
targetNoValue <- 2
targetName <- "Vasopressors"

targetIndex <- which(colnames(patients_data)==targetName)

# Vasopressors patients YES

patients_data_target_yes <- (patients_data[patients_data[, targetIndex]==targetYesValue,])
patients_data_target_yes <- patients_data_target_yes[ , order(names(patients_data_target_yes))]


for(i in 1:(ncol(patients_data_target_yes))) { 

    cat("\n\n", colnames(patients_data_target_yes)[i], ": \n", sep=""); 
    print(table(patients_data_target_yes[,i])) 
    print(summary(patients_data_target_yes[,i]))    
    

}

# Vasopressors patients NO

patients_data_target_no <- (patients_data[patients_data[, targetIndex]==targetNoValue,])
patients_data_target_no<- patients_data_target_no[ , order(names(patients_data_target_no))]

for(i in 1:(ncol(patients_data_target_no))) { 

    cat("\n\n", colnames(patients_data_target_no)[i], ": \n", sep=""); 
    print(table(patients_data_target_no[,i])) 
    print(summary(patients_data_target_no[,i])) 

}

# All patients: p-value, t-value, and PCC

for(i in 1:(ncol(patients_data))) { 

    # cat("\n\ncorrelation between (target) ", colnames(patients_data)[targetIndex], " and ",  colnames(patients_data)[i], ": \n", sep="") 
    
    # cat("\n", colnames(patients_data)[i], " [versus]  ", colnames(patients_data)[targetIndex], "(target) : correlation\n", sep="")
    
    thisTtest <- t.test(patients_data[,i], patients_data[,targetIndex])
    tValue <- (thisTtest$statistic)[[1]]
    pValue <- (thisTtest$p.value)
    thisPCC <- cor(patients_data[,i], patients_data[,targetIndex], method=c("pearson"))
    conf_int_start <- dec_two((thisTtest$conf.int)[1])
    conf_int_end <- dec_two((thisTtest$conf.int)[2])
    
    cat("t \t\t p-value \t PCC \t conf_int\n", sep="")
    cat(dec_two(tValue), "\t\t", pValue, "\t\t", dec_two(thisPCC), "\t", conf_int_start, "\t", conf_int_end, "\n\n", sep="")
    
    # cat("t = ", dec_two(tValue), "\n", sep="")
    # cat("p-value = ", dec_two(pValue), "\n", sep="")
    # cat("PCC(", colnames(patients_data)[targetIndex], ", ", colnames(patients_data)[i], ") = ",  dec_two(thisPCC), "\n", sep="")

}
