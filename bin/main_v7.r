

# I nomi sono classici in ML, standard...
# YF = Y Forward (predetta)
# YT = Y Test
# D = Dataset
# v = indici delle colonne nominali del dataset
# ix = indice delle colonne di imnput del dataset
# iy = indice delle colonne di output del dataset
# IM1 IM2 IM3 = matrici delle importanze per calcoloare la cosa che ti ho spiegato nelle mail precedenti
# mc = numero di MonteCarlo (è spiegato nel file...)
# k = vettore temporaneo di indirizzi per creare dati di train e test
# n = numero di campioni nel dataset
# il = vettore temporaneo di indirizzi del learning (chiamato volgarmente training) set
# it = vettore temporaneo di indirizzi del test set
# XL = matrice di imput del learning set
# YL = matrice di outpur del learning set
# XT = matrice di imput del test set
# YT = matrice di outpur del test set
# M = modello
# YFR, YTR = ho dimenticato di cacellarle. Puoi cancellare riga 58, 104, 105


# Enviroment
setwd(".")
rm(list=ls())

list.of.packages <- c("easypackages", "randomForest","caret","e1071","keras","ROSE","DMwR","Metrics","mltools","PRROC","MLmetrics","DescTools", "mmcr", "EvaluationMeasures")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

cat("\014")
set.seed(11)

# Parameters
method = "TREE" #Options: TREE, RF, SVMLIN, SVMKER, MLP
nl = 300 # Size of the training set
mc = 100 # Number of repetition of the experiments 100

# Functions
MetricsClass <- function(YF,YT,j){
  tmp1 = levels(YF)
  tmp2 = levels(YT)
  if (!all(tmp1==tmp2)){
    print("Problems in the output")
  }
  YF = as.numeric(YF) - 1
  YT = as.numeric(YT) - 1
  ris = mcc(preds=YF,actuals=YT)
  ris = c(ris,MLmetrics::F1_Score(YT,YF)) # F1_Score(y_true, y_pred, positive = NULL)
  ris = c(ris,MLmetrics::Accuracy(YF,YT))
  ris = c(ris,MLmetrics::Recall(YT,YF))
  ris = c(ris,MLmetrics::Specificity(YT,YF))
  ris = c(ris,PRAUC(YF,YT))
  ris = c(ris,AUC(YF,YT))
  
  # new Davide
  # thisPPV <- MLmetrics::Precision(YT,YF) # PPV: Precision(y_true, y_pred, positive = NULL)
  thisPPV <- EvaluationMeasures.PPV(Real = YT, Predicted = YF, Positive = 1)
  thisNPV <- EvaluationMeasures.NPV(Real = YT, Predicted = YF, Positive = 1) # EvaluationMeasures.NPV(Real = NULL, Predicted = NULL, Positive = 1,TP = NULL, TN = NULL, FP = NULL, FN = NULL
  cat("thisPPV = ", thisPPV, "\t", sep="")
  cat("thisNPV = ", thisNPV, "\n", sep="")
  
   ris = c(ris, thisPPV)
   ris = c(ris, thisNPV)
   
   cat("Confusion matrix: \n")
   print(EvaluationMeasures.table(Real = YT, Predicted = YF, Positive = 1))
   cat("\n")
  
  
}
MetricsRegr <- function(YF,YT){
  ris = rmse(YT,YF)
  ris = c(ris,mae(YT,YF))
  ris = c(ris,mse(YT,YF))
  ris = c(ris,mase(YT,YF))
  ris = c(ris,smape(YT,YF))
  ris = c(ris,R2_Score(YF,YT))
}

# Results
D <- read.csv('../data/sepsis_severity_dataset_edited_2019-03-22.csv', header=TRUE);
if (method == "RF") {
  v = c(2,4:13,20,22,25:29)
} else {
  v = c(20,29)
}
for (i in v) {
  D[,i] = as.factor(D[,i])
}
ix = c(1:19,21:22,25:28)
iy = c(20,23,29) 
ris1 = c(); ris2 = c(); ris3 = c(); 
IM1 = array(0,dim=c(mc,length(ix),2)); IM2 = array(0,dim=c(mc,length(ix),2)); IM3 = array(mc,dim=c(mc,length(ix),2));
VA1 = array(0,dim=c(mc,length(ix),2)); VA2 = array(0,dim=c(mc,length(ix),2)); VA3 = array(mc,dim=c(mc,length(ix),2));
for (i in c(1:mc))
{
  for (j in c(1:3))
  {
    print(sprintf("mc: %03d, y: %02d",i,j))
            
    n = nrow(D)
    k = sample(n)
    il = k[1:nl]
    it = k[nl:n]
    XL = D[il,ix]
    YL = D[il,iy[j]]
    XT = D[it,ix]
    YT = D[it,iy[j]]
    if (method == "RF")    { tmp  = min(sum(YL==levels(YL)[1]),sum(YL==levels(YL)[2])) }
    if (method == "TREE")  { strmethod = "rpart2";          grid = expand.grid(maxdepth=c(2,4,6,8,10,12,14)) }
    if (method == "SVMLIN"){ strmethod = "svmLinear";       grid = expand.grid(C=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50)) }
    if (method == "SVMKER"){ strmethod = "svmRadial";       grid = expand.grid(C=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50),sigma=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50)) }
    if (method == "MLP")   { strmethod = "mlpKerasDropout"; grid = expand.grid(size=c(5,10,20,40,80,160),dropout=c(0,.001,.01,.1),
                                                                               batch_size=c(nl/10,nl),lr=c(.001,.01,.1,1),rho=c(.9,0.09),
                                                                               decay=c(.001,.01,.1,1),activation=c("relu")) }
    if (j == 1 || j == 3)
    {
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, sampling = "up", allowParallel=TRUE)
      if (method == "RF"){
        M = randomForest(x=XL,y=YL,ntree=1000,do.trace=FALSE,importance=TRUE,sampsize=c(tmp,round(2.5*tmp/4)))
      } else {
        M <- train(x=XL,y=YL,method=strmethod,trControl=trctrl,tuneGrid=grid,preProcess=c("center","scale"),metric="Kappa")
      }
    } else {
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
      if (method == "RF"){
        M = randomForest(x=XL,y=YL,ntree=1000,do.trace=FALSE,importance=TRUE)
      } else {
        M <- train(x=XL,y=YL,method=strmethod,trControl=trctrl,tuneGrid=grid,preProcess=c("center","scale"))
      }
    }
    YF = predict(M, XT)
    if (j == 1)
    {
      ris1 = rbind(ris1, MetricsClass(YF,YT,j))
    }
    if (j == 2) {
      YF = as.vector(YF)
      YT = as.numeric(YT)
      ris2 = rbind(ris2, MetricsRegr(YF,YT))
    }
    if (j == 3)
    {
      ris3 = rbind(ris3, MetricsClass(YF,YT,j))
    }
    if (method == "RF"){
      IM11 = importance(M,type=1,scale=FALSE)
      IM33 = importance(M,type=2,scale=FALSE)
      if (j == 1){
        IM1[i,,1] = sort(IM11, decreasing = TRUE, index.return = TRUE)$ix; IM1[i,IM1[i,,1],1] = c(1:length(IM1[i,,1]))
        IM1[i,,2] = sort(IM33, decreasing = TRUE, index.return = TRUE)$ix; IM1[i,IM1[i,,2],2] = c(1:length(IM1[i,,2]))
        VA1[i,,1] = IM11
        VA1[i,,2] = IM33
      }
      if (j == 2){
        IM2[i,,1] = sort(IM11, decreasing = TRUE, index.return = TRUE)$ix; IM2[i,IM2[i,,1],1] = c(1:length(IM2[i,,1]))
        IM2[i,,2] = sort(IM33, decreasing = TRUE, index.return = TRUE)$ix; IM2[i,IM2[i,,2],2] = c(1:length(IM2[i,,2]))
        VA2[i,,1] = IM11
        VA2[i,,2] = IM33
      }
      if (j == 3){
        IM3[i,,1] = sort(IM11, decreasing = TRUE, index.return = TRUE)$ix; IM3[i,IM3[i,,1],1] = c(1:length(IM3[i,,1]))
        IM3[i,,2] = sort(IM33, decreasing = TRUE, index.return = TRUE)$ix; IM3[i,IM3[i,,2],2] = c(1:length(IM3[i,,2]))
        VA3[i,,1] = IM11
        VA3[i,,2] = IM33
      }
    }
  }
}
ris1.m = as.numeric(lapply(data.frame(ris1),mean));   ris1.s = as.numeric(lapply(data.frame(ris1),sd));
ris2.m = as.numeric(lapply(data.frame(ris2),mean));   ris2.s = as.numeric(lapply(data.frame(ris2),sd));
ris3.m = as.numeric(lapply(data.frame(ris3),mean));   ris3.s = as.numeric(lapply(data.frame(ris3),sd));
if (method == "RF"){
  for (i in c(1:dim(IM1)[2])){
    for (j in c(1:dim(IM1)[3])){
      tmp = Mode(IM1[,i,j]); IM1[1:length(tmp),i,j] = tmp
      tmp = Mode(IM2[,i,j]); IM2[1:length(tmp),i,j] = tmp
      tmp = Mode(IM3[,i,j]); IM3[1:length(tmp),i,j] = tmp
      VA1[1,i,j] = median(VA1[,i,j]);
      VA2[1,i,j] = median(VA2[,i,j]);
      VA3[1,i,j] = median(VA3[,i,j]);
    }
  }
}

# Print
topf = length(ix)
cat("\014")
print(names(D)[iy[1]])
print(sprintf("method     mcc     f1     acc     rec     spec     prauc     rocauc     ppv     npv"))
print(sprintf("%s    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f\\\\",
              method,ris1.m[1],ris1.s[1],ris1.m[2],ris1.s[2],ris1.m[3],ris1.s[3],ris1.m[4],ris1.s[4],ris1.m[5],ris1.s[5],ris1.m[6],ris1.s[6],ris1.m[7],ris1.s[7],ris1.m[8],ris1.s[8],ris1.m[9],ris1.s[9]))
if (method == "RF"){
  print(sprintf("Top %d Feature Set Method %d \\",topf,j))
  IM1 = IM1[1,,1]+IM1[1,,2]
  tmp = sort(IM1, decreasing = FALSE, index.return = TRUE)$ix
  j = 0
#   for (i in tmp[1:topf]){
#     j = j + 1
#     print(sprintf("$%d$     %.2e$    %.2e$ & \   extcolor{red}{\   extit{%s}} \\\\",j,VA1[1,i,2],VA1[1,i,1],names(D)[ix[i]]))
#   }
}
print(names(D)[iy[2]])
print(sprintf("rmse,mae,mse,mase,smape,mape,R2"))
print(sprintf("%s    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f \\\\",
              method,ris2.m[1],ris2.s[1],ris2.m[2],ris2.s[2],ris2.m[3],ris2.s[3],ris2.m[4],ris2.s[4],ris2.m[5],ris2.s[5],ris2.m[6],ris2.s[6]))
if (method == "RF"){
  print(sprintf("Top %d Feature Set Method %d \\",topf,j))
  IM2 = IM2[1,,1]+IM2[1,,2]
  tmp = sort(IM2, decreasing = FALSE, index.return = TRUE)$ix
  j = 0
#   for (i in tmp[1:topf]){
#     j = j + 1
#     print(sprintf("$%d$     %.2e$    %.2e$ & \   extcolor{red}{\   extit{%s}} \\\\",j,VA2[1,i,2],VA2[1,i,1],names(D)[ix[i]]))
#   }
}
print(names(D)[iy[3]])
print(sprintf("method     mcc     f1     acc     rec     spec     prauc     rocauc     ppv     npv"))
print(sprintf("%s    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f    %.2f+-%.2f\\\\",
              method,ris3.m[1],ris3.s[1],ris3.m[2],ris3.s[2],ris3.m[3],ris3.s[3],ris3.m[4],ris3.s[4],ris3.m[5],ris3.s[5],ris3.m[6],ris3.s[6],ris3.m[7],ris3.s[7],ris3.m[8],ris3.s[8],ris3.m[9],ris3.s[9]))
if (method == "RF"){
  print(sprintf("Top %d Feature Set Method %d \\",topf,j))
  IM3 = IM3[1,,1]+IM3[1,,2]
  tmp = sort(IM3, decreasing = FALSE, index.return = TRUE)$ix
  j = 0
#   for (i in tmp[1:topf]){
#     j = j + 1
#     print(sprintf("$%d$     %.2e$    %.2e$ & \   extcolor{red}{\   extit{%s}} \\\\",j,VA3[1,i,2],VA3[1,i,1],names(D)[ix[i]]))
#   }
}
