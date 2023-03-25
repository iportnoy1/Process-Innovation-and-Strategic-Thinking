setwd("C:/Users/idpdl/Downloads")
library("randomForest"); library(corrplot); library(neuralnet); library("e1071") #SVM
#Inputs: PE; #Outputs: IProcess, individualizada, promedio
X <- read.csv("Matriz de datos completa2.csv", header = T)
X[is.na(X)] <- as.numeric(0); n <- ncol(X); nf <- nrow(X); X2 <- X
for (i in 1:n) {
  if(class(X[,i]) == "character"){
    for (j in 1:nf) {
      if(X[j,i] == "Totalmente de acuerdo"){
        X2[j,i] <- 2
      }else if(X[j,i] == "De acuerdo"){
        X2[j,i] <- 1
      }else if(X[j,i] == "Ni de acuerdo ni en desacuerdo"){
        X2[j,i] <- 0
      }else if(X[j,i] == "En desacuerdo"){
        X2[j,i] <- -1
      }else if(X[j,i] == "Totalmente en desacuerdo."){
        X2[j,i] <- -2
      }
    }
  }
}
for (i in 1:n) {
  X2[,i] = as.numeric(X2[,i])
}
X2 <- X2[, grepl("PI",colnames(X2)) | grepl("ST",colnames(X2))]

# Correlation Heatmaps
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "spearman", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corMat=cor(X2, method = "spearman")
temp <- is.na(corMat)
corMat[temp] <- as.numeric(0)
p.mat <- cor.mtest(X2)
corrplot(corMat, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05)

#Machine Learning Models
X2$PI_mean <- rowMeans(X2[,grepl("PI",colnames(X2))])

#Random Forest
RF.mod <- randomForest(x = X2[,grepl("ST",colnames(X2))], y = X2$PI_mean, ntree = 200)
Preds <- predict(RF.mod, X2[,grepl("ST",colnames(X2))])
MAVE = mean(abs(X2$PI_mean - Preds))
MAVE

MAVE_percentage_of_span3 = 100*MAVE/(max(X2$PI_mean) - min(X2$PI_mean))

#Support Vector Machines
# SVM.mod <- svm(IProcess_mean ~ PE1+PE2+PE3+PE4+PE5+PE6+PE6+PE7+PE8+PE9+PE10+PE11+PE12,
#                data = X2, type = 'nu-regression', kernel = 'polynomial')
SVM.mod <- svm(PI_mean ~ ST1+ST2+ST3+ST4+ST5+ST6+ST6+ST7+ST8+ST9+ST10+ST11+ST12,
               data = X2)
Preds2 = predict(SVM.mod, X2[,grepl("ST",colnames(X2))])
MAVE2 = mean(abs(X2$PI_mean - Preds2))
MAVE2
MAVE_percentage_of_span2 = 100*MAVE2/(max(X2$PI_mean) - min(X2$PI_mean))

#Artificial Neural Networks
set.seed(1)
ANN = neuralnet(PI_mean ~ ST1+ST2+ST3+ST4+ST5+ST6+ST6+ST7+ST8+ST9+ST10+ST11+ST12, 
                X2, hidden=c(3,6))
plot(ANN)
Pred <- compute(ANN,X2)
Preds3 <- Pred$net.result
MAVE3 = mean(abs(X2$PI_mean - Preds3))
MAVE3
MAVE_percentage_of_span3 = 100*MAVE3/(max(X2$PI_mean) - min(X2$PI_mean))


#Enviarles correlaciones fuertes
#Agregas citas