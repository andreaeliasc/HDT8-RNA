ibrary(e1071)
library(caret)
library(rJava)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(cluster)#Para calcular la silueta
library(e1071)#para cmeans
library(cluster)#Para calcular la silueta
library(mclust) #mixtures of gaussians
library(fpc)#para hacer el plotcluster
library(NbClust)#Para determinar el número de clusters optimo
library(factoextra)#Para hacer graficos bonitos de clustering
library(e1071)
library(caret)
library(corrplot) # install.packages("corrplot")
library(ANN2)

## Separación de datos y agregar variable dicotómica 
# Se carga el set de datos
# Se detemina el porcentaje de datos que se utilizaran para train y para test
# utilizando el 70% de los datos para entrenamiento y el 30% de los datos para prueba.
porcentaje<-0.7
datos<-read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)

# Basados en la agrupacion de la hoja de trabajo anterior con cluster se hace la categorizacion de las casas
# Agegando la columna de tipo de casa segun el clustering anterior
datos$grupo <- ifelse(datos$SalePrice<178000, "3", 
                      ifelse(datos$SalePrice<301000, "2",
                             ifelse(datos$SalePrice<756000,"1",NA)))

# Se cambia la variable grupo a tipo factor
datos$grupo <- as.factor(datos$grupo)

## Analisis Exploratorio 
scatter.smooth(datos$LotFrontage, datos$SalePrice)
scatter.smooth(datos$LotArea, datos$SalePrice)
scatter.smooth(datos$GrLivArea, datos$SalePrice)
scatter.smooth(datos$YearBuilt, datos$SalePrice)
scatter.smooth(datos$BsmtUnfSF, datos$SalePrice)
scatter.smooth(datos$TotalBsmtSF, datos$SalePrice)
scatter.smooth(datos$X1stFlrSF, datos$SalePrice)
scatter.smooth(datos$GarageYrBlt, datos$SalePrice)
scatter.smooth(datos$GarageArea, datos$SalePrice)
scatter.smooth(datos$YearRemodAdd, datos$SalePrice)
scatter.smooth(datos$TotRmsAbvGrd, datos$SalePrice)
scatter.smooth(datos$MoSold, datos$SalePrice)
scatter.smooth(datos$OverallQual, datos$SalePrice)

#Obtenemos los datos de las variables que nos serviran
datos <- datos[,c("LotFrontage","LotArea","GrLivArea","GarageArea","YearRemodAdd","SalePrice" ,"grupo")]

datos <- na.omit(datos)

head(datos, 10)

# Se realiza el corte de datos para el set de Train y el Set de Test
porcentaje<-0.7
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

head(train)

head(test)

#-------------------------------------------------
# Red Neuronal con nnet 
#-------------------------------------------------

modelo.nn2 <- nnet(grupo~.,data = train[,c(1:5,7)], size=6, rang=0.0000001,
                   decay=5e-4, maxit=500) 
modelo.nn2

# Se realiza la prediccion con este modelo 
prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,1:5]))
columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])
columnaMasAlta
test$prediccion2<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
head(test, 30)

# Se obtiene la matriz de confusion para este modelo 
cfm<-confusionMatrix(as.factor(test$prediccion2),test$grupo)
cfm

#-------------------------------------------------
# Red Neuronal con RWeka 
#-------------------------------------------------
NB <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
NB 
WOW(NB)
nnodos='6'

modelo.bp<-NB(grupo~., data = train[,c(1:5,7)], control=Weka_control(H=nnodos, N=4000, G=TRUE), options=NULL)

# Se realiza la prediccion con este modelo 
test$prediccionWeka<-predict(modelo.bp, newdata = test[,1:5])
head(test[,c(1:5,7,9)], 30)
# Se obtiene la matriz de confusion para este modelo
cfmWeka<-confusionMatrix(test$prediccionWeka,test$grupo)
cfmWeka
                      
                      
                      
#weka2
NB <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
NB 
WOW(NB)
nnodos='4'

modelo.bp<-NB(as.factor(grupo)~., data=train[,c(1:5,7)], control=Weka_control(H=nnodos, N=1000, G=TRUE), options=NULL)
test$prediccionWeka<-predict(modelo.bp, newdata = test[,1:5])
cfmWeka<-confusionMatrix(test$prediccionWeka,as.factor(test$grupo))
cfmWeka

corr <- data.frame(test$SalePrice,test$prediccionWeka)
#####
 #-------------------------------------------------
# Red Neuronal con caret
#-------------------------------------------------

modeloCaret <- train(as.factor(group)~., data=train, method="nnet", trace=F)
modeloCaret
pc<-test$prediccionCaret<-predict(modeloCaret, newdata = test[,1:14])
test$prediccionCaret
cfmCaret<-confusionMatrix(as.factor(test$prediccionCaret),as.factor(test$group))
cfmCaret
corrN1 <- data.frame(test$SalePrice,test$prediccionCaret)
