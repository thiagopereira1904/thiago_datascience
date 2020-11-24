# Construção do Modelo

setwd("C:/Users/Thiago Pereira/Desktop/dsa/BigDataRAzure/Projeto 1")
getwd()


library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(Hmisc)
library(mltools)
library(caret)
library(corrplot)
library(DMwR)

# Carregando o dataset
df <- read_csv("train_sample.csv")
class(df)
str(df)


# Criando coluna hora 
df$hour <- factor(hour(as.POSIXct(df$click_time, format = "%Y/%m/%d")))

df2 <- df

# Dropando colunas desnecessárias para o modelo
df2 <- df2 %>% select(-ip, -click_time, -attributed_time)
head(df2)


# Proporção da variável target(is_attributed)
table(df2$is_attributed)
# Detectado problema de classe rara, será necessário usar uma técnica de OverSampling
# para balancear o dataset


# Transformando a variável target(is_attributed) em factor
df2$is_attributed <- factor(df2$is_attributed)


# ***** Dividindo os dados em treino e teste

trainIndex <- createDataPartition(df2$app, p = .7, list = FALSE)
train <- df2[trainIndex, ]
test <- df2[-trainIndex, ]


# ***** Balanceando o dataset
balanced_train <- SMOTE(is_attributed ~ ., as.data.frame(train),
                        method = "xgbTree",
                        nthread = 8,
                        metric = "ROC",
                        tuneGrid = grid,
                        trControl = ctrl)

# Proporção da variável target pós SMOTE
table(balanced_train$is_attributed)


# Transformando factor in numeric var 
balanced_train$is_attributed <- as.numeric(as.character(balanced_train$is_attributed))
balanced_train$hour <- as.numeric(as.character(balanced_train$hour))
str(balanced_train)


# Correlação das variáveis
correlation <- cor(balanced_train)
corrplot(correlation, method = "color")

# **** XGBoost

str(balanced_train)
# Transformando a variável alvo em factor
balanced_train$is_attributed <- factor(balanced_train$is_attributed)
# Treinando o modelo com a ténica de Cross Validation 
model <- train(is_attributed ~ ., data = balanced_train, method = "xgbTree",
               trControl = trainControl("cv", number = 10))
# Melhores parâmetros
model$bestTune

# Testando o modelo
test$hour <- as.numeric(as.character(test$hour))
predictTest <- model %>% predict(test)

# ***** Métricas para avaliação do modelo
library(MLmetrics)
# Matriz  de confusão
table(test$is_attributed, predictTest)
# Taxa de acerto
Accuracy(y_true = test$is_attributed, y_pred = predictTest)
# De todos os registros que o classificador como positivo, qual percentual é realmente 
# é positivo? 
Precision(y_true = test$is_attributed, y_pred = predictTest, positive = 1) 
# De todos os registros que realmente são positivos, qual percentual é identificado 
# corretamente pelo classificador?
Recall(y_true = test$is_attributed, y_pred = predictTest, positive = 1)
# Média Harmônica entre Precision e Recall
F1_Score(y_true = test$is_attributed, y_pred = predictTest)

# Salvando modelo
saveRDS(model, "./final_model.rds")
saveRDS(final_model, "./final_model.rds")