library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

#Data
data <- read.csv(file.choose(), header = T)
View(data)
str(data)
data <- data[,-1]

# Visualization
data %>%
  ggplot(aes(x=Status, y=IPK, fill = Status)) +
  geom_boxplot() +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Box Plot")
data %>%
  ggplot(aes(x=Status, y=JML_TANGGUNGAN, fill = Status)) +
  geom_boxplot() +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Box Plot")
data %>%
  ggplot(aes(x=Status, y=PENGHASILAN, fill = Status)) +
  geom_boxplot() +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Box Plot")

data %>% ggplot(aes(x=IPK, fill = Status)) +
  geom_density(alpha=0.8, color= 'black') +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Density Plot")
data %>% ggplot(aes(x=JML_TANGGUNGAN, fill = Status)) +
  geom_density(alpha=0.8, color= 'black') +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Density Plot")
data %>% ggplot(aes(x=PENGHASILAN, fill = Status)) +
  geom_density(alpha=0.8, color= 'black') +scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
train <- data[ind == 1,]
test <- data[ind == 2,]
model <- naiveBayes(formula = Status ~ ., data = train, usekernel =T)
print(model)
prediksi <- predict(model, test[,-4])
prediksi
comparation_result = cbind(prediction = as.character(prediksi), actual = as.character(test[,4]))
jumlah <- sum(prediksi==test[,4])
persenan_akurasi <- 100*jumlah/length(prediksi)
persenan_akurasi

library(caret)
cm <- confusionMatrix(prediksi,test[,4])
cm
