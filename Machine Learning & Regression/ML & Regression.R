rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
library(randomForest)
library(caret)
library(caTools)
library(e1071)
data1=data.frame(as.factor(cs$Product.Type),as.factor(cs$Category),cs$Price.1,
                 as.factor(cs$Vegan),as.factor(cs$Company.Name))

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(cs$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
dtm=DocumentTermMatrix(textcorpus)%>% as.matrix({.}) %>% as.data.frame({.})

#corpus1=cbind(cs$Product.Name,dtm)
#write.csv(corpus1,"C:\\Users\\AMRITA\\OneDrive\\Pictures\\Project\\corpus1.csv")


colnames(dtm) = make.names(colnames(dtm))
#data1=data.frame(as.factor(cs$Product.Type),as.factor(cs$Category),cs$Price.1
#,as.factor(cs$Company.Name),as.factor(cs$Vegan))

#colnames(data1) = c('Product.Type','Category','Price.1','Company.Name','Vegan',
#'Product.Name')
#corpus<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/corpus1.csv")
#cosmetics1=full_join(data1, corpus, by = "Product.Name")

col_names <- names(dtm)
dtm[,col_names] <- lapply(dtm[,col_names] , factor)
str(dtm)

data <- bind_cols(data1, dtm)
data=data.frame(data$as.factor.cs.Product.Type.,data$as.factor.cs.Category.,
                data$cs.Price.1,data$as.factor.cs.Vegan.,
                data$centella,data$ceramide,data$hyaluronic_acid,
                data$niacinamide,data$vitamin.b5,data$collagen,data$green_tea,
                data$peptide,data$rice,data$vitamin.c,data$as.factor.cs.Company.Name.)

#data$collagen,data$green_tea,data$peptide,data$rice,data$vitamin.c
#data <- data2 %>% mutate_if(is.character, as.factor)
#data$cs.Product.Name <- as.factor(data$cs.Company.Name)
set.seed(123)
split <- sample.split(data, SplitRatio = 0.8) 
split
data_train <- subset(data, split == "TRUE") 
data_test <- subset(data, split == "FALSE")
model <- svm(data.cs.Price.1 ~.,data= data_train,
             kernel='radial')
preds <- predict(model,data_test)
table(preds)
summary(model)

#confusionMatrix(data=preds,reference=data_test$cs.Price.1)
#accuracy <- sum((preds - data_test$cs.Price.1)^2)/nrow(data_test)
#print(accuracy*100)
sqrt(mean((preds - data_test$data.cs.Price.1)^2))
plot(preds,data_test$data.cs.Price.1,col=c("green","red"))
# Create a data frame with actual and predicted values




library(randomForest)
model1 <- randomForest(data.cs.Price.1 ~.,data= data_train)
model1
importance(model1)
varImpPlot(model1)
pred_test <- predict(model1, newdata = data_test)
sqrt(mean((pred_test - data_test$data.cs.Price.1)^2))
plot(pred_test,data_test$data.cs.Price.1,col=c("green","red"))
plot(model1)
#library(Metrics)

#accuracy(data_test$data.cs.Price.1,pred_test)
#accuracy(data_test$data.cs.Price.1,preds)
# Create a data frame with actual and predicted values

plot_data <- data.frame(Actual = data_test$data.cs.Price.1, Predicted = pred_test)



# Plot the scatterplot

ggplot(plot_data, aes(x = Actual, y = Predicted)) + 
  
  geom_point() + 
  
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  
  labs(title = "Actual vs Predicted Values", 
       x = "Actual Values", y = "Predicted Values") 


