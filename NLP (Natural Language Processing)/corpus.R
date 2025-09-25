library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(dplyr)

#brightning & rejuvenating
#reading data
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cosmetics<-read.csv(url,stringsAsFactors = FALSE)
rejuvenating=cosmetics %>% filter(cosmetics$Category == "Rejuvenating")
brightning=cosmetics %>% filter(cosmetics$Category == "Brightning")
g1=rbind(brightning,rejuvenating)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g1$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(3,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))



rm(list = ls())
#clarifying & deep cleansing
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
cleansing=cosmetics %>% filter(cosmetics$Category == "Deep Cleansing")
clarifying=cosmetics %>% filter(cosmetics$Category == "Clarifying")
g2=rbind(cleansing,clarifying)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g2$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Accent"))


rm(list = ls())
#hydration & soothing
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
hydration=cosmetics %>% filter(cosmetics$Category == "Hydration")
soothing=cosmetics %>% filter(cosmetics$Category == "Soothing")
g3=rbind(hydration,soothing)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g3$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Set2"))


rm(list = ls())
#moisturizating & nourishing
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
moisturizing=cosmetics %>% filter(cosmetics$Category == "Moisturizing")
nourishing=cosmetics %>% filter(cosmetics$Category == "Nourishing")
g4=rbind(moisturizing,nourishing)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g4$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.8), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Set1"))


rm(list = ls())
#pore_refining, outdoor_activity_waterproof & Water Sweat Resistant
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
pore=cosmetics %>% filter(cosmetics$Category == "Pore Refining")
outdoor=cosmetics %>% filter(cosmetics$Category == "Outdoor Activity Waterproof")
resistant=cosmetics %>% filter(cosmetics$Category == "Water Sweat Resistant")
g5=rbind(pore,outdoor,resistant)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g5$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.7), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Paired"))



rm(list = ls())
#dark circle & puffy eyes
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
dark=cosmetics %>% filter(cosmetics$Category == "Dark Circle")
puffy=cosmetics %>% filter(cosmetics$Category == "Puffy Eyes")
g5=rbind(dark,puffy)
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(g5$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 50, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))



####
rm(list = ls())
#normal
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
normal=cosmetics %>% filter(cosmetics$Skin.Type == "Normal")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(normal$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Accent"))


rm(list = ls())
#sensitive
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
sensitive=cosmetics %>% filter(cosmetics$Skin.Type == "Sensitive")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(sensitive$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Set1"))


rm(list = ls())
#dry
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
dry=cosmetics %>% filter(cosmetics$Skin.Type == "Dry")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(dry$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Paired"))


rm(list = ls())
#oily
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
oily=cosmetics %>% filter(cosmetics$Skin.Type == "Oily")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(oily$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Set1"))






rm(list = ls())
#vegan
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
vegan=cosmetics %>% filter(cosmetics$Vegan == "Yes")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(vegan$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Accent"))





rm(list = ls())
#not vegan
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
nvegan=cosmetics %>% filter(cosmetics$Vegan == "No")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(nvegan$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,10)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))



rm(list = ls())
#all
#reading data
cosmetics<-read.csv("C:/Users/AMRITA/OneDrive/Pictures/Project/cosmetics1.csv")
#creating corpus
textcorpus<-VCorpus(VectorSource(as.vector(cosmetics$Main.Ingredients)))
#cleaning the data
toSpace<-content_transformer(function(x)gsub(","," ", x))
textcorpus<-tm_map(textcorpus,toSpace)
#transforming in matrix
textcorpus_dtm<-TermDocumentMatrix(textcorpus)
dtm_m<-as.matrix(textcorpus_dtm)
dtm_v<-sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d<-data.frame(word = names(dtm_v),freq = dtm_v)
head(dtm_d,20)
set.seed(123)
wordcloud(words = dtm_d$word, freq = dtm_d$freq,scale=c(2,.6), min.freq = 1,
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))

