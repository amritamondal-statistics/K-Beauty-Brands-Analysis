rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Company.Name,cs$Product.Name,cs$Rating,cs$Total.Reviews,
                     cs$Product.Type,cs$Vegan,cs$Category)

moisturizer=cosmetics %>% filter(cosmetics$cs.Product.Type == "Moisturizer")
emulsion=cosmetics %>% filter(cosmetics$cs.Product.Type == "Emulsion")
moisturizer=rbind(moisturizer,emulsion)
bayesian_rating=(((mean(moisturizer$cs.Rating)*mean(moisturizer$cs.Total.Reviews))+(moisturizer$cs.Rating*moisturizer$cs.Total.Reviews)))/(mean(moisturizer$cs.Total.Reviews)+moisturizer$cs.Total.Reviews)
moisturizer1=cbind(moisturizer,bayesian_rating)
moisturizer1[order(moisturizer1$bayesian_rating,decreasing=TRUE),]


toner=cosmetics %>% filter(cosmetics$cs.Product.Type =="Toner")
essence=cosmetics %>% filter(cosmetics$cs.Product.Type == "Essence")
toner=rbind(toner,essence)
bayesian_rating=(((mean(toner$cs.Rating)*mean(toner$cs.Total.Reviews))+(toner$cs.Rating*toner$cs.Total.Reviews)))/(mean(toner$cs.Total.Reviews)+toner$cs.Total.Reviews)
toner1=cbind(toner,bayesian_rating)
toner1[order(toner1$bayesian_rating,decreasing=TRUE),]


mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Mask")
night_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Night Mask")
peel_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Peel Mask")
clay_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Clay Mask")
mask=rbind(mask,night_mask,peel_mask,clay_mask)
bayesian_rating=(((mean(mask$cs.Rating)*mean(mask$cs.Total.Reviews))+(mask$cs.Rating*mask$cs.Total.Reviews)))/(mean(mask$cs.Total.Reviews)+mask$cs.Total.Reviews)
mask1=cbind(mask,bayesian_rating)
mask1[order(mask1$bayesian_rating,decreasing=TRUE),]


cleanser=cosmetics %>% filter(cosmetics$cs.Product.Type == "Cleanser")
cleansing_oil=cosmetics %>% filter(cosmetics$cs.Product.Type == "Cleansing Oil")
exfoliator=cosmetics %>% filter(cosmetics$cs.Product.Type == "Exfoliator")
cleanser=rbind(cleanser,cleansing_oil,exfoliator)
bayesian_rating=(((mean(cleanser$cs.Rating)*mean(cleanser$cs.Total.Reviews))+(cleanser$cs.Rating*cleanser$cs.Total.Reviews)))/(mean(cleanser$cs.Total.Reviews)+cleanser$cs.Total.Reviews)
cleanser1=cbind(cleanser,bayesian_rating)
cleanser1[order(cleanser1$bayesian_rating,decreasing=TRUE),]


serum=cosmetics %>% filter(cosmetics$cs.Product.Type == "Serum")
bayesian_rating=(((mean(serum$cs.Rating)*mean(serum$cs.Total.Reviews))+(serum$cs.Rating*serum$cs.Total.Reviews)))/(mean(serum$cs.Total.Reviews)+serum$cs.Total.Reviews)
serum1=cbind(serum,bayesian_rating)
serum1[order(serum1$bayesian_rating,decreasing=TRUE),]


sunscreen=cosmetics %>% filter(cosmetics$cs.Product.Type == "Sunscreen")
bayesian_rating=(((mean(sunscreen$cs.Rating)*mean(sunscreen$cs.Total.Reviews))+(sunscreen$cs.Rating*sunscreen$cs.Total.Reviews)))/(mean(sunscreen$cs.Total.Reviews)+sunscreen$cs.Total.Reviews)
sunscreen1=cbind(sunscreen,bayesian_rating)
sunscreen1[order(sunscreen1$bayesian_rating,decreasing=TRUE),]

lip=cosmetics %>% filter(cosmetics$cs.Product.Type == "Lip Mask")
bayesian_rating=(((mean(lip$cs.Rating)*mean(lip$cs.Total.Reviews))+(lip$cs.Rating*lip$cs.Total.Reviews)))/(mean(lip$cs.Total.Reviews)+lip$cs.Total.Reviews)
lip1=cbind(lip,bayesian_rating)
lip1[order(lip1$bayesian_rating,decreasing=TRUE),]

eye=cosmetics %>% filter(cosmetics$cs.Product.Type == "Eye Cream")
bayesian_rating=(((mean(eye$cs.Rating)*mean(eye$cs.Total.Reviews))+(eye$cs.Rating*eye$cs.Total.Reviews)))/(mean(eye$cs.Total.Reviews)+eye$cs.Total.Reviews)
eye1=cbind(eye,bayesian_rating)
eye1[order(eye1$bayesian_rating,decreasing=TRUE),]

cosmetics2=rbind(moisturizer1,toner1,serum1,mask1,sunscreen1,lip1,eye1,cleanser1)


colnames(cosmetics2) = c('Product.Name','Rating','Total.Reviews','Product.Type','Bayesian Rating')
cosmetics2 = cosmetics2[,-2:-4]
cosmetics1=full_join(cs, cosmetics2, by = "Product.Name") 


summary(cs$Product.Name)
summary(cosmetics2$Product.Name)
#save where you want to, the new path
write.csv(cosmetics1,"C:\\Users\\AMRITA\\OneDrive\\Pictures\\Project\\cosmetics1.csv")
