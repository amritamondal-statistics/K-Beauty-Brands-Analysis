#use write.csv path where you want to

#detoxification group
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)
cleanser=cosmetics %>% filter(cosmetics$cs.Product.Type == "Cleanser")
cleansing_oil=cosmetics %>% filter(cosmetics$cs.Product.Type == "Cleansing Oil")
exfoliator=cosmetics %>% filter(cosmetics$cs.Product.Type == "Exfoliator")
cleanser1=rbind(cleanser,cleansing_oil,exfoliator)
write.csv(cleanser1,"C:/Users/AMRITA/OneDrive/Pictures/Project/cleanser1.csv")

table(cleanser1$cs.Skin.Type)
table(cleanser1$cs.Category)

library(MASS)
table1=table(cleanser1$cs.Vegan,cleanser1$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  cleanser1, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Oranges",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(cleanser1$cs.Skin.Type,cleanser1$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  cleanser1, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Pastel2",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(cleanser1$cs.Skin.Type,cleanser1$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  cleanser1, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set3",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table4=table(cleanser1$cs.Product.Type,cleanser1$cs.Product.Formulation)
table4
chisq.test(table4)$expected
library(stats)
test <- fisher.test(table4,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  cleanser1, cs.Product.Formulation, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set2",
  xlab="Product Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table5=table(cleanser1$cs.Product.Type,cleanser1$cs.Category)
table5
chisq.test(table5)$expected
library(stats)
test <- fisher.test(table5,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  cleanser1, cs.Category, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Pastel1",
  xlab="Product Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)


#target specific mask group
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)
mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Mask")
night_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Night Mask")
peel_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Peel Mask")
clay_mask=cosmetics %>% filter(cosmetics$cs.Product.Type == "Clay Mask")
mask1=rbind(mask,night_mask,peel_mask,clay_mask)
write.csv(mask1,"C:/Users/AMRITA/OneDrive/Pictures/Project/mask1.csv")

table(mask1$cs.Product.Type,mask1$cs.Mineral.Oil)
table(mask1$cs.Product.Type,mask1$cs.Vegan)
table(mask1$cs.Product.Type,mask1$cs.Fragrance)
table(mask1$cs.Product.Type,mask1$cs.Paraben)
table(mask1$cs.Skin.Type)
table(mask1$cs.Category)
library(MASS)
table1=table(mask1$cs.Vegan,mask1$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  mask1, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Greens",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(mask1$cs.Skin.Type,mask1$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  mask1, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Accent",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(mask1$cs.Skin.Type,mask1$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  mask1, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Paired",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table4=table(mask1$cs.Product.Type,mask1$cs.Product.Formulation)
table4
chisq.test(table4)$expected
library(stats)
test <- fisher.test(table4,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  mask1, cs.Product.Formulation, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "PiYG",
  xlab="Product Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table5=table(mask1$cs.Product.Type,mask1$cs.Category)
table5
chisq.test(table5)$expected
library(stats)
test <- fisher.test(table5,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  mask1, cs.Category, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set3",
  xlab="Product Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)



#skin balancing solution
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)
toner=cosmetics %>% filter(cosmetics$cs.Product.Type =="Toner")
essence=cosmetics %>% filter(cosmetics$cs.Product.Type == "Essence")
toner=rbind(toner,essence)
write.csv(toner,"C:/Users/AMRITA/OneDrive/Pictures/Project/toner.csv")


library(MASS)
table1=table(toner$cs.Vegan,toner$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  toner, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Blues",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(toner$cs.Skin.Type,toner$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  toner, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set2",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(toner$cs.Skin.Type,toner$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  toner, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set3",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table4=table(toner$cs.Product.Type,toner$cs.Product.Formulation)
table4
chisq.test(table4)$expected
library(stats)
test <- fisher.test(table4,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  toner, cs.Product.Formulation, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Spectral",
  xlab="Product Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table5=table(toner$cs.Product.Type,toner$cs.Category)
table5
chisq.test(table5)$expected
library(stats)
test <- fisher.test(table5,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  toner, cs.Category, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Paired",
  xlab="Product Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)


#treatment potion
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)
serum1=cosmetics %>% filter(cosmetics$cs.Product.Type == "Serum")
write.csv(serum1,"C:/Users/AMRITA/OneDrive/Pictures/Project/serum1.csv")

table(serum1$cs.Company.Name)
table(serum1$cs.Skin.Type)
table(serum1$cs.Category)
table(serum1$cs.Mineral.Oil)
table(serum1$cs.Vegan)
table(serum1$cs.Fragrance)
table(serum1$cs.Paraben)
library(MASS)
table1=table(serum1$cs.Vegan,serum1$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  serum1, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Pastel2",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(serum1$cs.Skin.Type,serum1$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  serum1, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Accent",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(serum1$cs.Skin.Type,serum1$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  serum1, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Pastel1",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)
#skin conditioning group
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)
moisturizer=cosmetics %>% filter(cosmetics$cs.Product.Type == "Moisturizer")
emulsion=cosmetics %>% filter(cosmetics$cs.Product.Type == "Emulsion")
moisturizer1=rbind(moisturizer,emulsion)
write.csv(moisturizer1,"C:/Users/AMRITA/OneDrive/Pictures/Project/moisturizer1.csv")

table(moisturizer1$cs.Skin.Type)
table(moisturizer1$cs.Category)

library(MASS)
table1=table(moisturizer1$cs.Vegan,moisturizer1$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  moisturizer1, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Reds",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(moisturizer1$cs.Skin.Type,moisturizer1$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  moisturizer1, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Accent",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(moisturizer1$cs.Skin.Type,moisturizer1$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  moisturizer1, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Paired",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table4=table(moisturizer1$cs.Product.Type,moisturizer1$cs.Product.Formulation)
table4
chisq.test(table4)$expected
library(stats)
test <- fisher.test(table4,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  moisturizer1, cs.Product.Formulation, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "PiYG",
  xlab="Product Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table5=table(moisturizer1$cs.Product.Type,moisturizer1$cs.Category)
table5
chisq.test(table5)$expected
library(stats)
test <- fisher.test(table5,workspace = 2e8)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  moisturizer1, cs.Category, cs.Product.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set3",
  xlab="Product Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)


#skin protective shield
rm(list = ls())
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cs<-read.csv(url,stringsAsFactors = FALSE)
library(dplyr)
cosmetics=data.frame(cs$Product.Name,cs$Category,cs$Product.Formulation,
                     cs$Skin.Type,cs$Fragrance,cs$Vegan,
                     cs$Mineral.Oil,cs$Paraben,cs$Company.Name,
                     cs$Smallest.Quantity,cs$Price.1,
                     cs$Total.Reviews,cs$Product.Type)

sunscreen1=cosmetics %>% filter(cosmetics$cs.Product.Type == "Sunscreen")
write.csv(sunscreen1,"C:/Users/AMRITA/OneDrive/Pictures/Project/sunscreen1.csv")

table(sunscreen1$cs.Company.Name)
table(sunscreen1$cs.Skin.Type)
table(sunscreen1$cs.Category)
table(sunscreen1$cs.Mineral.Oil)
table(sunscreen1$cs.Vegan)
table(sunscreen1$cs.Fragrance)
table(sunscreen1$cs.Paraben)

library(MASS)
table1=table(sunscreen1$cs.Vegan,sunscreen1$cs.Skin.Type)
table1
chisq.test(table1)$expected
library(stats)
test <- fisher.test(table1)
test
library(RColorBrewer)
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  sunscreen1, cs.Vegan, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "PRGn",
  xlab="Skin Types", ylab="Labels", legend.title = "Label",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table2=table(sunscreen1$cs.Skin.Type,sunscreen1$cs.Product.Formulation)
table2
chisq.test(table2)$expected
library(stats)
test <- fisher.test(table2,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  sunscreen1, cs.Product.Formulation, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Accent",
  xlab="Skin Types", ylab="Product Formulations", legend.title = "Formulations",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

table3=table(sunscreen1$cs.Skin.Type,sunscreen1$cs.Category)
table3
chisq.test(table3)$expected
library(stats)
test <- fisher.test(table3,simulate.p.value = TRUE)
test
# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  sunscreen1, cs.Category, cs.Skin.Type,
  results.subtitle = FALSE,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Set3",
  xlab="Skin Types", ylab="Product Specializations", legend.title = "Category",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.05, "< 0.05", round(test$p.value, 3))
  )
)

