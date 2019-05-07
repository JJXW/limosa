library(rpart)
library(dplyr)
library(visTree)
library(rattle)
library(party)		
library(caret)
library(rpart.plot)	
library(partykit)

retail_test <- read.csv('Dropbox/limosa/app_demo/Retail Example_flat.csv')

# column name cleaning

names(retail_test) <- c('id', 'shopping_method', 
                        'platform_engage', 'value_most', 
                        'shop_family', 'shop_per_month', 
                        'spend_per_visit', 'income_range',
                        'education', 'sex', 'location')

model1 <- rpart(spend_per_visit ~ location + sex, 
                retail_test)
rpart.plot(model1)
