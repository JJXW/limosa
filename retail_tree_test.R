library(rpart)
library(dplyr)
library(visTree)
library(rattle)
library(party)		
library(caret)
library(rpart.plot)	
library(rpart.utils)
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

model1_data <- model1$frame
model1_ylevels <- attr(model1, "ylevels")

model1_node_data <- as.data.frame(model1_data$yval2)
model1_node_data <- cbind(c(1:nrow(model1_node_data)), model1_node_data)


names(model1_node_data) <- c('node',
                             'var', 
                             paste0(model1_ylevels[1], '_nobs'),
                             paste0(model1_ylevels[2], '_nobs'),
                             paste0(model1_ylevels[3], '_nobs'),
                             paste0(model1_ylevels[1], '_pct'),
                             paste0(model1_ylevels[2], '_pct'),
                             paste0(model1_ylevels[3], '_pct'),
                             'pop_pct'
)



model1_node_data_melt <- melt(model1_node_data[,c(1, 6:8)], 
                              id.vars = 'node',
                              measure = c('$20 to less than $50_pct',
                                          '$50 to less than $100_pct',
                                          'Less than $20_pct'))

ggplot() + geom_bar(aes(y = value, 
                        x = node, 
                        fill = variable), data = model1_node_data_melt,
                    stat="identity")

model1_node_data_chart <- gvisColumnChart(model1_node_data[,c(5:7)],
                                          options=)
plot(model1_node_data_chart)
