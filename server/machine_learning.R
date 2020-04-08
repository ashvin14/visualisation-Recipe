library(arules)

mydata <- c(2,5)
grocery_new <- recipe_data[mydata]

write.csv(grocery_new,'basket.csv',quote = F,row.names=F)

transaction_data <-read.transactions('basket.csv',quote = "",sep=',')

frequentItems <- eclat (transaction_data, parameter = list(supp = 0.07, maxlen = 15))

association.rules<-apriori(transaction_data,parameter=list(supp=0.001,conf=0.8,minlen=3,maxlen=10))

rules_df <- DATAFRAME(association.rules, setStart='', setEnd='', separate = TRUE)

