

# recipe_dataset <- read.csv("data/epi_r.csv")
# 
# molten_filtered_recipe_dataset2 <- read.csv("data/epi_r2.csv")

recipe_csv <- read.csv("data/final_data.csv")

recipe_data <- recipe_csv[c(3, 5, 6, 7, 9, 10, 13, 14, 16,18, 20)]

colnames(recipe_data) <- c('url', 'title', 'id', 'instructions', 'ingredient_list',
                          'fsa_lights_per_100gm', 'nutr_per_ingr',
                          'nutr_per_100gm', 'quantity','units', 'weight_per_ingr')

head(recipe_data)

recipe_data$ingredient_list[1]

pattern = "\\[\\{'text': "
ingredients <- str_remove(recipe_data$ingredient_list, pattern)

pattern = "\\{'text'"
ingredients <- str_remove_all(ingredients, pattern)

pattern = ": "
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\}"
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\]"
ingredients <- str_remove_all(ingredients, pattern)
pattern = "'"
ingredients <- str_remove_all(ingredients, pattern)

recipe_data$ingredient_list<- noquote(ingredients)



pattern = "\\[\\{'text': "
Quantity <- str_remove(recipe_data$quantity, pattern)

pattern = "\\{'text'"
Quantity <- str_remove_all(Quantity, pattern)

pattern = ": "
Quantity <- str_remove_all(Quantity, pattern)

pattern = "\\}"
Quantity <- str_remove_all(Quantity, pattern)

pattern = "\\]"
Quantity <- str_remove_all(Quantity, pattern)

pattern = "'"
Quantity <- str_remove_all(Quantity, pattern)

recipe_data$quantity<- noquote(Quantity)


pattern = "\\["
Weight <- str_remove(recipe_data$weight_per_ingr, pattern)
pattern = "\\]"
Weight <- str_remove_all(Weight, pattern)

recipe_data$weight_per_ingr<- noquote(Weight)



## Nutrition table
pattern="\\[\\{"

nutrition<-str_remove_all(recipe_data$nutr_per_ingr,pattern)

pattern='\\}'

nutrition<-str_remove_all(nutrition,pattern)

pattern='\\{'

nutrition<-str_remove_all(nutrition,pattern)

pattern='\\]'

nutrition<-str_remove_all(nutrition,pattern)

recipe_data$nutr_per_ingr<-noquote(nutrition)

nutritiontable<-data.frame('fat'=integer(),'nrg'=integer(),'pro'=integer(),'sat'=integer(),'sod'=integer(),'sug'=integer(),'fat'=integer())

for (i in (1:length(recipe_data$nutr_per_ingr)))
{
  new<-strsplit(recipe_data$nutr_per_ingr[i],',')
  new<-new[[1]]
  new<-noquote(new)
  df<-data.frame(new)
  df<-df%>%separate(new,c('content','value'),sep=':')
  df[,1]<-(gsub("'",'',df[,1]))
  df[,2]<-as.numeric(df[,2])
  df<-df%>%group_by(content)%>%summarize(value=sum(value))
  df<-pivot_wider(df,names_from = content,values_from = value)
  df<-df%>%rename('fat.1'='fat')
  nutritiontable[nrow(nutritiontable)+1,1:7]<-df
}



options(scipen=999)
nutritiontable<-round(nutritiontable,2)
rownames(nutritiontable)<-NULL
recipe_data<-cbind(recipe_data,nutritiontable)
recipe_data<-recipe_data[,c(-7,-18)]
recipe_data<-recipe_data%>%rename('Fat'='fat','Energy'='nrg','Protein'='pro','Saturated fat'='sat','Sodium'='sod','Sugar'='sug')
recipe_data




# scroll <- image_read("data/scroll.png")
# 
# #print(scroll)
# scroll <- image_scale(scroll, "300")
# scroll <- image_scale(scroll, "x300")
# 
# list_directions <- json_data[[1]]["title"]
# text_directions <- paste(list_directions, collapse = "\n")
# 
# print(text_directions)
# 
# image_annotate(scroll, text_directions, size = 15, color = "green",
#                location = "+20+100")


