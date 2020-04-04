
library(stringr)
recipe_dataset <- read.csv("data/epi_r.csv")

molten_filtered_recipe_dataset2 <- read.csv("data/epi_r2.csv")

recipe_csv <- read.csv("data/final_data.csv")

recipe_data <- recipe_csv[c(3, 5, 6, 7, 9, 10, 13, 14, 16, 20)]

colnames(recipe_data) <- c('url', 'title', 'id', 'instructions', 'ingredient_list',
                          'fsa_lights_per_100gm', 'nutr_per_ingr',
                          'nutr_per_100gm', 'quantity', 'weight_per_ingr')

head(recipe_data)

recipe_data$ingredient_list[1]

pattern = "\\[\\{'text': "
ingredients <- str_remove_all(recipe_data$ingredient_list, pattern)

pattern = "\\{'text'"
ingredients <- str_remove_all(ingredients, pattern)

pattern = ": "
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\}"
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\]"
ingredients <- str_remove_all(ingredients, pattern)

recipe_data$ingredient_list<- noquote(ingredients)





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