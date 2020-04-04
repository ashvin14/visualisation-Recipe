
<<<<<<< HEAD

recipe_dataset <- read.csv("data/epi_r.csv")

=======
library(stringr)
recipe_dataset <- read.csv("data/epi_r.csv")

>>>>>>> master
molten_filtered_recipe_dataset2 <- read.csv("data/epi_r2.csv")

recipe_csv <- read.csv("data/final_data.csv")

<<<<<<< HEAD
recipe_data <- recipe_csv[c(3, 5, 6, 7, 9, 10, 13, 14, 16,18, 20)]

colnames(recipe_data) <- c('url', 'title', 'id', 'instructions', 'ingredient_list',
                          'fsa_lights_per_100gm', 'nutr_per_ingr',
                          'nutr_per_100gm', 'quantity','units', 'weight_per_ingr')
=======
recipe_data <- recipe_csv[c(3, 5, 6, 7, 9, 10, 13, 14, 16, 20)]

colnames(recipe_data) <- c('url', 'title', 'id', 'instructions', 'ingredient_list',
                          'fsa_lights_per_100gm', 'nutr_per_ingr',
                          'nutr_per_100gm', 'quantity', 'weight_per_ingr')
>>>>>>> master

head(recipe_data)

recipe_data$ingredient_list[1]

pattern = "\\[\\{'text': "
<<<<<<< HEAD
ingredients <- str_remove(recipe_data$ingredient_list, pattern)
=======
ingredients <- str_remove_all(recipe_data$ingredient_list, pattern)
>>>>>>> master

pattern = "\\{'text'"
ingredients <- str_remove_all(ingredients, pattern)

pattern = ": "
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\}"
ingredients <- str_remove_all(ingredients, pattern)

pattern = "\\]"
ingredients <- str_remove_all(ingredients, pattern)
<<<<<<< HEAD
pattern = "'"
ingredients <- str_remove_all(ingredients, pattern)
=======
>>>>>>> master

recipe_data$ingredient_list<- noquote(ingredients)


<<<<<<< HEAD
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

=======
>>>>>>> master



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