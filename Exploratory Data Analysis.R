library(dplyr)
library(ggplot2)

fastfoods = read.csv(file.choose(),header = TRUE)

## AVERAGE NUTRITIONAL VALUES BY RESTAURANT 
  
average_nutrition <- fastfoods %>%
           group_by(restaurant) %>%
          summarise(Avg_Calories = mean(calories), 
                    Avg_TotalFat = mean(total_fat),
                    Avg_TotalCarb = mean(total_carb),
                    Avg_Protein = mean(protein), 
                    Avg_satfat = mean(sat_fat), 
                    Avg_transfat = mean(trans_fat), 
                    Avg_cholesterol = mean(cholesterol), 
                    Avg_transfat = mean(trans_fat), 
                    Avg_sodium = mean(sodium), 
                    Avg_fiber = mean(fiber),
                    Avg_sugar = mean(sugar),
                    Avg_protein = mean(protein), 
                    Avg_vita = mean(vit_a),
                    Avg_vitc = mean(vit_c),
                    Avg_calcium = mean(calcium))

## CLASSIFICATION OF RESTAURANTS BASED ON NUTRITIONAL VALUES //**

average_nutrition_r <- average_nutrition%>%
      mutate(Calorie_Class = ifelse(Avg_Calories > quantile(Avg_Calories, 0.75), "High Calorie",
                             ifelse(Avg_Calories < quantile(Avg_Calories, 0.25), "Low Calorie", "Medium Calorie")),
             
             Fat_Class = ifelse(Avg_TotalFat > quantile(Avg_TotalFat, 0.75), "High Fat",
                         ifelse(Avg_TotalFat < quantile(Avg_TotalFat, 0.25), "Low Fat", "Medium Fat")),
            
             Carb_Class = ifelse(Avg_TotalCarb > quantile(Avg_TotalCarb, 0.75), "High Carbs",
                         ifelse(Avg_TotalCarb < quantile(Avg_TotalCarb, 0.25), "Low Carbs", "Medium Carbs")),
            
             Protein_Class = ifelse(Avg_Protein > quantile(Avg_Protein, 0.75), "High Protein",
                            ifelse(Avg_Protein < quantile(Avg_Protein, 0.25), "Low Protein", "Medium Protein")),
            
             sugar_class = ifelse(Avg_sugar > quantile(Avg_sugar, 0.75), "High Sugar",
                          ifelse(Avg_sugar < quantile(Avg_sugar, 0.25), "Low Sugar", "Medium Sugar")),  
            
             cholestrol_class = ifelse(Avg_cholesterol > quantile(Avg_cholesterol, 0.75), "High Cholesterol",
                              ifelse(Avg_cholesterol < quantile(Avg_cholesterol, 0.25), "Low Cholesterol", "Medium Cholesterol")))

## **// SIDE BY SIDE BAR PLOTS OF AVERAGE NUTRITIONAL VALUES //**

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_Calories), y = Avg_Calories)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Average Calories", title = "Average Calories per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_TotalFat), y = Avg_TotalFat)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Total Fat", title = "Total Fat per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_TotalCarb), y = Avg_TotalCarb)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Total Carb", title = "Total Carb per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_cholesterol), y = Avg_cholesterol)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Total Cholesterol", title = "Total Cholesterol per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_sugar), y = Avg_sugar)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Total Sugar", title = "Total Sugar per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_nutrition_r, aes(x = reorder(restaurant, -Avg_Protein), y = Avg_Protein)) +
       geom_bar(stat = "identity", fill = 'steelblue') +
       labs(x = "Restaurant", y = "Total Protein", title = "Total Protein per Restaurant") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
