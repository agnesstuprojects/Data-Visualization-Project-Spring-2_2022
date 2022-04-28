Apartment Prices in USA 2021 
The real estate markets, present an interesting opportunity for data analysts to analyze and predict where property prices are moving towards. 
Prediction of property prices is becoming increasingly important and beneficial. 
Property prices are a good indicator of both the overall market condition and the economic health of a country. 
Considering the data provided, we are wrangling a large set of property sales records stored in an unknown format and with unknown data quality issues
---
title: "Data Visualization Final Project"
name: "Agnes Sithole St Thomas University_Miami_2022"
topic: "Appartment price USA"
output: html_notebook
---

#Libraries are in a seperate file

```{r}
apartments <- read_csv("apartment_price_2021.csv")
head(apartments)
```

# Apartments bedrooms vs bathrooms by City
```{r}
p <- ggplot(data = apartments, aes(x=bedrooms,y=bathrooms,fill=city))+
  geom_bar(stat = "identity")+ guides(fill = FALSE)
    theme(axis.text.x = element_text(colour = "black", size = 10,angle=90),
        axis.text.y = element_text(colour = "black", size = 10))
p+theme()
```


# Average living area in Sqft is 2139 sqft
```{r}
apartments %>% summarise(average_living_area = mean(sqft_living))
```

#Distribution of Bedrooms
```{r}
apartments %>%
  group_by(bedrooms) %>%
  count()
ggplot(apartments)+
  geom_bar(mapping = aes(x = bedrooms), fill = "#c86ca0") +
  labs(
    title = "Distribution of Appartments on the basis of No of Bedrooms",
    x = "No. of Bedrooms",
    y = "Count"
  ) +
  ggthemes::theme_few()
```

# Comparitive distribution of apartments in Seattle and Bellevue on the basis of number of Bedrooms in each Appartment

```{r}
apartments %>%
  filter(city %in% c("Seattle","Bellevue")) %>%
ggplot() +
  geom_density(aes(x = bedrooms, fill = city)) +
  labs(
    title = "Comparitive distribution of Apartments in Seattle and Redmond",
    x = "No. of Bedrooms",
    y = "Distribution"
  ) +
  ggthemes::theme_few()
```

```{r}
#shapiro test for normality
shapiro.test(apartments$bedrooms)
##pvalue is low, we reject the null hypothesis : it is not normal
```

# Top 10 Cities in USA with Rental Apartments and the number of Apartments in those cities.
```{r}
apartments %>%
  group_by(city) %>%
  count(sort = TRUE) %>%
  head(10) %>%
  ggplot() +
  geom_col(aes(x = n, y = city, fill = city)) +
  coord_flip() + guides(fill = FALSE)
  labs(
    title = "Number of Rental Apartments in Cities",
    x = "Number of Apartments",
    y = "Cities"
  ) +
  ggthemes::theme_few() -> top10cities
```

# Relationship between Price of Appartment and Living area
```{r}
apartments %>%
  select(c("sqft_living", "price")) %>%
cor()
```


```{r}
apartments %>%
  select(c("sqft_living", "price", "city")) %>%
  ggplot(aes(x = price, y = sqft_living)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "Is area of living and price correlated?  YES") +
  ggthemes::theme_calc()
```


#Simple ANN with only a sigle hidden neuron
```{r}
## Prepare Training and Testing Sets
set.seed(12345)
# Split the data
sample <- sample.split(apartments$bedrooms, SplitRatio = .75)
train <- subset(apartments, sample == TRUE)
test <- subset(apartments, sample == FALSE)
model <- neuralnet(formula = bedrooms ~ price + bathrooms + sqft_living + sqft_lot + waterfront + view + condition + sqft_above + sqft_basement, data = train)
#Visualize the network topology
plot(model)
```

#Compute values
```{r}
#Predict strength (index 9) when given [1:8]
model_results <- neuralnet::compute(model, test[1:9])
#Obtain predicted strength values
predicted_bedrooms <- model_results$net.result
#Examine the correlation between predicted and actual values
ann_result <- cor(predicted_bedrooms,test$bedrooms)
ann_result
```

#5 Neurons for the hidden layer
```{r}
set.seed(12345)
model2 <- neuralnet(formula = bedrooms ~ price + bathrooms + sqft_living + sqft_lot + waterfront + view + condition + sqft_above + sqft_basement, data = train,hidden = 5)
#Visualize the network topology
plot(model2)
```

```{r}
#Compute values and test correlation
#Predict strength (index 9) when given [1:9]
model_results2 <- neuralnet::compute(model2, test[1:9])
#Obtain predicted strength values
predicted_bedrooms2 <- model_results2$net.result
#Examine the correlation between predicted and actual values
ann_result2 <- cor(predicted_bedrooms2,test$bedrooms) 
ann_result2
```

##Examine normalized data to unnormalized values(actual)
```{r}
#note that the predicted and actual values are on different scales
bedrooms <- data.frame ( 
  actual = apartments$bedrooms,
  pred = round(predicted_bedrooms2))
head(bedrooms, n = 3)
```

#Classification Analysis
#Naive Bayes Analysis
```{r}
## Prepare Training and Testing Sets
set.seed(12345)
# Split the data
sample <- sample.split(apartments$bedrooms, SplitRatio = .75)
train <- subset(apartments, sample == TRUE)
test <- subset(apartments, sample == FALSE)
# Build the naiveBayes classifier
nb_model <- naiveBayes(bedrooms~., data = train)
# Predict the class on the testing set
nb_prediction <- predict(nb_model, test, type = "class")
# Confusion Matrix
nb_prediction_confusion <- table(nb_prediction, test$bedrooms)
# Accuracy is the overall success rate of the model
naivebayesaccuracy <- sum(diag(nb_prediction_confusion)/sum(nb_prediction_confusion))
cat("Naive Bayes Accuracy:", naivebayesaccuracy, "\n")
# LaPlace Smoothing
## Laplace parameter adds a positive integer value to every class to remove zero in the probability calculation
laplace_model <- naiveBayes(bedrooms~.,data = train, laplace = 1)
laplace_prediction <- predict(laplace_model, test, type = "class")
# Calculate Accuracy
laplace_results <- data_frame(Actual = test$bedrooms, Prediction = laplace_prediction)
accurateRows <- nrow(subset(laplace_results, Actual == Prediction))
laplaceaccuracy <- accurateRows/nrow(test)
cat("Laplace Accuracy:", laplaceaccuracy)
```

Naive Bayes and Laplace may not be the best method to classify this data, because 0.21 accuracy, might be too low.I would recommend to further analyze using other models then choose the best method of study.


#Linear Model
 A Multivariate Linear Regression Model that predicts number of bedrooms depending on the price.bathrooms and sqft_living area
```{r}
## Prepare Training and Testing Sets
set.seed(12345)
# Split the data
sample <- sample.split(apartments$bedrooms, SplitRatio = .75)
train <- subset(apartments, sample == TRUE)
test <- subset(apartments, sample == FALSE) 
# Build the model with the training set
model <- lm(bedrooms ~ price + bathrooms + sqft_living, data = train)
# Predict with the testing set
predictedbedrooms4 <- predict(model,test)
# Create a data frame predicted bedrooms and the actual bedrooms
bedroomsPrediction <- data.frame(Price = test$price, 
                              Bathrooms = test$bathrooms,
                              Sqft_living = test$sqft_living,
                              ActualBedrooms = test$bedrooms,
                              PredictedBedrooms4 = round(predictedbedrooms4))
head(bedroomsPrediction, n = 3)
write.csv(bedroomsPrediction, file = "BedroomsPredvsActual.csv")
```

#Linear Model Error Metrices
```{r}
mae <- mae(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Mean Absolute Error:",mae,"\n")
mse <- mse(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Mean Squared Error:",mse,"\n")
rmse <- rmse(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Root mean square error:",rmse,"\n")
```
### Which model is the best and Why?
RMSE is the best model. It has the least error without being overfitted and uses the least amount of variables for prediction which means less computing power is needed to process the model prediction.

#Create a Results table for all models used
```{r}
# Create a data frame predicted Gold Medals and the Actual Gold
Results <- data.frame(ann_single_neuron = ann_result, 
                      ann_5_hidden_neurons = ann_result2,
                      naivebayes_accuracy = naivebayesaccuracy,
                      laplace_accuracy = laplaceaccuracy,
                      lm_mae = mae,
                      lm_mse = mse,
                      lm_rmse = rmse)
Results
write.csv(Results, file = "modelResults.csv")
```
