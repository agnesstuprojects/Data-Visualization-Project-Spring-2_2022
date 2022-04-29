# Apartment Prices in USA 2021 

The real estate markets, present an interesting opportunity for data analysts to analyze and predict where property prices are moving towards. 
Prediction of property prices is becoming increasingly important and beneficial. 
Property prices are a good indicator of both the overall market condition and the economic health of a country. 
Considering the data provided, we are wrangling a large set of property sales records stored in an unknown format and with unknown data quality issues

# Libraries are in a seperate file
# Code in R Programming
# Read the data file
```{r}
apartments <- read_csv("apartment_price_2021.csv")
head(apartments)
```
![image](https://user-images.githubusercontent.com/95668215/165870549-6ee23fc7-c8ee-4d0a-b883-c01f89fb27cb.png)

# Apartment bedrooms vs bathrooms by City
```{r}
p <- ggplot(data = apartments, aes(x=bedrooms,y=bathrooms,fill=city))+
  geom_bar(stat = "identity")+ guides(fill = FALSE)
    theme(axis.text.x = element_text(colour = "black", size = 10,angle=90),
        axis.text.y = element_text(colour = "black", size = 10))
p+theme()
```
![image](https://user-images.githubusercontent.com/95668215/165869694-9e288ebe-2f40-4bfd-84b2-1bea29028614.png)

# Average living area in Sqft is 2139 sqft
```{r}
apartments %>% summarise(average_living_area = mean(sqft_living))
```
![image](https://user-images.githubusercontent.com/95668215/165870653-274c4b80-26f8-4ace-bcc2-726bf07b0fa5.png)

# Distribution of Bedrooms
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
![image](https://user-images.githubusercontent.com/95668215/165869751-6e6bd2ef-7c1f-4a92-83b0-b32d6a344b05.png)

# Comparitive distribution of apartments in Seattle and Bellevue on the basis of number of Bedrooms in each Apartment

```{r}
apartments %>%
  filter(city %in% c("Seattle","Bellevue")) %>%
ggplot() +
  geom_density(aes(x = bedrooms, fill = city)) +
  labs(
    title = "Comparitive distribution of Apartments in Seattle and Bellevue",
    x = "No. of Bedrooms",
    y = "Distribution"
  ) +
  ggthemes::theme_few()
```
![image](https://user-images.githubusercontent.com/95668215/165869935-1d5d8105-695b-4d0b-9ee3-18520785e4db.png)

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
![image](https://user-images.githubusercontent.com/95668215/165869989-f6b0639a-6da2-4efd-a9c9-12a8e612934b.png)

# Relationship between Price of Apartment and Living area
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
![image](https://user-images.githubusercontent.com/95668215/165870034-5c0d27b8-cbab-49e2-a046-0f0ec9fc9423.png)


# Simple ANN with only a sigle hidden neuron
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
![image](https://user-images.githubusercontent.com/95668215/165870079-f354620c-9d77-460c-bdd8-844598a113f4.png)

# Compute values
```{r}
#Predict strength (index 9) when given [1:8]
model_results <- neuralnet::compute(model, test[1:9])
#Obtain predicted strength values
predicted_bedrooms <- model_results$net.result
#Examine the correlation between predicted and actual values
ann_result <- cor(predicted_bedrooms,test$bedrooms)
ann_result
```
         [,1]
[1,] 0.03346162

# 5 Neurons for the hidden layer
```{r}
set.seed(12345)
model2 <- neuralnet(formula = bedrooms ~ price + bathrooms + sqft_living + sqft_lot + waterfront + view + condition + sqft_above + sqft_basement, data = train,hidden = 5)
#Visualize the network topology
plot(model2)
```
![image](https://user-images.githubusercontent.com/95668215/165870115-31d1b2ff-6050-4559-b7cf-95e85f214db2.png)

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
           [,1]
[1,] 0.01939913

# Examine normalized data to unnormalized values(actual)
```{r}
#note that the predicted and actual values are on different scales
bedrooms <- data.frame ( 
  actual = apartments$bedrooms,
  pred = round(predicted_bedrooms2))
head(bedrooms, n = 3)
```
![image](https://user-images.githubusercontent.com/95668215/165870204-30c8423f-6bf9-4c98-9e60-7b15e55989d4.png)

# Naive Bayes Analysis
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
# Result
Naive Bayes Accuracy: 0.2121739

Laplace Accuracy: 0.2182609

Naive Bayes and Laplace may not be the best method to classify this data, because 0.21 accuracy, might be too low.I would recommend to further analyze using other models then choose the best method of study.


# Linear Model
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
![image](https://user-images.githubusercontent.com/95668215/165870271-b3703219-1f2b-4bab-bd24-5a8dba907aa4.png)


# Linear Model Error Metrices
```{r}
mae <- mae(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Mean Absolute Error:",mae,"\n")
mse <- mse(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Mean Squared Error:",mse,"\n")
rmse <- rmse(bedroomsPrediction$ActualBedrooms,bedroomsPrediction$PredictedBedrooms4)
cat("Root mean square error:",rmse,"\n")
```
# Result
Mean Absolute Error: 0.486087 

Mean Squared Error: 0.5765217 

Root mean square error: 0.7592903 

# Which model is the best and Why?
RMSE is the best model. It has the least error without being overfitted and uses the least amount of variables for prediction which means less computing power is needed to process the model prediction.

# Create a Results table for all models used
```{r}
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
![image](https://user-images.githubusercontent.com/95668215/165870339-9344bfc3-fbee-4a60-aa0f-ef9e61c42428.png)


# Present Output in Shiny

# Shiny Output Link:    https://wolned-agnes-sithole.shinyapps.io/FinalProjectResults

# Libraries

library(shiny)

library(shinyWidgets)

library(tidyverse)

library(ggthemes)

library(ggplot2)

library(plotly)

library(DT)

# Read Data
apartments <- read.csv("apartment_price_2021.csv")

bedroomspred <- read.csv("BedroomsPredvsActual.csv")

bedroomspred1 <- bedroomspred[c(2,3,4,5,6)]

results <- read.csv("modelResults.csv")

# Define User Interface
ui <- fluidPage(
  titlePanel("Bedroom Prediction Results"),
  sliderInput(inputId = "ActualBedrooms",
              label = "Bedrooms:",
              min = 0,
              max = 8,
              value = 1),
  fluidRow(
    column(8)),
  
# Show a plot of the generated distribution
mainPanel(
  tableOutput("table"),
  plotlyOutput("plot1")
))


# Define server logic required to draw PLOT
  server <- function(input, output){
    output$table <- renderTable(results)
    output$plot1 <- renderPlotly({
        plot1 <- bedroomspred1 %>%  
        filter(ActualBedrooms == input$ActualBedrooms) %>% 
        ggplot() + 
        geom_point(mapping = aes(x = ActualBedrooms, 
                                 y = PredictedBedrooms4,
                                 text = paste(
                                   "Actual Bedrooms:", ActualBedrooms,
                                   "\nPredicted Bedrooms:", PredictedBedrooms4,
                                   "\nBathrooms:", Bathrooms,
                                   "\nSqft Living:", Sqft_living, 
                                   "\nPrice:", dollar(Price)))) + 
          labs(title = "Bedrooms Actual vs Prediction",
             x = "Actual",
             y = "Prediction")
        ggplotly(plot1)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

# Result
![image](https://user-images.githubusercontent.com/95668215/165867639-6276c877-dd1a-4640-b69d-949ac7121a27.png)

