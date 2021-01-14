library(tidyverse)
library(caret)
library(rsample)
library(ggthemes)

### load the data
data(iris)
df_raw <- iris %>% as_tibble()
df_raw %>% glimpse()
df_raw %>% count(Species)

### split the data
split <- df_raw %>% 
  initial_split(prop = 0.7)

train_data <- training(split)
test_data <- testing(split)

# viz
facet <- ggplot(data=train_data, aes(Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point(aes(shape=Species), size=1.5) + 
  geom_smooth(method="lm") +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Faceting") +
  theme_fivethirtyeight() +
  facet_grid(. ~ Species)

print(facet)

### build a model

set.seed(1234)
model_01 <- rpart::rpart(Species~., train_data)

print(model_01)

plot(model_01, margin = 0.1)
text(model_01)

## Predictions on train dataset
test_data$pred <- predict(object = model_01, newdata = test_data, type="class")
test_data %>%
  count(Species, pred)

## Checking the accuracy using a confusion matrix by comparing predictions to actual classifications
pred_result <- confusionMatrix(test_data$pred, test_data$Species)
pred_result$table




