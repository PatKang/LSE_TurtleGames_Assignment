# Assignment Activity for turtle sales

# Import the library
library(tidyverse)

# Set the working directory
setwd("C:/Users/patri/Desktop/LSE_CareerAccelerator/Course 3/Assignment")

# Import the data and the library
sales <- read.csv(file.choose(), header = T)

# Explore the data
view(sales)
typeof(sales)
summary(sales)

# We will remove the redundant data for this analysis. 
# Our main focus is to determine the product that drives sales 

## Remove the redundant columns [Ranking, Year, Genre, Publisher]
sales_2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

## Print the new dataframe
head(sales_2)
summary(sales_2)

# As the product is considered a number we need to convert it into characters
sales_2 <- mutate(sales_2,
                      Product = as.character(Product))

# Now we can create plots to reveal any insights or relationships within the data
qplot(NA_Sales, data = sales_2)
qplot(EU_Sales, data = sales_2)
qplot(Global_Sales, data = sales_2)
qplot(NA_Sales, EU_Sales, data = sales_2)

# To draw the European and North America sales in one boxplot.
# The reshape 2 package will be loaded to create a combined subset of the data in long format
library('reshape2')

# Create a subset of the data
sales_3 <- select(sales_2, NA_Sales, EU_Sales, Global_Sales)

sales_3long <- melt(sales_3)
summary(sales_3long)

ggplot(sales_3long, aes(x = value, y = variable))+
  geom_boxplot()+
  labs(title = 'Distribution of Sales', 
                        x = 'Sales in M £',
                        y = 'Sales by geographical Area')

# The NA sales are slightly higher in terms of the sales mean value

qplot(Platform, Global_Sales, data = sales_2)
qplot(Product, Global_Sales, data = sales_2)
# Show top ten products by sales...
top_products <- aggregate(x = sales_2$Global_Sales,
          by = list(sales_2$Product),
          FUN = sum)

names(top_products) <- c('ProductId', 'Global_Sales')

top_5products <- top_products %>% top_n(5)
top_5products

arrange(top_5products, desc(Global_Sales))
qplot(ProductId, Global_Sales, data = top_5products)
# The best selling products are 107, 515, 123, 254, 195


################################################################################
# Assignment 5: Clean, Manipulate and visualize the data

# To recap we will view the summary of our prepped dataset
summary(sales_2)
as_tibble(sales_2)

# Another column is created to contain the sales in other regions
sales_2 <- sales_2 %>%
  mutate(Other_Sales = Global_Sales - (NA_Sales + EU_Sales))
head(sales_2)
summary(sales_2)

# Now the impact on sales per productID will be determined
grouped_sales <- sales_2 %>% group_by(Product) %>% 
  summarise(Sum_NA_Sales = sum(NA_Sales),
            Sum_EU_Sales = sum(EU_Sales),
            Sum_Other_Sales = sum(Other_Sales),
            Sum_Global_Sales = sum(Global_Sales))

summary(grouped_sales)

arrange(grouped_sales, desc(Sum_Global_Sales))

grouped_sales <- grouped_sales %>% 
  mutate(percentage_of_total = Sum_Global_Sales / sum(Sum_Global_Sales))

head(grouped_sales)

# The summary of  the sales data shows us the degree to which they contribute
# To the overall sales numbers. 

# Now that there is a summary table, 
# The sales numbers will be investigated further with visualizations

# First the distribution of sales by location will be shown
sales_4 <- select(sales_2, NA_Sales, EU_Sales, Other_Sales, Global_Sales)
sales_4long <- melt(sales_4)
summary(sales_4long)

ggplot(sales_4long, aes(x = value, y = variable))+
  geom_boxplot()+
  labs(title = 'Distribution of Sales by Products', 
       x = 'Sales in M £',
       y = 'Sales by geographical Area')

# North America is our biggest market. 
# Next the distribution of sales by product ID will be investigated 

ggplot(grouped_sales, aes(x = Sum_NA_Sales))+
  geom_histogram()+
  labs(title = 'Sales figures in North America in M £',
       x = 'Total Sales',
       y = 'Frequency')

ggplot(grouped_sales, aes(x = Sum_EU_Sales))+
  geom_histogram()+
  labs(title = 'Sales figures in Europe in M £',
       x = 'Total Sales',
       y = 'Frequency')

ggplot(grouped_sales, aes(x = Sum_Other_Sales))+
  geom_histogram()+
  labs(title = 'Sales figures Other in M £',
       x = 'Total Sales',
       y = 'Frequency')

# Visually there seems to be no normal distribution of the sales data. 
# There is a skew towards the left and there are a number of outliers.  

# Create a qq plot for the sales data
qqnorm(sales_2$Global_Sales, pch = 1, frame = FALSE)
# Add a line through the plot
qqline(sales_2$Global_Sales, col= 'red', lwd = 2)
# Global Sales seems not to be normally distributed,
# The datapoints tend to be above the line 

# Create a qq plot for the NA and EU Sales
qqnorm(sales_2$NA_Sales, pch = 1, frame = FALSE)
qqline(sales_2$NA_Sales, col= 'red', lwd = 2)

qqnorm(sales_2$EU_Sales, pch = 1, frame = FALSE)
qqline(sales_2$EU_Sales, col= 'red', lwd = 2)

qqnorm(sales_2$Other_Sales, pch = 1, frame = FALSE)
qqline(sales_2$Other_Sales, col= 'red', lwd = 2)

# The same format is visible in the EU and NA data. The datapoints are a bit 
# better aligned for the EU data but the extremes are far away from the line. 

# I will continue with the test for normality by running a shapiro-wilk test
# for all the sales data. 
# H0: the data is not normally distributed 
# HA: the data is normally distributed
shapiro.test(sales_2$Global_Sales)
shapiro.test(sales_2$NA_Sales)
shapiro.test(sales_2$EU_Sales)
shapiro.test(sales_2$Other_Sales)

# The values are very small and therefore we can not reject the H0 that the data
# is not normally distributed. The data does not seem to be normally distributed.

# Perform the test for kurtosis and skewness
library(moments)

kurtosis(sales_2$Global_Sales)
skewness(sales_2$Global_Sales)

# The kurtosis is 32.7, this means our data is very concentrated
# The skewness is also high at 4.04, this indicates we have big tails

# The global sales column has failed the test for normality, this means
# it will be unwise to run statistical tests on that column.

# Now the same tests will be repeated for the remaining sales data
kurtosis(sales_2$NA_Sales)
skewness(sales_2$NA_Sales)

kurtosis(sales_2$EU_Sales)
skewness(sales_2$EU_Sales)

kurtosis(sales_2$Other_Sales)
skewness(sales_2$Other_Sales)

# The results are similar, without going into all the details, all sales
# data have failed the tests for normality. Performing any inferential statistics
# would not make sense. 

################################################################################

# While we can not perform tests we will evaluate the dataset at hand. 

# A scatterplot will be created to show whether there is a relationship
# between Sales in North America and in Europe
ggplot(grouped_sales, aes(x = Sum_NA_Sales, y = Sum_EU_Sales))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_light()+
  labs(title= 'Relationship between North American and European Sales',
       x= 'Total Sales in North America in M £',
       y = 'Total Sales in Europe in M £')

# There is a positive relationship between the two figures. 

# Next I will assess whether the categorical data has important insights
# To do so I will refer to the original dataset. 
# The first analysis is the relationship between Genre and global sales
ggplot(sales, aes(x = Genre, y = Global_Sales))+
  geom_col()+
  labs(title = 'Global Sales by Genre',
         x = 'Genre',
         y = 'Global Sales in M £')

# Now I will analyze the Global sales based on the Publisher
sales_by_publisher <- sales %>% group_by(Publisher) %>% 
  summarise(Sum_Global_Sales = sum(Global_Sales))
            
head(sales_by_publisher)

ggplot(sales_by_publisher, aes(x = Publisher, y = Sum_Global_Sales))+
  geom_col()+
  coord_flip()+
  theme_light()+
  labs(title = 'Global Sales by Publisher',
       x = 'Global Sales in M£',
       y = 'Publisher')

# Check the relationship between year of release and global sales
ggplot(sales, aes(x = Year, y = Global_Sales))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm, se = F)+
  theme_light()+
  labs(title = 'Global Sales by Year when Game was published',
       x = 'Year Published',
       y = 'Global Sales in M£')

# Lastly we will compare all the sales columns for correlations
cor(sales_4)
# Not surprising, that the sales columns are highly correlated, increase in 
# sales in one region does not negatively impact sales in other regions.

################################################################################

# Making recommendations to the business

# Investigate possible relationship by creating simple and multiple linreg models
# Based on the models we will answer the following questions:

# Do we have confidence in the models based on goodness of fit and accuracy?
# What do we suggest and recommend to the business
# How could we improve the model?

# Create a linear regression model between the three columns
model1 <- lm(grouped_sales$Sum_NA_Sales ~ grouped_sales$Sum_Global_Sales)
model2 <- lm(grouped_sales$Sum_NA_Sales ~ grouped_sales$Sum_EU_Sales)
model3 <- lm(grouped_sales$Sum_EU_Sales ~ grouped_sales$Sum_Global_Sales)

# Create a summary for all the models 
summary(model1)
summary(model2)
summary(model3)
# All models have very high significance values.
# Model 1 NA-Global has a very high correlation and a high R-squared of 0.84
# Model 2 NA-EU has the weakest R-squared of 0.38, the NA sales are not a great,
# indicator of the sales in EU
# Model 3 EU-Global has a R squared of 0.72 which is also very high.

# Create visualization of the models 
plot(model1$residuals)
abline(coefficients(model1))
SSE1 <- sum(model1$residuals^2)
SSE1

plot(model2$residuals)
abline(coefficients((model2)))
SSE2 <- sum(model2$residuals^2)
SSE2

plot(model3$residuals)
abline(coefficients((model3)))
SSE3 <- sum(model3$residuals^2)
SSE3

# Model 3 EU-Global has the lowest Sum of squared residuals, meaning the model 
# has the least variation between observed data and the modeled values. 

# Next a multiple linear regression model is tested on the three columns
model_mlr <- lm(Sum_Global_Sales ~ Sum_NA_Sales + Sum_EU_Sales, data = grouped_sales)
summary(model_mlr)

# While expected there is a very high indication to predict global sales with the 
# two largest regions that make up for the majority of the Global Sales. 
# An Adjusted R Squared of 0.96 proofs of almost a perfect model. 

# Now some random values will be picked and the predicted global sales 
# will be compared to the observed values
tail(grouped_sales, 10)
head(grouped_sales, 10)
# Now check how well the model performs based on new observations
observations <- data.frame(Sum_NA_Sales = c(34.02,14.4,3.35,3.89,26.6), 
                           Sum_EU_Sales = c(23.8, 7.79,1.97,3.21,4.01))

# Use the model to predict the new values
predict(model_mlr, newdata = observations)

# The values are very close to the observed values 
# observed 1 = 67.8, predicted = 68.05
# observed 2 = 25.4, predicted = 26.66
# observed 3 = 7.13, predicted = 7.19
# observed 4 = 10.0, predicted = 9.29
# observed 5 = 37.2, predicted = 35.92


