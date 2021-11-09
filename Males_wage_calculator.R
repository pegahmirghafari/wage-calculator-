####################
####################
##### PART - 0 #####
# IMPORT TIDYVERSE, DATA & REVIEW

# Import the TidyVerse package
library(tidyverse)

males <- read.csv("males.csv", stringsAsFactors = TRUE, na.strings = "NA")

# ALTERNATIVE You can load the CSV directly from a URL if needed...
# males <- read.csv("https://research.library.gsu.edu/ld.php?content_id=57157521", stringsAsFactors = TRUE, na.strings = "NA")

# Inspect the data
view(males)

# Confirm that the "males" dataset is open and assigned to its own object in R/RStudio
summary(males)
# Structure and datatypes in dataset
str(males)

# Inspect the data
head(males)


males$edu_exp <- males$school + males$exper
males$age <- males$exper + males$school + 6


view(males)



####################
####################
##### PART - 1 #####
# BASICS OF LINEAR MODELLING
# Using the males dataset, we can create a simple linear model where "wage" is the dependent 
# variable ("y") and "school" is the independent variable ("x").


linear_model <- lm ( formula = wage ~ school, data = males)

summary(linear_model)

anova(linear_model)

linear_model$residuals

linear_model$fitted.values



new_model <- lm(
   formula = wage ~ exper,
   data = males
 )

# Use the summary(...) function to display the summary of the new_model
summary(new_model)


# Use the "$" following new_model to display just the coefficients of the model

new_model$coefficients


####################
####################
##### PART - 2 #####
# FORMULATING MODELS
# "multiple linear regression" models with fixed effects and interaction terms. 

linear_model <- lm(
  formula = wage ~ school + exper,
  data = males
  )

linear_model

linear_model <- lm(
  formula = wage ~ . ,
  data = males
)

summary(linear_model)



linear_model <- lm(
  formula = log(wage) ~ 1 + . -(health+married) ,
  data = males
)

summary(linear_model)


# Categorical variables
linear_model <- lm(
  formula = wage ~ union ,
  data = males
)

summary(linear_model)


# Dependent variable:  log(wage)
# Independent variables: school, exper, school multiplied by exper, married, and ethn

new_model <- lm(
   formula = log(wage) ~ ((school+ exper+ school)* (exper + married +ethn)) ,
   data = males
)

summary(new_model)


####################
####################
##### PART - 3 #####
# PREDICTIONS, DIAGNOSTICS, AND SAVING AND LOADING MODELS

# First, create a linear_model
linear_model <- lm(
  formula = wage ~ 1 + school + exper + union ,
  data = males
)

# Inspect the model to make sure it represents the expected outcomes.
summary(linear_model)


# PREDICTIONS
# First, create a new_data dataset.  Using tidyverse (dplyr) functions and pipes, we will 
# create a simple subset of 10 random observations for testing purposes.
sample_data <- males %>% drop_na() %>% sample_n(10)
view(sample_data)


# Using the predict(...) function, we can feed the function our model (linear_model) and 
# new samples (sample_data) to process through the model to receive predicted values.
predict(linear_model, newdata = sample_data)


# DIAGNOSTICS
influence.measures(linear_model)


dfbetas(linear_model)
cooks.distance(linear_model)
dffits(linear_model)


# DIAGNOSTIC PLOTS


par(mfrow=(c(2,2)))
plot(linear_model)

####################
####################
##### PART - 4 #####
# GGPLOT2 - GRAPHS AND PLOTS.  START SMALL, BUILD UP!

# Lastly, adding the geom_function results in a visualized plot
wage_plot <- ggplot(data = males, mapping = aes(x=wage)) + 
  geom_histogram(color = "black" , fill = "green" )+
  labs(x = "Hourly Wages",
       y = "Frequency Count",
       title = "Histogram of Hourly Wages")

wage_plot



school_plot <- 
   ggplot(data = males, mapping = aes( x = school ) ) + 
   geom_histogram( color = "black" , fill = "blue"   )+ 
  labs(x = "Years of School",
       y = "Frequency Count",
       title = "Histogram of School")
 school_plot


# Create a histogram plot using the males dataset and the variable exper

exper_plot <- 
  ggplot(data = males, mapping = aes( x = exper ) ) + 
  geom_histogram( color = "black" , fill = "orange"  )+
  labs(x = "Years of Experiance",
       y = "Frequency Count",
       title = "Histogram of Experiance")
  

exper_plot



####################
####################
##### PART - 6 #####
# GGPLOT2 - SCATTERPLOTS!!!


linear_model <- lm(
  formula = wage ~ 1 + school + exper + union + married + ethn ,
  data = males
)

model_data <-
  linear_model$model %>%
  mutate(index = seq(1,n()), .before = wage) %>%
  bind_cols(yhat = linear_model$fitted.values) %>%
  bind_cols(res = linear_model$residuals) %>%
  bind_cols(res_standard = rstandard(linear_model)) %>%
  bind_cols(res_student = rstudent(linear_model)) %>%
  tibble()

# Inspect the data
view(model_data)


scatter_plot <- 
  ggplot(data = model_data, mapping = aes(x=wage, y=yhat)) +
  geom_point(mapping = aes(color = union))
scatter_plot


# Plotting x = yhat (i.e. fitted/predicted value of wage) and y = "standardized residual" in order to evaluate and
# inspect the distribution of residuals in the model.
scatter_plot <- 
  ggplot(data = model_data, mapping = aes(x=yhat, y=res_standard)) +
  geom_point( mapping = aes(color = union , shape = married, size = exper))+
  scale_color_brewer(palette="Set1") +
  scale_size( range = c(1,3) )+
  labs( x = "Fitted Values") +
  labs( y = "Standardized Residuals") +
  labs( title = "Standardized Residuals Plot")
scatter_plot



plot_object <- 
   ggplot(data = model_data, mapping = aes(x=index, y=res_student)) +
   geom_point( maping = aes(color = exper ) ) +
  labs( x = "indecies") +
  labs( y = "Standardized Residuals") +
  labs( title = "Standardized Residuals Plot")
plot_object



####################
####################
##### PART - 6 #####

# DENSITY DISTRIBUTION PLOT
exp_density <-
  ggplot(data = model_data, mapping = aes(exper)) +
  geom_density( color = "purple", fill = "black", size=4, ) +
  labs( x = "Years of Experience" ) +
  labs( y = "Density") +
  labs( title = "Density Distrubtion of Exper")
exp_density

# DENSITY ALTERNATIVE
exp_density <-
  ggplot(data = model_data, mapping = aes(exper)) +
  geom_line( stat="density", color="red", size=2, alpha=1.0 ) +
  geom_area( stat="density", fill="blue", size=0, alpha=0.25) + 
  labs( y = "Density") +
  labs( title = "Density Distrubtion of Exper")
exp_density


# BOXPLOT
boxplot <-
  ggplot(data = model_data, mapping = aes(x=ethn,y=wage,fill=ethn)) + 
  geom_boxplot(  ) +
  scale_fill_brewer( palette = "Spectral", na.value = "white") +
  labs( x = "Ethnicity") +
  labs( y = "Hourly Wages" ) +
  labs( title = "Boxplot of Hourly Wages across Ethnicity" )
boxplot



