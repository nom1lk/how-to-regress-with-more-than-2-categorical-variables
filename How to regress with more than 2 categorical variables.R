# In python: pd.get_dummies (from pandas module)

# Key points: just like the SEW seasonal consumption econometrics model, you have a dummy for 3 seasons and one season is the 'base'
# same for the CIKM turning two teams playing each other into numerically-encoded dummies

# Note that it seems glm() will do all the work for you (it must detect factors and go for it!)! (which is fantastic) - see evidence here:
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/ (see how the author converts anything categorical into factor, 
# and judging by the output (coefficients) it looks like glm keeps all but one factor for each categorical variable which it makes the base)

# but if you want to do it manually, here's my interpretation of how it should be done:
head(iris)

# Turn a categorical varible (in this case with 3 possibilities) into numerically-encoded binary dummy variables
dummies <- model.matrix( ~ Species - 1, data=iris )

#now just run regression on the dataset BUT
# - without the original categorical column AND 
# - including all but one of the numerically-encoded columns (the one you exclude will be the 'base' of the model)
# For example, if you exclude New South Wales, your model will predict for NSW

new_iris <- cbind(iris[,c(1:4)], dummies)

# We include all the original variables (except the categorical one), and all the numerically-encoded dummies (except one, in this case Speciesvirginica)
model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Speciessetosa + Speciesversicolor, data = new_iris)
summary(model)

# Interpretation and examples
model_coefficients <- summary(model)[[4]][,c(1)]

# If we want to predict the Sepal.Length for a Speciesvirginica (our excluded dummy) that has
# Sepal.Width = 3, Petal.Length = 4 and Petal.Width = 1.3, then we just use the base model:

model_coefficients[1] + 3 * model_coefficients[2] + 4 * model_coefficients[3] + 1.3 * model_coefficients[4]

# If we want to predcit the Sepal.Length for a flower which isn't the base of our model, we include the relevant dummy:
# Say we want to predict for a Speciesversicolor with Sepal.Width = 3, Petal.Length = 4 and Petal.Width = 1.3
model_coefficients[1] + 3 * model_coefficients[2] + 4 * model_coefficients[3] + 1.3 * model_coefficients[4] + 1 * model_coefficients[6]














iris1 <- cbind(iris[,1:4], Species = as.character(iris[,5]), stringsAsFactors = FALSE)

object_size_in_bytes <- 13899229872
iterations <- ceiling(object_size_in_bytes/(as.numeric(object.size(iris))))







iris %>% object.size


irislist = list()



for (i in 1:10) {
  irislist[[i]] <- iris1 
}

big_iris = do.call(rbind, irislist)
big_iris %>% object.size




for (i in 1:100) {
  irislist[[i]] <- iris1 
}

very_big_iris = do.call(rbind, irislist)
very_big_iris %>% object.size




for (i in 1:1000) {
  irislist[[i]] <- iris1 
}

very_very_big_iris = do.call(rbind, irislist)
very_very_big_iris %>% object.size




for (i in 1:10000) {
  irislist[[i]] <- iris1 
}

very_very_very_big_iris = do.call(rbind, irislist)
very_very_very_big_iris %>% object.size


# Try to pedict this non-linear growth
df <- data.frame(irises=c(1, 10, 100, 1000, 10000), size=c(7256, 61440, 601440, 6001440, 60001440), stringsAsFactors = FALSE)
df






# Refresher on linear regression

dataset <- cbind(iris[,1:4], Species = as.character(Species), stringsAsFactors = FALSE) 


dataset %>% sapply(class)

lm(Species~Sepal.Width+Petal.Length, data = dataset)


# Univariate regression (i.e. single independent variable)
response <- c(1,2,3)
somevar <- c(2,5,6)
lm(response ~ somevar)

# Same as above but providing a data.frame input works
res <- data.frame(res=c(1,2,3), som=c(2,5,6), stringsAsFactors = TRUE)
lm(res ~ som, data = res)



# Multivariate regression (i.e. multiple independent variables)
response <- c(1,2,3)
somevar <- c(2,5,6)
someothervar <- c(4, 1, -10)
lm(response ~ somevar + someothervar)


# Same as above but providing a data.frame input works
res <- data.frame(res=c(1,2,3), som=c(2,5,6), somoth=c(4, 1, -10), stringsAsFactors = TRUE)
lm(res ~ som + somoth, data = res)



lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)



# Without species 
lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)



# With Species 
Species_Dummies <- model.matrix(~ iris$Species - 1) %>% 
  as.data.frame %>% 
  select(-1) %>% # Remove first column and let whatever it was be the 'base' model
  `colnames<-`(str_split(names(.), "\\$") %>% sapply(function(x) { paste0(x[2], "_dummy") } )) # Give columns nice names


lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Speciesversicolor_dummy + Speciesvirginica_dummy, data = cbind(iris, Species_Dummies))
glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Speciesversicolor_dummy + Speciesvirginica_dummy, data = cbind(iris, Species_Dummies))


# Show that it roughly worked - the results don't look right
iris %>% 
  group_by(Species) %>% 
  summarise(ave_sep_len = mean(Sepal.Length),
            ave_sep_wid  = mean(Sepal.Width),
            ave_pet_len  = mean(Petal.Length),
            ave_pet_wid  = mean(Petal.Width))
# # A tibble: 3 x 5
# Species    ave_sep_len ave_sep_wid ave_pet_len ave_pet_wid
# 1 setosa            5.01        3.43        1.46       0.246
# 2 versicolor        5.94        2.77        4.26       1.33 
# 3 virginica         6.59        2.97        5.55       2.03 

# https://stats.stackexchange.com/questions/461123/methods-or-processes-for-discovering-the-major-reasons-for-linear-regression-coe

fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Speciesversicolor_dummy + Speciesvirginica_dummy, data = cbind(iris, Species_Dummies))
iris1 <- cbind(iris, predict(fit))
iris1$Sepal.Length - iris1$`predict(fit)`


# Okay worked it out. 
# So you can either
# -- OLS with all the dummies, omitting the intercept OR
# -- OLS on an intercept and the dummies, omitting one dummy 
# You cannot do both! Source: https://www.ssc.wisc.edu/~bhansen/390/390Lecture14.pdf#page=3

# Interpretation:
# alpha is the value of the omitted dummy
# The coefficients of the included dummies are: impact_of_any_included_dummy = its_coefficient + alpha

# So impact of bieng versicolor is 2.1713 + (-0.7236) = 1.4477





# Works, sensible result
data.frame(people=c(5, 50, 10), temp=c(12, 15, 14), rain=c(1, 0, 1), stringsAsFactors = F) %>% 
  lm(people ~ temp + rain, .)








