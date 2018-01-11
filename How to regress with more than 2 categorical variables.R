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





