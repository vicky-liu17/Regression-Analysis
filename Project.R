# Set repository
install.packages("car", repos = "https://cloud.r-project.org")
library(car)

# Read the bodyfatmen.csv data
data <- read.csv("bodyfatmen.csv")

# Setup

datamen <- read.csv(file = "bodyfatmen.csv")
head(datamen)
pairs(datamen)

df.model <- lm(data = datamen)
summary(df.model)
plot(df.model)

# Residual analysis

plot(density ~ hip, data = datamen)
abline(lm(density ~ hip, data = datamen))

stud_resids <- rstudent(df.model)
res <- residuals(df.model)
plot(res, stud_resids)
head(stud_resids, 14)

# Normality assumptions

qqnorm(stud_resids)
qqline(stud_resids)


# --------- Outliers --------

# DFBETAS:

df.hatmatrix <- hatvalues(df.model)
df.cook <- cooks.distance(df.model)
plot(df.cook)
# suggested cutoff
threshold <- 2/sqrt(nrow(datamen))
abline(h = threshold, col="blue")

# take away the two high leverege points, 
# one is clearly above and one is need for
# further investigation



# DFFITS:

df.fits = dffits(df.model)
plot(df.fits)
# suggested cutoff
threshold2 <- 2*sqrt(15/nrow(datamen))
abline(h = threshold2, col="blue")
abline(h = -threshold2, col="blue")



# Covariance and Cov-Ratio:

df.covariance <- cov(datamen)
df.covrat <- covratio(df.model)
plot(df.covrat)



# ------- Multicolinearity --------

y <- datamen$density
x <- data.matrix(datamen[,c("age","weight", "height","neck","chest","abdomen","hip","thigh","knee", "ankle","biceps","forearm","wrist" )])


# check VIF

df.vif <- vif(df.model)
plot(df.vif,  xlab="j:th regressor", ylab="VIF")
abline(h=5,col="blue")



# check eigenvalues:
X <- scale(x)/sqrt(nrow(x)-1)
XtX <- t(X)%*%X
eig <- eigen(XtX)$values

plot(max(eig)%/%eig, ylab="Condition Indices", ylim=c(0,1100))
abline(h=1000,col="red")
abline(h=100,col="blue")


coul <- colorRampPalette(brewer.pal(11, "PiYG"))(50)
levelplot(XtX, col.regions=coul,xlab="",ylab="",scales=list(x=list(rot=45)))



df01.model0 <- glmnet(X, y, alpha = 0) 
coefficients(df01.model0, s = 0.05)



# --------- MSE and R^2 using cross validation ----------

# First model
ridge_model <- glmnet(x, y, alpha = 0)
summary(ridge_model)


# optimal lambda
cv_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)


# find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)


# produce Ridge trace plot
plot(ridge_model, xvar = "lambda")
summary(best_model)$adj.r.squared


# use fitted best model to make predictions
y_predicted <- predict(ridge_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
mse <- sum((y_predicted - mean(y))^2)

mse2 <- mean((y - y_predicted)^2)


# find R-Squared
rsq <- 1 - sse/sst
rsq.adj <- 1-(((1-rsq)*(248-1))/(248-length(coef(best_model))-1))
rsq.adj

y_first <- predict(df.model, newx = x)

# find first SSE
sse_first <- sum((y_first - y)^2)
mse_first <- sum((y_first - mean(y))^2)

# find R-Squared
rsq_first <- 1 - sse_first/sst
rsq_first




# ------------ Forward Stepwise Selection ------------

#define intercept-only model
intercept_only <- lm(density ~ 1, data = datamen)

#define model with all predictors
all <- lm(density ~ ., data = datamen)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)

#view results of forward stepwise regression
forward$anova

#view final model
forward$coefficients




# ------------- Backward Stepwise Selection ------------

#define intercept-only model
intercept_only <- lm(density ~ 1, data=datamen)

#define model with all predictors
all <- lm(density ~ ., data=datamen)

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)

#view results of backward stepwise regression
backward$anova

#view final model
backward$coefficients



# ----------- Possible transformations ------------

bc <- boxcox(df.model, lambda = seq(-4,4,0.01))
summary(bc)

lambda <- bc$x[which.max(bc$y)]


BoxCox.lambda(x, method = c("guerrero", "loglik"), lower = -1, upper = 2)

library(AID)
out = boxcoxnc(datamen[,1],datamen[,2], method="mle", lambda = seq( -4 ,4 ,0.01), verbose = T)
