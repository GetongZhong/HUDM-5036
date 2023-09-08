# Working with state.x77 data; make it a data frame
state.x77
dat <- data.frame(state.x77)

# Examine the univariate distribution of the outcome variable
hist(dat$Life.Exp, 
     breaks = 15,
     xlab = "Life Expectancy (yrs)", 
     main = "State Life Expectancies")
# And predictor variables
hist(dat$Murder, 
     breaks = 15,
     xlab = "Number of Murders per 100,000", 
     main = "State Murder Rates")
hist(dat$Frost, 
     breaks = 15,
     xlab = "Avg. # Days Below Freezing", 
     main = "State Frost")
hist(dat$Illiteracy,
     breaks = 15,
     xlab = "Percent Illiterate (1970)", 
     main = "State Illiteracy Rates")

# Create pairwise scatterplots
plot(dat[,c(3, 4, 5, 7)])

# Print the correlation matrix, rounded to two dec places
(c1 <- round(cor(dat[,c(3,4,5,6)]), 2))

# Create an ordered correlogram
library(corrplot)
corrplot(corr = c1, 
         method = "circle", 
         order = "hclust")

# Fit a multiple regression model by regressing life expectancy
# on murder, illiteracy, and frost.
lm1 <- lm(Life.Exp ~ Murder + Illiteracy + Frost, data = dat)

# Load package car for diagnostics
library(car)

# Residual normality
# QQ plot
qqPlot(lm1)

# Histogram of residuals with normal curve overlay
hist(lm1$residuals, freq = FALSE)
curve(dnorm(x, mean = mean(lm1$residuals), sd = sd(lm1$residuals)),
      add = TRUE, lwd = 2, col = 3)

# Linearity
# Plot studentized residuals by fitted values
residualPlot(lm1, type = "rstudent")

# Or get studentized residuals from studres() from package MASS
# and plot 
library(MASS)
plot(x = lm1$fitted.values, 
     y = studres(lm1),
     xlab = "Fitted Values",
     ylab = "Studentized Residuals")

# Check component + residual plots
crPlot(lm1, variable = "Murder")
crPlot(lm1, variable = "Frost")
crPlot(lm1, variable = "Illiteracy")

# Variance inflation factors for multicollinearity
vif(lm1)

# These could also be calculated by getting the R^2 
# from regression one predictor on the rest.
(R_sq_murder <- summary(lm(Murder ~ Frost + Illiteracy, data = dat))$r.squared)
(vif_murder <- 1/(1 - R_sq_murder))

# Constant variance assumption
residualPlot(lm1, type = "rstudent")

?influencePlot
influencePlot(lm1)

# Finally, interpret the regression coefficients.
summary(lm1)

#####
### Categorical predictor
#####
# Run a new regression to see how diagnostics work with a
# categorical predictor. Add a categorical predictor for state region.
dat <- cbind(dat, state.region)
str(dat)
lm2 <- lm(Life.Exp ~ state.region, data = dat)
summary(lm2)

# The incremental F test to test the categorical factor can 
# be run by fitting an intercept-only model and comparing it
# to the model with the categorical factor using a likelihood
# ratio test via anova(). Note that because the categorical 
# state region factor is the only predictor here, this test
# result is identical to the omnibus test result in the summary
# output.
lm0 <- lm(Life.Exp ~ 1, data = dat)
anova(lm0, lm2)

# Default option for unordered factors is to use
# contr.treatment; this is dummy coding.
options()$contrasts

# How can you interpret the three regression slope estimates
# given by summary(lm2)?
# Normality
qqPlot(lm2)

# Linearity - When there is only a categorical predictor that has
# been dummy-coded, there is no need to check linearity because 
# it is trivially satisfied due to the dummy-coding.
residualPlot(lm2, type = "rstudent")

# Constant variance - With a categorical predictor or predictors 
# and no continuous predictors, the constant variance assumption may 
# be evaluated by examining the variances in each group, which should
# be equal. This can be done by calculating sample variances (effect
# sizes), by examining boxplots (visually), or by running Levene's 
# test (statistically). Doing all three of these is a good idea.
# As a rule of thumb, Maxwell, Delaney and Kelley (p. 139) suggest that
# (n_max/n_min) * (s^2_max/s^2_min) > 4 is problematic. That is,
# the product of the ratio of largest to smallest group size and
# the ratio of largest to smallest group sample variance should be
# less than about 4.

# Sample variance by group and group sample sizes
by(data = dat$Life.Exp, INDICES = dat$state.region, FUN = var)
table(dat$state.region)

# Maxwell et al rule of thumb balue of 5.81 suggests 
# heteroskedasticity may be problematic here.
(16/9)*(1.83/0.56)

# Run Levene's test, which is not significant (p = .47),
# so there is no evidence of heteroskedasticity based on 
# the test.
leveneTest(lm2)

# Can also look at boxplots and examine IQRs.
boxplot(Life.Exp ~ state.region, data = dat)

# In any case, if we are concerned we can do a robustness check
# using Welch's ANOVA procedure, which is robust to violation
# of homoskedasticity assumption.
welch1 <- oneway.test(Life.Exp ~ state.region, 
                      data = dat, 
                      var.equal = FALSE)
welch1

# The p-value for the test of the categorical factor via
# regression in lm2 was .00004; whereas the p-value based on
# Welch's test was .0001. Both are significant, so the result
# is robust to potential violation of homoskedasticity here.

#####
## Some further plotting
#####
# Refer to the notes from last class where we fit the 
# following models:
lm1 <- lm(C6R4MSCL ~ 1, data = eclsk)
lm2 <- lm(C6R4MSCL ~ MIRT + 1, data = eclsk)
lm3 <- lm(C6R4MSCL ~ MIRT - 1, data = eclsk)
lm4 <- lm(C6R4MSCL ~ F5SPECS + MIRT, data = eclsk)
lm5 <- lm(C6R4MSCL ~ F5SPECS + MIRT + I(MIRT^2), data = eclsk)
lm6 <- lm(C6R4MSCL ~ F5SPECS + MIRT + I(MIRT^2) + I(MIRT^3), data = eclsk)
lm7 <- lm(C6R4MSCL ~ F5SPECS + MIRT + I(MIRT^2) + I(MIRT^3) + I(MIRT^4), data = eclsk)
lm8 <- lm(C6R4MSCL ~ F5SPECS + MIRT + F5SPECS:MIRT, data = eclsk)
lm9 <- lm(C6R4MSCL ~ F5SPECS + MIRT + I(MIRT^2) + I(MIRT^3) + 
          F5SPECS:MIRT + F5SPECS:I(MIRT^2) + F5SPECS:I(MIRT^3), 
          data = eclsk)

X <- rnorm(1000, mean = 100, sd = 15)
Y <- 10 + 1*X + rnorm(1000, 0, sd = 4)
plot(X, Y)
lm11 <- lm(Y ~ X)
summary(lm11)
library(car)
qqPlot(lm11)
hist(lm11$residuals, breaks = 50)
