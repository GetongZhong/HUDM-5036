library(tidyverse)

# Convert state.x77 to a tibble and keep the rownames
df <- as_tibble(state.x77, rownames = NA)
df

# Add a column for state region
df$Region <- state.region
df

df[,9]

# Apply summary statistics to the life expectancy variable
# Notice that back ticks are needed because of the space in the name
life_exp <- df$`Life Exp`
life_exp

# Aside on conditional assignments
# Create a new variable that is a 1 if life expectancy is 70 or greater
# and 0 otherwise
df$LE <- ifelse(df$`Life Exp` > 70, 1, 0)
df

df <- df %>% mutate(LE2 = ifelfse(`Life Exp` > 70, 1, 0))
df

# Mean, variance, SD
(mean(life_exp))
(var(life_exp))
(sd(life_exp))

sum(life_exp)/length(life_exp)

# Five number summary (and mean)
summary(life_exp)

# Sample quantiles
quantile(x = life_exp, probs = .25) # Q1
quantile(x = life_exp, probs = .5) # Q2
quantile(x = life_exp, probs = .75) # Q3

median(life_exp)

# Visualize the sample quantiles
boxplot(life_exp, horizontal = TRUE)

# A more fine-grained view of the distribution
hist(life_exp, breaks = 20)

# As you know from last class, the apply() function can be 
# used to apply a function to all rows or cols of a matrix.
apply(X = df, MARGIN = 2, FUN = mean)
apply(X = df[,-9], MARGIN = 2, FUN = mean)
apply(X = df[,-9], MARGIN = 2, FUN = var)
apply(X = df[,-9], MARGIN = 2, FUN = sd)

# Suppress scientific notation
options(scipen = 999)
round(apply(X = df[,-9], MARGIN = 2, FUN = mean), 1)
round(apply(X = df[,-9], MARGIN = 2, FUN = var), 1)
round(apply(X = df[,-9], MARGIN = 2, FUN = sd), 1)
# Restore default option
options(scipen = 0)

# Can also get descriptives by groups with the by() function
df$Life_Exp <- df$`Life Exp`
by(data = df$`Life Exp`,
   INDICES = df$Region, 
   FUN = mean)

by(data = life_exp,
   INDICES = df$Region, 
   FUN = mean)

# Name the output to have it in a vector if you want to store it
# for later use or to operate on it
LE_reg_mns <- by(data = df$`Life Exp`,
                 INDICES = df$Region, 
                 FUN = mean)
LE_reg_mns
mean(LE_reg_mns)
table(df$Region)

# Bivariate statistics cov() and cor()
# Can be applied to a matrix or matrix-like object but only
# with numeric data
cor(df)

# The problem here is that region is not numeric, so remove it
# before taking correlation (or covariance)
cm <- round(cor(df[, -9]), 2)
covm <- round(cov(df[, -9]), 2)
cor(x = df$Murder, y = df$`Life Exp`)

diag(8)
upper.tri(cm)

covm[upper.tri(covm)] <- cm[upper.tri(cm)]
covm

length(life_exp)
life_exp[2] <- 100
life_exp

cm[lower.tri(cm)] <- NA
cm

# In tidyverse we can use the summarize() function to calculate 
# descriptive statistics
summarize(df, mean(`Life Exp`))
summarize(df, sd(`Life Exp`))
summarise(df, quantile(`Life Exp`, c(0.25, 0.50, 0.75)))

# Can use function group_by() to get descriptives by group
summarize(group_by(df, Region), mean(`Life Exp`))

# With piping
df %>% group_by(Region) %>% summarize(mean(`Life Exp`))
df %>% group_by(Region) %>% summarize(mean(`Murder`))

# To add frequency counts for grouping variable use n()
df %>% group_by(Region) %>% summarize(mean(`Murder`), n = n())


#####
# DATA TRANSORMATION
#####
# Load the herisson data. If you import the csv, use read_csv() so 
# that it comes in as a tibble.
dim(herisson)
herisson
names(herisson)
str(herisson)

# Do we need V1 as an ID? Are there any repeated measures?
table(herisson$V1)
which(table(herisson$V1) > 1)

# Drop V1 and Dischargedestination variables
herisson <- subset(x = herisson, select = -c(V1, Dischargedestination))
herisson <- select(herisson, -c(V1, Dischargedestination))

# Some variables that should be numeric are coded as character, and
# Groupaffiliation is numeric but should be a factor
str(herisson)
herisson$GroupF <- 
  factor(herisson$Groupaffiliation,
         levels = c(1, 2),
         labels = c("ES", "PS"))
str(herisson)

# Could also have done this via dplyr with recode_factor() and mutate()
herisson <- mutate(herisson, 
                   GroupF2 = recode_factor(Groupaffiliation, 
                                           '1' = "ES", '2' = "PS"))
str(herisson)

# Now let's work on converting the variables that should be numeric
table(herisson$Day7NIHSS, useNA = "always")
table(herisson$M3NIHSS, useNA = "always")
table(herisson$Day7Rankinscore, useNA = "always")
table(herisson$M3IndexdeBarthel, useNA = "always")

# First convert all "NK" values to NA (missing)
herisson <- na_if(herisson, "NK")

# Then, once the character values are gone, convert the
# variables to numeric. Could do this one at a time via
herisson$Day7NIHSS <- as.numeric(herisson$Day7NIHSS)
str(herisson)

# More efficient to do all at once with mutate_if()
herisson <- mutate_if(herisson, is.character, as.numeric)

# Create a variable called avg_NIHSS that is the average of
# 7 day and 3 month NIHSS scores.
herisson <- mutate(herisson, avg_NIHSS = 
                     rowMeans(select(herisson, Day7NIHSS, M3NIHSS), 
                              na.rm = TRUE))
str(herisson)

# Replace NaN values with NA
herisson$avg_NIHSS
which(is.nan(herisson$avg_NIHSS))
herisson$avg_NIHSS[which(is.nan(herisson$avg_NIHSS))] <- NA
str(herisson)

# Drop the duplicate grouping factor
herisson <- herisson %>% select(-c(GroupF2))
names(herisson)

# Summarize data
herisson %>% summarise()