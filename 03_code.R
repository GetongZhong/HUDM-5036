###########
# Lists
###########
vec1 <- c(1, "a", FALSE)
vec1
class(vec1)

lst1 <- list(1, "a", FALSE)
lst1
class(lst1)

lst1[[1]]
class(lst1[[1]])
lst1[[2]]
class(lst1[[2]])
lst1[[3]]
class(lst1[[3]])

lst1 <- list(number = 1, letter = "a", third_thing = FALSE)
lst1

#################
# Data frames
#################
state.x77
class(state.x77)
typeof(state.x77)

df <- data.frame(state.x77)
class(df)
str(df)
df
# Add variables
df <- data.frame(df, abb = state.abb, regn = state.region)
str(df)
state.region
# Load packages from the tidyverse
library(tidyverse)

# mutate() to add two variables that use percentiles to
# create binned versions of other variables.
df <- mutate(df, 
             Murder_C = ntile(Murder, 2),
             Life.Exp_C = ntile(Life.Exp, 4))
str(df)

# Make the new binned variables factors and rename the
# levels
df$Murder_C <- factor(df$Murder_C,
                      levels = c(1, 2), 
                      labels = c("Low", "High"))
df$Life.Exp_C <- factor(df$Life.Exp_C,
                        levels = c(1, 2, 3, 4), 
                        labels = c("Lowest", "Low", "High", "Highest"))
str(df)
table(df$Murder_C)
table(df$Life.Exp_C)

#############################
# Plotting bivariate data
#############################
# Scatterplots for two numeric variables
plot(x = df$Life.Exp, 
     y = df$Murder,
     xlab = "Life Expectancy",
     ylab = "Murder Rate",
     main = "The 50 United States")

ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_point() + 
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

plot(x = df$Life.Exp, 
     y = df$Murder,
     pch = 24,
     cex = 1.5, # character expansion
     bg = 2, # background color
     col = 4, # point line color
     xlab = "Life Expectancy",
     ylab = "Murder Rate",
     main = "The 50 United States")

ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_point(shape = 24, fill = 2, color = 4, size = 3.5) + 
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

# Lots of options for point characters, line types, colors, etc.
?points
?par # see the lty argument for line types
# See this link (below) for a list of recognized color names.
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

plot(x = df$Life.Exp, 
     y = df$Murder,
     pch = 24,
     cex = 1.5, # character expansion
     bg = 2, # background color
     col = 4, # point line color
     xlab = "Life Expectancy",
     ylab = "Murder Rate",
     main = "The 50 United States")

# Run linear regression and plot line of best fit
lm1 <- lm(formula = Murder ~ Life.Exp, data = df)
abline(lm1, col = "steelblue", lty = 2, lwd = 3)

ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
              color = "steelblue", linetype = "dashed") +
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

plot(x = df$Life.Exp, 
     y = df$Murder,
     xlab = "Life Expectancy",
     ylab = "Murder Rate",
     main = "The 50 United States",
     type = "n")
text(x = df$Life.Exp, 
     y = df$Murder,
     labels = df$abb)

ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_text(mapping = aes(label = abb)) + 
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

as.numeric(df$regn)
cols <- factor(x = df$regn, 
               levels = c("Northeast", "South", "North Central", "West"),
               labels = c("blue", "red", "purple", "green"))
as.character(cols)
df
plot(x = df$Life.Exp, 
     y = df$Murder,
     xlab = "Life Expectancy",
     ylab = "Murder Rate",
     main = "The 50 United States",
     type = "n")
text(x = df$Life.Exp, 
     y = df$Murder,
     labels = df$abb,
     col = as.character(cols))
legend(x = "topright",
       legend = c("Northeast", "South", "North Central", "West"),
       col = c("blue", "red", "purple", "green"),
       pch = 15, pt.cex = 1.5)
df
ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_text(mapping = aes(label = abb, color = regn)) + 
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

ggplot(data = df, mapping = aes(x = Life.Exp, y = Murder)) + 
  geom_point(mapping = aes(color = regn), size = -2) + 
  geom_text(mapping = aes(label = abb, color = regn), show.legend = FALSE) + 
  xlab("Life Expectancy") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States") +
  guides(color = guide_legend(override.aes = list(size = 5, pch = 15)))
  
boxplot(df$Murder ~ df$regn,
        ylab = "Murder Rate",
        xlab = "Region",
        main = "The 50 United States")

ggplot(data = df, mapping = aes(x = regn, y = Murder)) + 
  geom_boxplot() +
  xlab("Region") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")

ggplot(data = df, mapping = aes(x = regn, y = Murder)) + 
  geom_violin() +
  geom_boxplot(width = .1) + 
  xlab("Region") + 
  ylab("Murder Rate") + 
  ggtitle("The 50 United States")
    
tab1 <- table(df$regn)
barplot(tab1)

ggplot(data = df, mapping = aes(x = regn)) + 
  geom_bar()

tab2 <- table(df$regn, df$Murder_C)
tab2

barplot(tab2, beside = TRUE, col = 2:5,
        main = "Grouped Barplot") 
legend(x = 3, y = 13, 
       legend = c("Northeast", "South", "North Central", "West"),
       col = 2:5, pch = 15)

ggplot(data = df, mapping = aes(x = Murder_C, fill = regn)) + 
  geom_bar(position = "dodge") + 
  xlab("Region") + 
  ylab("Murder Rate") + 
  ggtitle("Grouped Barplot") + 
  guides(fill = guide_legend(title = "Region"))

barplot(tab2, beside = FALSE, col = 2:5,
        main = "Grouped Barplot",
        xlab = "Murder Rate") 
legend(x = 1.65, y = 11, 
       legend = c("Northeast", "South", "North Central", "West"),
       col = 2:5, pch = 15)

ggplot(data = df, mapping = aes(x = Murder_C, fill = regn)) + 
  geom_bar() + # Note I removed position = "dodge"
  xlab("Region") + 
  ylab("Murder Rate") + 
  ggtitle("Grouped Barplot") + 
  guides(fill = guide_legend(title = "Region"))
df

