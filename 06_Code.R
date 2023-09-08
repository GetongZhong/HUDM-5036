#######
# Multivariate Plotting
#######

library(car)
par(mfrow = c(2,2))

plot(x = Salaries$yrs.since.phd,
     y = Salaries$salary,
     xlab = "Years since PhD",
     ylab = "Annual salary ($)")

hist(x = Salaries$salary,
     main = "", 
     freq = FALSE,
     xlab = "Annual salary ($)")
d5 <- density(Salaries$salary)
lines(d5, col = "red", lwd = 3)

hist(x = Salaries$yrs.since.phd,
     main = "", 
     freq = FALSE,
     xlab = "Years since PhD")
d6 <- density(Salaries$yrs.since.phd)
lines(d6, col = "red", lwd = 3)

plot.new()
text(x = .5, y = .5, 
     labels = paste0("This is an example of partitioning the\n",
                    "plotting space with par(mfrow = c(2,2))"))

layout(mat = matrix(data = c(1, 2), nrow = 2, ncol = 1, byrow = TRUE),
       widths = c(1), 
       heights = c(1, 2))
par(mar = c(0, 4.1, 4.1, 2.1))        # set bottom margin to 0
hist(x = Salaries$yrs.since.phd,
     main = "", 
     freq = FALSE,
     axes = FALSE,                    # suppress axes
     xlab = "", ylab = "")            # suppress labels
d6 <- density(Salaries$yrs.since.phd)
lines(d6, col = "red", lwd = 3)
par(mar = c(5.1, 4.1, 0, 2.1))        # restore bottom margin
plot(x = Salaries$yrs.since.phd,      # but set top to zero
     y = Salaries$salary,
     xlab = "Years since PhD",
     ylab = "Annual salary ($)",
     xlim = c(0, 60))
par(mar = c(5, 4, 4, 2) + .1)         # restore all margins
mtext("Here is the mtext", side = 3, outer = FALSE, line = 10)

library(tidyverse)
library(ggExtra)
p1 <- ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary)) +
  geom_point() +
  xlab("Years since PhD") + 
  ylab("Annual salary ($)") +
  theme(legend.position = "none")
p1
# with marginal histograms
p2 <- ggMarginal(p1, type = "histogram")
p2

p3 <- ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary)) +
  geom_point(mapping = aes(color = sex)) +
  xlab("Years since PhD") + 
  ylab("Annual salary ($)")
p3
p4 <- ggMarginal(p3, groupColour = TRUE, groupFill = TRUE)
p4

ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary)) +
  geom_point(mapping = aes(color = sex)) +
  xlab("Years since PhD") + 
  ylab("Annual salary ($)") +
  facet_wrap(~ rank)

#######
# More on Data Transformation
#######
# Define vector and square each element with a for loop
vec1 <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
vec1
for (i in 1:10) {vec1[i] <- vec1[i]^2}
vec1

# Define vector and square each element with vectorization
vec1 <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
vec1
vec1 <- vec1^2
vec1

# Examples of relational operator ==
4 == 4
4 == 5
"a" == "b"
"4ever" == "4ever"
4 == c(1, 4, 2, 4) # vectorization
c("a", "lazy", "brown", "fox") == "brown" # vectorization
c(1, 2, 3, 1, 2, 3, 1, 2, 3) == c(1, 1, 1, 2, 2, 2, 3, 3, 3)
c(3, 2) == c(3, 2, 4, 3, 2, 3, 3, 2) # with recycling
c(3, 2, 3, 2, 3, 2, 3, 2) == c(3, 2, 4, 3, 2, 3, 3, 2) # identical to above
c(1, 2, 3) == c(1, 2, 3, 4, 2, 3, 1, 2)
mat1 <- matrix(c(1, 2, 3, 4), 2, 2)
mat1
mat1 == 2

# Logical and and or
log1 <- c(TRUE, TRUE, FALSE, FALSE)
log2 <- c(TRUE, FALSE, TRUE, FALSE)
log1 & log2
log1 | log2

# Combine == with |
vec2 <- c(1, 4, 5, 7, 9, 10, 14, 10, 11, 7)
vec2 == 7 | vec2 == 10

# Working with state.x77
library(tidyverse)
df <- as_tibble(state.x77)
df$Abb <- state.abb
df$Region <- state.region

# The which() function
log3 <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
which(log3)

# Subset to only South
df_south <- df[which(df$Region == "South"), ]
df_south

# Northeast or West
df_NW <- df[which(df$Region == "Northeast" | df$Region == "West"), ]
df_NW

# HS grad rate < 50
df_grad_less_50 <- df[which(df$`HS Grad` < 50), ]
df_grad_less_50

# HS grad rate < 45% and murder rate > 10 per 100,000
df_combo <- df[which(df$`HS Grad` < 45 & df$Murder > 10),]
df_combo

# Four way "and"
df_combo2 <- df[which(df$`HS Grad` > 60 & df$Murder < 5 & 
                        df$Illiteracy < 1 & df$`Life Exp` >= 71), ]
df_combo2

# Same as above but with with()
df_combo3 <- with(data = df,
                  df[which(`HS Grad` > 60 & Murder < 5 & 
                             Illiteracy < 1 & `Life Exp` >= 71), ])
df_combo3

# Replicate df_south and df_combo2 with subset()
df_south <- subset(df, Region == "South")
df_combo2 <- subset(df, `HS Grad` > 60 & Murder < 5 & 
                      Illiteracy < 1 & `Life Exp` >= 71)

# Subset first and third columns but keep all rows
df_13 <- df[, c(1, 3)]
df_13

# Subset Murder and Frost columns
df_MF <- df[, c("Murder", "Frost")]
df_MF

# Do the same as above using subset()
df_MF <- subset(df, select = c(Murder, Frost))
df_MF

# subset() recognizes the : operator
df_MF <- subset(df, select = Murder:Frost)
df_MF

# Value match operator %in%
target_states <- c("NY", "CA", "TX", "RI", "MA", "LA",
                   "ID", "AZ", "WI", "IL", "TN", "FL")
df_target <- subset(df, Abb %in% target_states)

# Replicate above subsetting of cases using filter() and pipe
df_south <- df %>% filter(Region == "South")
df_NW <- df %>% filter(Region == "Northeast" | df$Region == "West")
df_grad_less_50 <- df %>% filter(`HS Grad` < 50)
df_combo <- df %>% filter(`HS Grad` < 45 & Murder > 10)
df_combo2 <- df %>% filter(`HS Grad` > 60 & Murder < 5 & 
                             Illiteracy < 1 & `Life Exp` >= 71)

# Replicate column selections using select()
df_MF <- df %>% select(c(Murder, Frost))

# Sort df by life expectancy (ascending order)
df_sort1 <- df[order(df$`Life Exp`), ]
df_sort1

# Sort df by life expectancy (descending order)
df_sort2 <- df[order(df$`Life Exp`, decreasing = TRUE), ]
df_sort2

# Sort df by region followed by HS graduation rate
df_sort3 <- df[order(df$Region, -df$`HS Grad`), ]
print(df_sort3, n = 50)
df_sort4 <- df[order(df$Region, -df$`HS Grad`), ]
print(df_sort4, n = 50)

# Sort via arrange()
df_sort1 <- df %>% arrange(`Life Exp`)
df_sort2 <- df %>% arrange(desc(`Life Exp`))
df_sort3 <- df %>% arrange(Region, `HS Grad`)
df_sort4 <- df %>% arrange(Region, -`HS Grad`)
df_sort4 <- df %>% arrange(Region, desc(`HS Grad`))
