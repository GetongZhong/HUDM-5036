# Can import a .csv file by using read.csv() and specifying 
# the location of the file in quotes and using forward slashes.
states <- read.csv(file = "C:/Users/tonyg/Desktop/Academic/Grad/HUDM 5026/states.csv")

# In general, you should try to keep you code on each line shorter than
# 80 characters so that you don't need to scroll left and right to read 
# it. If you directory path name is short, you can use the code above 
# directly. If not, use paste() to store a long file name and have the
# code broken across lines so it fits in under 80 chars/line.

# The paste() function concatenates vectors
paste(c("a", "b", "c"), c(1, 2, 3))
paste(c("a", "b", "c"), c(1, 2, 3), sep = "_")
paste(c("a", "b", "c"), c(1, 2, 3), sep = "")

# The collapse argument further concatenates the results
paste(c("a", "b", "c"), c(1, 2, 3), sep = "")
paste(c("a", "b", "c"), c(1, 2, 3), sep = "", collapse = "")

# When used on a single vector with a collapse argument
# the vector elements will be collapsed to a single character
# string.
paste(c("abc", "def", "ghi"))
paste(c("abc", "def", "ghi"), collapse = "")

# Set name of file location and call it path
path <- paste(c("C:/Users/tonyg/Desktop/",
                "Academic/Grad/HUDM 5026/", 
                "states.csv"),
              collapse = "")
path

# Import csv file and call it states
states <- read.csv(file = path)

# Another approach (not recommended) involves using file.choose(),
# which opens a browser window. 
states <- reas.csv(file = file.choose())

# Yet another approach is to use the "Files" tab of the plotting
# window pane in RStudio to navigate to the file on your computer.
# Then, if you click on the file name and choose "Import Dataset...", 
# a pop-up window will open up and you can choose to import. This is 
# a good option but will import the data as a tibble (mor on tibbles
# later). To get it to be a data frame that is no also a tibble, use
# as.data.frame after importing with this method.
# states <- as.data.frame(states)

# Once the data are imported and stored as a data frame, use the 
# str() function to get information on the structure of the data.
str(states)

# To subset a data frame, use square brackets with row and column
# indexes separated by a comma.
# First row, all columns
states[1, ]
# All rows, first column
states[, 1]
# 10th to 15th rows, 2nd to 4th columns
states[10:15, 2:4]
# 5th row, 6th column
states[5, 6]

# Vectors. What is in our workspace environment?
ls()

# Get the second column of states and call it pop.
pop <- states[,2]
pop
ls()

# length, class, and names of vector
length(pop)
class(pop)
names(pop)

# Let's assign the vector names based on the state abbreviations
names(states)
names(pop) <- states[, 10]
names(pop)

# Save the state abbreviation names as a separate vector
nms <- states[, 10]
length(nms)
class(nms)

# Use some functions on the pop vector. What happens if you use
# a numeric function on the nms vector? Error.
sum(pop)
mean(pop)
var(pop)
sd(pop)
sum(nms)

# Sort the values
sort(pop)

# 10th through 15th elements of nms
nms[c(10, 11, 12, 13, 14, 15)]

# Define logical, numeric, and character vectors
lgl1 <- c(TRUE, FALSE, FALSE)
num1 <- c(1, 2, 5, 10)
chr1 <- c("a", "boy", "dog", "cat", "banana", "apple", "boy", "girl")

# What is the class of each?
class(lgl1)
class(num1)
class(chr1)

# Vector coercion
c(lgl1, num1)
c(lgl1, chr1)
c(num1, chr1)

# Boxplot
boxplot(pop, 
        horizontal = TRUE, 
        xlab = "U.S. State Population in 1975 (in 1000s)")

# Boxplots highlight the quartiles
quantile(x = pop, probs = c(.25, .5, .75))

# Interquartile range
IQR(pop)

# Observations are labeled outliers if they fall outside of
# Q1 - 1.5*IQR and Q3 + 1.5*IQR
1079.5 - 1.5*3889.0
4968.5 + 1.5*3889.0

# Five outliers
sort(pop)[which(sort(pop) > 10802)]

# Boxplot in ggplot2
library(ggplot2)
ggplot(data = states, mapping = aes(x = pop)) + 
  geom_boxplot()

# Histogram 
hist(pop, breaks = seq(0, 25000, 5000), 
     xlab = "U.S. State Population in 1975 (in 1000s)",
     main = "Frequency histogram with 5 bins")
hist(pop, breaks = seq(0, 25000, 1000), 
     xlab = "U.S. State Population in 1975 (in 1000s)",
     main = "Frequency histogram with 25 bins")

# Histogram in ggplot2
gplot(data = pop_df, mapping = aes(x = pop)) + 
  geom_histogram(breaks = seq(0, 25000, 5000), 
                 col = "black", fill = "cadetblue3") + 
  xlab("U.S. State Population in 1975 (in 1000s)") + 
  ggtitle("Frequency histogram with 5 bins")

ggplot(data = pop_df, mapping = aes(x = pop)) + 
  geom_histogram(breaks = seq(0, 25000, 1000), 
                 col = "black", fill = "cadetblue3") + 
  xlab("U.S. State Population in 1975 (in 1000s)") + 
  ggtitle("Frequency histogram with 25 bins")

# Kernel density plot
plot(density(pop), 
     xlab = "U.S. State Population in 1975 (in 1000s)",
     main = "Kernel Density Plot", col = "steelblue", lwd = 2)

# Kernel density plot in ggplot2
ggplot(data = states, mapping = aes(x = pop)) + 
  geom_density(col = "steelblue", lwd = 1.05) + 
  xlab("U.S. State Population in 1975 (in 1000s)") + 
  ggtitle("Kernel Density Plot")

ggplot(data = states, mapping = aes(x = pop)) + 
  geom_density(col = "steelblue", lwd = 1.05) + 
  xlab("U.S. State Population in 1975 (in 1000s)") + 
  ggtitle("Kernel Density Plot") + 
  xlim(c(-500, 25000))

# QQ plot 
# Set random seed
set.seed(1234)

# Generate vector of 100 random draws from N(100, 15^2)
vec1 <- rnorm(n = 100, mean = 100, sd = 15)

# Create vector of expected quantiles for N = 100
quant1 <- qnorm(p = seq(0.005, .995, .01))

# Plot in bivariate scatter diagram
plot(quant1, sort(vec1),
     xlab = "Normal quantiles for 100 points", 
     ylab = "Random sample from N(100, 15^2; N = 100)",
     main = "Normal QQ plot")

# Add line through points
abline(a = 100, b = 15, lwd = 2)

# Normal QQ for population
qqnorm(y = pop,
       ylim = c(-10000, 25000),
       main = "Normal QQ plot for State Populations")
qqline(y = pop)

# QQ plot in ggplot2
ggplot(data = states, mapping = aes(sample = Population)) + 
  geom_qq(distribution = stats::qnorm) + 
  geom_qq_line(distribution = stats::qnorm) + 
  xlab("Theoretical normal quantiles") + 
  ylab("Sample quantiles")
