#######
# Code for Part I video
#######

library(tidyverse)

# 12 Tidy Data
table1  # this one is tidy
table2  # multiple observations in the same row (not tidy)
table3  # multiple values per cell (not tidy)
table4a # data here are split into two tables:
table4b # cases (4a) and population (4b)

# Three rules for tidy data.
# 1. Variables in columns.
# 2. Each observation has its own row.
# 3. Each value has its own cell.

# Exercise 12.2.2
(table1_R <- table1 %>% mutate(per_cap_cases = (cases/population) * 10000))
# Compute rate for table 2
# First get frequency of TB cases per year
(n_cases <- filter(table2, type == "cases"))
# Next get the population size per year
(n_pop <- filter(table2, type == "population"))
# Divide and multiply by 10000
# Put the data into a tidy form for the calculation.
per_cap <- tibble(
  year = n_cases$year,
  country = n_cases$country,
  cases = n_cases$count,
  population = n_pop$count
) %>%
  mutate(per_cap_cases = (cases/population) * 10000)
per_cap
# Now add the per capita information it back to table2.
# To do so we need to add additional rows with bind_rows().
# Need to have a 'type' variable to match the table 2 var names.
table2_R <- per_cap %>% 
  transmute(country, year, type = "per_cap", count = per_cap_cases) %>%
  bind_rows(table2) %>%
  arrange(country, year, type)

# Do the same with tabes 4a and 4b
table4a_R <- table4a %>% mutate(per_cap_1999 = (`1999`/table4b$`1999`) * 10000,
                                per_cap_2000 = (`2000`/table4b$`2000`) * 10000)



# 12.3 Pivoting
# Data like those in table4a are said to be in "wide" format; 
# whereas tidy data are said to be in "long" format.
# That is, data in wide format have multiple values in a 
# single row. That is, there are two values for each country in
# a single row: one for 1999 and one for 2000.
table4a
table4b

# To tidy this up, we need to "pivot" the data.
# To go from wide to long, use pivot_longer()
table4a
table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
# Note the backticks!

# Same idea with table4b
table4b
table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")


# To combine the two tables to get back to table1 (i.e., to tidy), 
# use left_join(). We will hear more about left_join() in ch 13.
# Name the tables in long format and then left_join them.
tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
left_join(tidy4a, tidy4b)

# It is also possible that we may want to go in the opposite direaction 
# (i.e., from long to wide). For that we use pivot_wider().
# Let's "widen" table2 so that the cases and population variables have
# their own columns. This will make table2 tidy like table1.
table2 
table2 %>%
  pivot_wider(names_from = type, values_from = count)
table1

# 12.4 Separating and Uniting
# separate() is useful for pulling apart a column based
# on a character you wish to use to mark the separation.
# As an example, recall table3, which has two variables in col.
table3
# Let's separate.
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
# Now it's tidy like table1 except that the vaiables are still 
# characters. Let's convert to numeric.
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)

# unite() is the reverse of separate() in that it combines
# multiple columns into a single column.
# table5 has century and year separated
table5
table5 %>% 
  unite(new, century, year)

# Note that by default the separation character is "_".
table5 %>% 
  unite(new, century, year, sep = "")

cnms <- colnames(state.x77)
paste0(cnms[4], " ~ ", paste0("`", cnms[c(1:3, 5:8)], collapse = "` + ", "`"))

# 12.5 Missing Values
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks
# From WG:
# "One way to think about the difference is with this Zen-like koan: 
# An explicit missing value is the presence of an absence; 
# an implicit missing value is the absence of a presence."

# May want to  make implicit missing explicit.
stocks %>% 
  pivot_wider(names_from = year, values_from = return)

# May also want to make explicit missing implicit.
# Use pivot_longer() with values_drop_na = TRUE
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

# Complete takes all combinations and makes missingness
# totally explicit.
stocks %>% 
  complete(year, qtr)

# 13.3 Keys
library(nycflights13)
data(package = "nycflights13")
airlines
airports
flights
planes
weather

# See the graphic in WG that describes the data set interrelations
# Primary keys are id variables that refer to cases in the same data set.
planes$tailnum
# Foreign keys are id variables that refer to cases in other data sets.
flights$tailnum

# Any duplicates of primary keys?
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

weather %>% filter(year == 2013 & month == 11 & day == 3 & hour == 2 & origin == "EWR")
# Any missing on hour?
weather %>% filter(year == 2013 & month == 3 & day == 10)

dim(weather)
weather$hour
length(weather$hour)
which(diff(weather$hour) == 2)

# Flights doesn't have a unique key. Neither flight number nor
# tail number are unique on a given day.
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

# Instead, can add a row number as a proxy for a unique key
print(flights %>% mutate(id = row_number()), width = Inf)

# 13.4 Mutating joins
# Create a subset of the data
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

# Want to add the full airline name to flights2.
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

# Visualizing joins (see WG for graphics)
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
x
y

# Inner join
x %>% 
  inner_join(y, by = "key")

# Outer joins can be left, right, or full.
x %>% left_join(y, by = "key") # keeps all key values in x
x %>% right_join(y, by = "key") # keeps all key values in y
x %>% full_join(y, by = "key") # keeps all key values in both

# What about duplicate keys?
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
x
y
left_join(x, y, by = "key")

# It's important to address duplicate key values before joining
# because otherwise you will get repeats.
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
x
y
left_join(x, y, by = "key")

# Defining columns
# The default if no by argument is specified is to join by 
# all variables that are in common across the two data sets.
names(flights2)
names(weather)
names(flights2)[names(flights2) %in% names(weather)]
flights2 %>% 
  left_join(weather)

# Can specify the variable(s) to join by explicitly.
# Note that now no other variables are used, only tailnum.
# Also note that variables with identical names will be 
# given a suffix based on which data set they came from.
# What does year mean in the flights data? What does year mean
# in the planes data?
flights2 %>% 
  left_join(planes, by = "tailnum")

# Can specify to join by a named character vector. From WG:
# This will match variable a in table x to variable b in table y. 
# The variables from x will be used in the output.
flights2 %>% 
  left_join(airports, c("dest" = "faa"))
flights2 %>% 
  left_join(airports, c("origin" = "faa"))


# Create a categorical variable from dummies
df1 <- data.frame(c1 = c("a", "b", "a", "b", "c", "c", "a", "b", "c"))
options()$contrasts
options(contrasts = c("contr.sum", "contr.poly"))
options(contrasts = c("contr.treatment", "contr.poly"))
mm1 <- model.matrix(object = ~ c1, data = df1)
dummyb <- mm1[,2]
dummyc <- mm1[,3]

# 1. Nested ifelse() loop:
(cat1 <- ifelse(dummyb == 1, "b", ifelse(dummyc == 1, "c", "a")))

# 2. pivot_longer:
(df2 <- tibble(id = 1:9, b = dummyb, c = dummyc)) # put dummies in a tibble 
(df2 <- df2 %>% mutate(a = 1*(b == 0 & c == 0))) # add dummya via mutate
(df2 <- df2 %>% pivot_longer(-id, values_to = "values", names_to = c("categ")))
(df2 <- df2 %>% filter(values == 1))
(df2 <- df2 %>% select(categ))

# Same approach but all in one expression:
(df2 <- tibble(id = 1:9, b = dummyb, c = dummyc)) # put dummies in a tibble 
(cat2 <- df2 %>% mutate(a = 1*(b == 0 & c == 0)) %>%
   pivot_longer(-id, values_to = "values", names_to = c("categ")) %>% 
   filter(values == 1) %>%
   select(categ))

# 3. Matrix multiplication
df2 <- tibble(b = dummyb, c = dummyc) %>% 
  mutate(a = 1*(b == 0 & c == 0)) %>% 
  relocate(a) # put col "a" up front
df2
(cat3 <- as.matrix(df2) %*% 1:3)
(cat3 <- cat3 %>% recode("a", "b", "c"))


# install.packages("AMCP")
library(AMCP)
data("C11E5")
dat <- C11E5
dat
library(tidyverse)
dat <- tibble(id = 1:5, dat)
dat
pivot_longer(dat, cols = -id, 
             values_to = "outcome", 
             names_to = "condition")


df2 <- tibble(dates = c("01/23/2020", "2/25/2021", "12/4/2020", "11/09/2019"))
df2

parse_date(x = df2$dates, format = "%m/%d/%Y")

#######
# Code for Part II video
#######

# Load data from package AMCP
library(AMCP)
data(C11E17)
df1 <- C11E17

# Data in wide format convenient for cor()
round(cor(df1), 2)

# Convert from wide to long with pivot_longer()
df1_long <- df1 %>% pivot_longer(cols = everything(),
                                 names_to = "Day",
                                 names_prefix = "Day",
                                 values_to = "Weight")
print(df1_long, n = Inf)

# Add id number for infant
df1_long <- df1 %>% 
  mutate(id = row_number()) %>%
  pivot_longer(cols = Day1:Day4,
               names_to = "Day",
               values_to = "Weight")
df1_long

# Drop the Day prefix
df1_long <- df1 %>% 
  mutate(id = row_number()) %>%
  pivot_longer(cols = Day1:Day4,
               names_to = "Day",
               names_prefix = "Day",
               values_to = "Weight") %>%
  mutate(Day = as.numeric(Day))
df1_long

# Make a `spaghetti plot'
df1_long %>% ggplot(aes(x = Day, y = Weight)) + 
  geom_line(aes(group = id))

#######
# Code for Part III (No video)
#######

# CREATE A DATA FRAME TO WORK WITH
# 25 employees and rated by their managers based
# on five questions
manager <- c(67, 211, 263, 211, 67, 263, 162, 121, 243, 
             263, 211, 121, 211, 243, 211, 158, 30, 30,
             243, 76, 243, 67, 162, 243, 158)
country <- c("US", "UK", "UK", "UK", "US", "UK", "US", 
             "US", "UK", "UK", "UK", "US", "UK", "UK", 
             "UK", "US", "US", "US", "UK", "US", "UK", 
             "US", "US", "UK", "US")
gender <- c("F", "M", "M", "M", "F", "M", "F", "F", "M", 
            "M", "M", "F", "M", "M", "M", "F", "F", "F", 
            "M", "M", "M", "F", "F", "M", "F") 
age <- c(64, 46, 49, 31, -9999, 67, 22, 18, 64, 24, 37,
         36, 18, 20, 49, 28, 40, 39, 54, 47, 35, 49, 81, 
         58, 50) 
date <- c("2018-04-14", "2018-04-10", "2018-04-22", "2018-04-28",
          "2018-04-08", "2018-04-05", "2018-04-01", "2018-04-25",
          "2018-04-24", "2018-04-14", "2018-04-09", "2018-04-15",
          "2018-04-19", "2018-04-13", "2018-04-05", "2018-04-15",
          "2018-04-16", "2018-04-04", "2018-04-30", "2018-04-17",
          "2018-04-23", "2018-04-01", "2018-04-22", "2018-04-24",
          "2018-04-29")
q1 <- c(5, 3, 3, 3, 3, 1, 5, 5, 4, 3, 3, 4, 4, 3, 3, 
        NA, 2, 1, 4, 4, 5, 4, 4, 3, 3)
q2 <- c(4, 5, 2, 2, 2, 4, 2, 1, 1, 4, 3, 4, 1, 4, 1, 
        4, 4, 1, 2, 4, 4, 1, 3, 2, 2)
q3 <- c(5, 4, 5, 4, 3, 4, 5, 4, 4, 2, 3, 4, 3, 2, 4, 
        3, 2, 1, 4, NA, 3, 2, 5, 5, 5) 
q4 <- c(3, 2, 5, 4, 4, 3, 3, 4, 3, 2, 3, 3, 1, 3, 4, 
        4, 1, 4, 3, NA, 2, 3, 2, 2, 4) 
q5 <- c(5, 4, 1, 3, 4, 4, 2, 5, 1, 2, 3, 2, 5, 4, 3, 
        3, 2, 3, 3, NA, 3, 3, 2, 4, 3) 
leadership <- data.frame(manager, country, gender, date,
                         age, q1, q2, q3, q4, q5, 
                         stringsAsFactors=FALSE)
leadership
str(leadership)

# TABLES
help(table)

# One-way table of gender frequencies
(t1 <- table(leadership$gender))

# Two-way table of gender by employee country
(t2 <- table(leadership$gender,   # Left margin
             leadership$country))       # Right margin

# Three-way table of gender by employee
# country with different tables for each manager
(t3 <- table(leadership$gender,   # Left margin
             leadership$country,        # Right margin
             leadership$manager))       # Different tables

# Three-way table of gender by manager with
# different tables for each country
(t4 <- table(leadership$gender,
             leadership$manager,
             leadership$country))

# The function margin.table() computes the marginal
# sum of table entries for rows, columns, etc.
help(margin.table)

t2 # gender by country

# Marginal sum for gender (summing across country)
(m2_gend <- margin.table(x = t2, 
                         margin = 1))

# Marginal sum for country (summing across gender)
(m2_ctry <- margin.table(x = t2,
                         margin = 2))

# The function prop.table() computes the marginal
# proportions where the cell counts are divided 
# by the relevant marginal sums.
help(prop.table)

# Proportions based on gender totals.
(p2_gend <- prop.table(x = t2,
                       margin = 1))
round(x = p2_gend, digits = 2)
# This table may be used to answer questions 
# about what proportion of females/males
# were from each country. Rows add to 1.00.

(p2_ctry <- prop.table(x = t2,
                       margin = 2))
round(x = p2_ctry, digits = 2)
# This table may be used to answer questions 
# about what proportion of UK/US employees
# were male/female. Columns add to 1.00.

