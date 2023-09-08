########################
# paste() and grep() ###
########################
# Paste function
paste("x", 1:100, sep = "", collapse = " + ")
 paste("y ~ ", paste("x", 1:100, sep = "", collapse = " + "), sep = "")
paste0("y ~ ", paste("x", 1:100, sep = "", collapse = " + "))
fmla1 <- as.formula(paste("y ~ ", paste("x", 1:100, sep = "", collapse = " + ")))
fmla1

# Characters with spaces are no problem for paste()
# but cause an error when making a formula
paste(colnames(state.x77)[-5], collapse = " + ")
fmla2 <- as.formula(paste("Murder ~", paste(colnames(state.x77[,-5]), collapse = " + ")))

# Use back ticks
pst2 <- paste0("Murder ~ ", 
               paste(colnames(state.x77)[c(1:3, 7:8)], collapse = " + "),
               " + `Life Exp` + `HS Grad`")
pst2
fmla2 <- as.formula(pst2)
lm1 <- lm(fmla2, data = as.data.frame(state.x77))
summary(lm1)

# Define three string vectors
txt1 <- c("The", "licenses", "for", "most", "software", "are",
          "designed", "to", "take", "away", "your", "freedom",
          "to", "share", "and", "change", "it.",
          "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
          "is", "intended", "to", "guarantee", "your", "freedom", "to",
          "share", "and", "change", "free", "software", "--",
          "to", "make", "sure", "the", "software", "is",
          "free", "for", "all", "its", "users")
txt2 <- "The quick brown fox jumps over the lazy dog."
txt3 <- c("Reread", "Aperture", "Careful", "Preregistration")

# Explore some functions and arguments
grep(pattern = "re", x = txt1, ignore.case = TRUE)
grep(pattern = "re", x = txt1, ignore.case = TRUE, value = TRUE)
grepl(pattern = "re", x = txt1, ignore.case = TRUE)
which(grepl(pattern = "re", x = txt1, ignore.case = TRUE))

grep(pattern = "th", x = txt1, ignore.case = TRUE)
grep(pattern = "th", x = txt1, ignore.case = TRUE, value = TRUE)

# Pattern recognition and substitution
sub(pattern = "re", replacement = "RE", x = txt3, ignore.case = TRUE)
gsub(pattern = "re", replacement = "RE", x = txt3, ignore.case = TRUE)

# Other useful functions
txt2
substr(x = txt2, start = 5, stop = 19)
nchar(x = txt1)
strsplit(x = txt3, split = "t", fixed = TRUE)

###############################################
# Functions in stringr package in tidyverse ###
###############################################
library(tidyverse)
chr1 <- c("a", "5126", "R for Data Science", NA, 
          "The quick brown fox jumps over the lazy dog")
chr1

# Length of the vector
length(chr1)

# Number of characters in each element
nchar(chr1)
str_length(chr1)

# Note that quoted NA is not missing
(chr2 <- c("NA", NA, NA, "NA", "NA"))
str_length(chr2)

# To combine multiple character strings into one, use 
# str_c()
str_c("AB", "cd12", "bb8-45")

# Use sep = argument to pick the character used to separate them
str_c("AB", "cd12", "bb8-45")
str_c("AB", "cd12", "bb8-45", sep = " ")
str_c("AB", "cd12", "bb8-45", sep = " |*&*&*| ")

# str_c() is vectorized. Shorter vectors are recycled to the
# length of longer with a warning if not evenly divisible.
chr3 <- c("a", "e", "i", "o", "u")
chr4 <- c("1", "2", "3")
str_c(chr3, chr4)

# To collapse a vector of strings into a single string 
# use collapse argument
chr3
str_c(chr3, collapse = "")
str_c(chr3, collapse = " ")

# To extract parts of a string use str_sub().
chr5 <- c('The', 'quick', 'brown', 'fox', 'jumps', 
          'over', 'the', 'lazy', 'dog')
chr5
str_length(chr5)
str_c(chr5, collapse = " ")

str_sub(chr5, 1, 3)   # First through third characters of each string
str_sub(chr5, -3, -1) # 3rd-to-last to last characters.
str_sub(chr5, 1, 20)  # No error if you ask for too much.

# Can use str_sub() to assign characters.
(chr6 <- chr5)
str_sub(chr6, 2, 3)
str_sub(chr6, 2, 3) <- "BB"
chr6

chr6 <- chr5
chr6

# Change case with str_to_upper() and str_to_lower()
str_to_upper(chr6)

# Change first letter of each string to upper case.
str_sub(chr6, 1, 1)
str_sub(chr6, 1, 1) <- str_to_upper(str_sub(chr6, 1, 1))
chr6

#########################
# Regular Expressions ###
#########################
# Matching patterns with regular expressions
chr7 <- c("apple", "banana", "pear")

# str_view() and str_view_all() show regex matches in the viewer
str_view(chr7, "an")
str_view_all(chr7, "an")
str_view_all(chr7, "an", match = TRUE)

chr6 <- chr5
chr6
inds <- grep(pattern = "ow|th", ignore.case = TRUE, x = chr6)
inds
for (i in 1:length(inds)) {chr6[inds[i]] <- str_to_title(chr6[inds[i]])}
chr6

# When placed in quotes, the . will match any character
str_view(chr7, ".a.")
str_view_all(chr7, ".a.")
str_view("bananana", ".a.")

# No overlap allowed in matches
str_view_all("bananana", ".a.")

# What if you want all instances, including overlapping instances?
# Need to use lookahead.
str_view_all("bananana", "(?=ana)")

chr5
str_view(chr5, "t.", )

# But how to match . itself? Use double backslash
str_view(c("a.b.c", "a.c", "bef"), "\\.")
str_view_all(c("a.b.c", "a.c", "bef"), "\\.")
grep("\\.", c("a.b.c", "a.c", "bef"))

# Anchors are used to represent the beginnings and ends of strings
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

# To force a regular expression to only match a complete string, 
# anchor it with both ^ and $:
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

# Detecting repetition
# ? is 0 or 1
# + is 1 or more
# * is 0 or more
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, 'C[LX]+')

# Can also specify the number exactly where
# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m (blank = Inf)
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

# By default these functions do greedy matching (longest possible match)
# Can make them do lazy matching by putting ? at end
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')


# Look at exercises
# 14.3.1.1 (problems 1, 2, 3)
# 14.3.2.1 (problem 2 only)
# 14.3.3.1 (problem 1 only)
# 14.3.4.1 (problems 1, 2, 3)

# Solutions can be found here: 
# https://jrnold.github.io/r4ds-exercise-solutions/strings.html
