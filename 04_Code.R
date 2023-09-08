# Load the acupuncture data (see notes)

#####
# One- and Two-Sample Statistics
#####
# Sample means
acupuncture<-read.csv("C:/Users/tonyg/Desktop/Academic/Grad/HUDM 5026/acupuncture.csv")
mean(acupuncture$pk1)
mean(acupuncture$pk5)

# Mean of group assignment variable
mean(acupuncture$group)

# Sample means for pk1 by treatment group
by(data = acupuncture$pk1,
   INDICES = acupuncture$group, 
   FUN = mean,
   na.rm = TRUE)

# Variance and SD for pain ratings at baseline and one year
var(acupuncture$pk1)
var(acupuncture$pk5)
sd(acupuncture$pk1)
sd(acupuncture$pk5)

# Correlation between baseline and one year pain
cor(acupuncture$pk1, acupuncture$pk5)

# Correlation matrix for the entire data set (rounded to 2 dec places)
round(cor(acupuncture), digits = 2)

#####
# Work with the Data
#####
# Convert group to a factor
str(acupuncture)
acupuncture$group <- factor(acupuncture$group,
                            levels = c(0, 1),
                            labels = c("C", "T"))
str(acupuncture)
levels(acupuncture$group)

# Some visualizations
boxplot(acupuncture$pk1 ~ acupuncture$group, 
        horizontal = TRUE, las = 1,
        xlab = "Pain Rating at Baseline",
        ylab = "Treatment Group",
        main = "Baseline Pain by Group")

boxplot(acupuncture$pk5 ~ acupuncture$group,
        xlab = "Pain Rating After 1 Year",
        ylab = "Treatment Group",
        main = "Posttest (1 yr) Pain by Group",
        horizontal = TRUE, las = 1)
boxplot(acupuncture$pk5,
        xlab = "Pain Rating After 1 Year",
        ylab = "Treatment Group",
        main = "Posttest (1 yr) Pain by Group",
        horizontal = FALSE, las = 1)
#####
# Baseline Balance
#####
# Assess baseline balance
# Check for baseline balance across treatment conditions on 
# age, sex, migraine, chronicity, and baseline severity to 
# see if randomization resulted in balance across groups,
# as expected.

# Calculate covariate means by treatment status
(mns_age <- by(data = acupuncture$age, INDICES = acupuncture$group, FUN = mean))
(mns_sex <- by(data = acupuncture$sex, INDICES = acupuncture$group, FUN = mean))
(mns_mig <- by(data = acupuncture$migraine, INDICES = acupuncture$group, FUN = mean))
(mns_chr <- by(data = acupuncture$chronicity, INDICES = acupuncture$group, FUN = mean))
(mns_pk1 <- by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = mean))

# Group sample sizes
(ns <- table(acupuncture$group))

# Calculate covariate group variances by treatment status
(vars_age <- by(data = acupuncture$age, INDICES = acupuncture$group, FUN = var))
(vars_sex <- by(data = acupuncture$sex, INDICES = acupuncture$group, FUN = var))
(vars_mig <- by(data = acupuncture$migraine, INDICES = acupuncture$group, FUN = var))
(vars_chr <- by(data = acupuncture$chronicity, INDICES = acupuncture$group, FUN = var))
(vars_pk1 <- by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = var))

# Cohen's d
(d_age <- (mns_age[2] - mns_age[1]) / 
    sqrt(((ns[2] - 1)*vars_age[2] + (ns[1] - 1)*vars_age[1]) / (ns[1] + ns[2] - 2)))
(d_sex <- (mns_sex[2] - mns_sex[1]) / sqrt(((ns[2] - 1)*vars_sex[2] + (ns[1] - 1)*vars_sex[1]) / (ns[1] + ns[2] - 2)))
(d_mig <- (mns_mig[2] - mns_mig[1]) / sqrt(((ns[2] - 1)*vars_mig[2] + (ns[1] - 1)*vars_mig[1]) / (ns[1] + ns[2] - 2)))
(d_chr <- (mns_chr[2] - mns_chr[1]) / sqrt(((ns[2] - 1)*vars_chr[2] + (ns[1] - 1)*vars_chr[1]) / (ns[1] + ns[2] - 2)))
(d_pk1 <- (mns_pk1[2] - mns_pk1[1]) / sqrt(((ns[2] - 1)*vars_pk1[2] + (ns[1] - 1)*vars_pk1[1]) / (ns[1] + ns[2] - 2)))

# Variance ratios]
(vr_age <- vars_age[2]/vars_age[1])
(vr_sex <- vars_sex[2]/vars_sex[1])
(vr_mig <- vars_mig[2]/vars_mig[1])
(vr_chr <- vars_chr[2]/vars_chr[1])
(vr_pk1 <- vars_pk1[2]/vars_pk1[1])

##### 
# Write a Function to Accomplish Above
#####
# This is a great example of when a function would be useful.
bal_check <- function(cv, gp) {
  # cv is the covariate
  # gp is the group vector with control group as refernce
  ns <- table(gp)
  mns <- by(cv, gp, mean, na.rm = TRUE)
  vars <- by(cv, gp, var, na.rm = TRUE)
  d <- (mns[2] - mns[1]) / sqrt(((ns[2] - 1)*vars[2] + (ns[1] - 1)*vars[1]) / (ns[1] + ns[2] - 2))
  r <- vars[2]/vars[1]
  out <- c(d, r); names(out) <- c("d", "r")
  out }

# Apply the function to check balance for each baseline covariate of interest
bal_check(cv = acupuncture$age, gp = acupuncture$group)
bal_check(cv = acupuncture$sex, gp = acupuncture$group)
bal_check(cv = acupuncture$migraine, gp = acupuncture$group)
bal_check(cv = acupuncture$chronicity, gp = acupuncture$group)
bal_check(cv = acupuncture$pk1, gp = acupuncture$group)

#####
# Now use bal_check() to write a new function that plots
#####
# Create a new function that uses the bal_check() function
# to make a plot
bal_plot <- function(dat, cov_names, gp) {
  # dat is the data frame where variables can be found
  # cov_names is the list of names of covariates
  # gp is group factor name
  out <- sapply(X = dat[, cov_names], FUN = bal_check, gp = dat[, gp])
  plot(t(out), pch = 19, 
       xlim = range(out[1,]) + c(-.1, .1),
       ylim = range(out[2,]) + c(-.2, .2),
       xlab = "Cohen's d (T - C)",
       ylab = "Variance Ratio (T/C)")
  abline(v = c(-.1, .1), lty = 2)
  abline(v = 0); abline(h = 1)
  abline(h = c(4/5, 5/4), lty = 2)
  text(t(out), labels = rownames(t(out)), pos = 1)
  out
}

# Use the function with our data
out <- bal_plot(dat = acupuncture,
                cov_names = c("age", "sex", "migraine", "chronicity", "pk1"),
                gp = "group")
out

# Adjust the function bal_plot() so that the margins are equally spaced
bal_plot <- function(dat, cov_names, gp) {
  # dat is the data frame where variables can be found
  # cov_names is the list of names of covariates
  # gp is group factor name
  out <- sapply(X = dat[, cov_names], FUN = bal_check, gp = dat[, gp])
  bound1 <- max(abs(range(out[1,])))
  bound2 <- max(abs(1 - out[2,]))
  plot(t(out), pch = 19, 
       xlim = c(0 - bound1 -.1, 0 + bound1 + .1),
       ylim = c(1 - bound2 -.2, 1 + bound2 + .2),
       xlab = "Cohen's d (T - C)",
       ylab = "Variance Ratio (T/C)")
  abline(v = c(-.1, .1), lty = 2)
  abline(v = 0); abline(h = 1)
  abline(h = c(4/5, 5/4), lty = 2)
  text(t(out), labels = rownames(t(out)), pos = 1)
  out
}

# Use the (new, edited) function with our data
out <- bal_plot(dat = acupuncture,
                cov_names = c("age", "sex", "migraine", "chronicity", "pk1"),
                gp = "group")
out
grid.arrange(plt, tbl,
             nrow=2,
             as.table=TRUE,
             heights=c(3,1))
