# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 6 practical: example solution
#
# This is ONE way to solve the practical. It is not the only way: other code
# that gives the same answers is just as good. You will learn much more if you
# solve the practical yourself first (with other students, if you can), and
# only then compare your solution with this one.
#
# How to use this script
# - Save it in your BIO144 RStudio project folder and open it in RStudio.
# - Run it one line (or one command) at a time, from the top.
# - Lines starting with # are comments. Read them: they explain each step and
#   point out what to look at in the output.
# - The datasets are read directly from the internet, so you need to be online.
#
# Theory: course book Chapter 6 (Multiple regression).
# =============================================================================


# Load the packages ----
library(tidyverse)  # read_csv(), dplyr functions, ggplot2
library(ggfortify)  # autoplot() for model diagnostic plots
library(car)        # vif() for collinearity
library(patchwork)  # put several ggplots side by side
library(GGally)     # ggpairs(): all pairwise scatter plots at once


# Part 1, step 1: The question ----
# Response variable: kcal.per.g (energy content of milk, kcal per gram).
# Explanatory variables: neocortex.perc (% of brain mass that is neocortex)
# and mass (average female body mass, kg).
# Answer (hypothesis): there may be a POSITIVE relationship between milk energy
# and neocortex size. Why: a large neocortex is energetically expensive to grow,
# so species with a larger neocortex may need more energy-rich milk to support
# brain growth in their young.


# Part 1, step 2: Read the data ----
milk_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/milk_rethinking.csv")
milk_data
# The data are already clean and tidy, so no wrangling is needed.

# How many observations (species)?
nrow(milk_data)
# Answer: 17. Each row is one primate species.


# Part 1, step 2: Distributions of the three variables ----
# With only 17 values we use few bins.
ggplot(milk_data, aes(x = kcal.per.g)) +
  geom_histogram(bins = 7)
ggplot(milk_data, aes(x = neocortex.perc)) +
  geom_histogram(bins = 7)
ggplot(milk_data, aes(x = mass)) +
  geom_histogram(bins = 7)
# Answer: with only 17 values it is hard to say if a variable is normally
# distributed, and kcal.per.g and mass look a bit (right) skewed.
# neocortex.perc is NOT very skewed.

# The dataset already has log10-transformed versions of kcal.per.g and mass.
# Check that they are less skewed:
ggplot(milk_data, aes(x = log10_kcal.per.g)) +
  geom_histogram(bins = 7)
ggplot(milk_data, aes(x = log10_mass)) +
  geom_histogram(bins = 7)
# Yes: log10_mass in particular is much less skewed than mass.
# From now on we use log10_kcal.per.g, neocortex.perc and log10_mass.


# Part 1, step 2: Visualise the data with the question in mind ----
# Three scatter plots: response against each explanatory variable, and the
# two explanatory variables against each other.
p_kcal_mass <- ggplot(milk_data, aes(x = log10_mass, y = log10_kcal.per.g)) +
  geom_point() +
  labs(x = "log10 body mass (kg)", y = "log10 milk energy (kcal/g)")
p_kcal_neocortex <- ggplot(milk_data, aes(x = neocortex.perc, y = log10_kcal.per.g)) +
  geom_point() +
  labs(x = "Neocortex (% of brain mass)", y = "log10 milk energy (kcal/g)")
p_mass_neocortex <- ggplot(milk_data, aes(x = log10_mass, y = neocortex.perc)) +
  geom_point() +
  labs(x = "log10 body mass (kg)", y = "Neocortex (% of brain mass)")
p_kcal_mass + p_kcal_neocortex + p_mass_neocortex
# Answer (log10_kcal.per.g vs log10_mass): none, no clear relationship.
# Answer (log10_kcal.per.g vs neocortex.perc): none, no clear relationship.
# Answer (log10_mass vs neocortex.perc): positive. Heavier species tend to
# have a larger percentage of neocortex.
# Answer (what else is clear?): the two explanatory variables are correlated
# with each other (collinearity). This can make the slope estimates less
# precise (larger standard errors) and harder to interpret.

# All pairwise scatter plots at once (like pairs() in the practical, but with
# ggplot2). The numbers are the correlation coefficients.
milk_data |>
  select(log10_kcal.per.g, neocortex.perc, log10_mass) |>
  ggpairs()

# Degrees of freedom for error, BEFORE fitting the model:
# n - number of estimated parameters = 17 - 3 (intercept + 2 slopes).
nrow(milk_data) - 3
# Answer: 14.


# Part 1, step 3: Fit the multiple regression model ----
# The two explanatory variables are separated with a "+".
m_milk <- lm(log10_kcal.per.g ~ neocortex.perc + log10_mass, data = milk_data)

# Check the model assumptions with the four diagnostic plots.
autoplot(m_milk, smooth.colour = NA)
# (A warning about removed rows comes from smooth.colour = NA. Ignore it.)
# Answer (patterns in the residuals?): perhaps. There is no strong pattern,
# but with only 17 points it is hard to be confident.
# Answer (QQ-plot OK?): perhaps. No strong deviation from the line, but with
# so few points we should be cautious.


# Part 1, step 3: Look at the coefficients ----
summary(m_milk)
# Answer (slope of log10_mass): -0.14 (two significant figures; -0.1446).
# Answer (how can we tell the log10_mass slope is unlikely to be due to
# chance?): its p-value is small (0.0014). Such an extreme estimate would be
# unlikely if the true slope were zero.
# Answer (slope of neocortex.perc): 0.018 (two significant figures; 0.01836).
# Answer (adjusted vs multiple R-squared): adjusted R-squared (0.48) is
# smaller than multiple R-squared (0.54) because it corrects for the number
# of explanatory variables (it penalises unnecessary complexity).
# Note: "Residual standard error: ... on 14 degrees of freedom", as we predicted.

# 95% confidence intervals of the coefficients:
confint(m_milk)
# The same "by hand": estimate +/- t-critical value * standard error.
# With 14 residual df use qt(0.975, df = 14), about 2.14, NOT 1.96.
est_neocortex <- summary(m_milk)$coefficients["neocortex.perc", "Estimate"]
se_neocortex <- summary(m_milk)$coefficients["neocortex.perc", "Std. Error"]
t_crit <- qt(0.975, df = df.residual(m_milk))
t_crit
est_neocortex - t_crit * se_neocortex
# Answer (lower bound of the 95% CI of the neocortex.perc slope): 0.0073.
# (With 1.96 you would get 0.0083: an interval that is too narrow.)


# Part 1, step 4: Conditional effects plots with confidence bands ----
# (Course book Chapter 6, "Question 5: How do we make predictions?")
# To show the relationship with one explanatory variable "conditioned on" the
# other, we predict over a range of the first variable while holding the
# second at its mean.

# Milk energy against log10 body mass, with neocortex.perc at its mean:
new_data_mass <- tibble(
  log10_mass = seq(min(milk_data$log10_mass), max(milk_data$log10_mass),
                   length.out = 100),
  neocortex.perc = mean(milk_data$neocortex.perc)
)
pred_mass <- predict(m_milk, newdata = new_data_mass, interval = "confidence")
new_data_mass <- cbind(new_data_mass, pred_mass)
p_cond_mass <- ggplot(new_data_mass, aes(x = log10_mass, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line() +
  labs(x = "log10 body mass (kg)",
       y = "Predicted log10 milk energy\n(neocortex at its mean)")

# Milk energy against neocortex.perc, with log10_mass at its mean:
new_data_neocortex <- tibble(
  neocortex.perc = seq(min(milk_data$neocortex.perc), max(milk_data$neocortex.perc),
                       length.out = 100),
  log10_mass = mean(milk_data$log10_mass)
)
pred_neocortex <- predict(m_milk, newdata = new_data_neocortex, interval = "confidence")
new_data_neocortex <- cbind(new_data_neocortex, pred_neocortex)
p_cond_neocortex <- ggplot(new_data_neocortex, aes(x = neocortex.perc, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line() +
  labs(x = "Neocortex (% of brain mass)",
       y = "Predicted log10 milk energy\n(body mass at its mean)")

p_cond_mass + p_cond_neocortex
# Look at: once the other variable is held constant, milk energy clearly
# decreases with body mass and increases with neocortex size. We could not
# see this in the bivariate scatter plots, because the two explanatory
# variables are positively correlated and have opposite effects: they
# "hide" each other.

# Model reporting sentences (biology first, then the statistics):
# "Among 17 primate species, milk was more energy-rich in species with a
# larger neocortex, when female body mass was taken into account: energy
# content (log10 kcal/g) increased by 0.018 per percentage point of neocortex
# (95% CI 0.007 to 0.029; t = 3.58, df = 14, p = 0.003). At a given neocortex
# size, larger species produced less energy-rich milk (slope -0.14 per log10
# unit of body mass, 95% CI -0.22 to -0.07; t = -3.96, df = 14, p = 0.001).
# Together the two variables explained about half of the variation in milk
# energy (R-squared = 0.54). Neither relationship was visible without taking
# the other variable into account, because larger species also have a larger
# neocortex."


# Part 1, step 5: Unique contributions of each explanatory variable ----
# (Course book Chapter 6, "Assessing the importance of an explanatory
# variable in the presence of collinearity".)
# Fit two reduced models, each leaving out one explanatory variable,
# and compare each with the full model with an F-test.
m_milk_neocortex <- lm(log10_kcal.per.g ~ neocortex.perc, data = milk_data)
m_milk_mass <- lm(log10_kcal.per.g ~ log10_mass, data = milk_data)

# Does adding log10_mass improve a model that already has neocortex.perc?
anova(m_milk_neocortex, m_milk)
# Yes: F = 15.70, p = 0.0014.

# Does adding neocortex.perc improve a model that already has log10_mass?
anova(m_milk_mass, m_milk)
# Answer (F-statistic, model with log10_mass vs model with both): 12.78.
# (p = 0.003. Note that F = t^2 of the neocortex slope: 3.576^2 = 12.78.)

# Partial R-squared of neocortex.perc = (RSS of reduced - RSS of full) / RSS of reduced.
rss_mass <- sum(residuals(m_milk_mass)^2)
rss_full <- sum(residuals(m_milk)^2)
(rss_mass - rss_full) / rss_mass
# Answer: 0.48 = (0.175811 - 0.091895) / 0.175811.
# Answer (meaning): of the variation in milk energy NOT explained by
# log10_mass, adding neocortex.perc explains 48%.


# Part 1, step 6: Collinearity (VIF) ----
vif(m_milk)
# Answer (VIF of neocortex.perc): 2.29. With only two explanatory variables
# both VIFs are the same. The variance of each slope is about 2.3 times larger
# than without collinearity (standard errors about sqrt(2.29) = 1.5 times
# larger). This is below the rule-of-thumb limits of 5 or 10.


# Part 1, step 6: Standardised coefficients ----
# scale() subtracts the mean and divides by the standard deviation, so all
# slopes are in "standard deviations per standard deviation" and can be compared.
m_milk_standardised <- lm(scale(log10_kcal.per.g) ~ scale(neocortex.perc) + scale(log10_mass),
                          data = milk_data)
summary(m_milk_standardised)
# Answer (standardised coefficient of log10_mass): -1.08.
# (neocortex.perc: 0.98.) So the two variables are about equally important,
# but act in opposite directions. Compare the t-values and p-values with
# summary(m_milk): they are the same.
# Answer (what do we lose by standardising?): the coefficients are no longer
# in biological units, and they depend on how variable each explanatory
# variable happens to be in this sample. (The p-values do NOT change, and
# the model still accounts for collinearity.) It is often good to report both.


# Part 2: Why we use adjusted R-squared ----
# We add random explanatory variables (pure noise) one at a time, and record
# R-squared and adjusted R-squared each time.
# set.seed() makes the random numbers the same each time you run the script.
# Your own random numbers will be different, so your numbers will differ a
# little, but the pattern will be the same.
set.seed(3)

# Row 0: the model with no random variables (m_milk from Part 1).
r2_table <- tibble(
  n_random = 0,
  r_squared = summary(m_milk)$r.squared,
  adj_r_squared = summary(m_milk)$adj.r.squared
)

# Rows 1 to 10: add r1, then r2, ... up to r10. A for loop does the
# repetitive work: each time it makes one new random variable, adds it to
# the model formula, refits the model, and adds one row to the table.
milk_random <- milk_data
model_formula <- "log10_kcal.per.g ~ neocortex.perc + log10_mass"
for (i in 1:10) {
  new_var <- paste0("r", i)
  milk_random[[new_var]] <- rnorm(17)
  model_formula <- paste(model_formula, "+", new_var)
  m_milk_random <- lm(as.formula(model_formula), data = milk_random)
  r2_table <- r2_table |>
    add_row(n_random = i,
            r_squared = summary(m_milk_random)$r.squared,
            adj_r_squared = summary(m_milk_random)$adj.r.squared)
}
# The last model formula, with all 10 random variables:
model_formula
r2_table

# The same as a graph:
r2_table |>
  pivot_longer(cols = c(r_squared, adj_r_squared),
               names_to = "measure", values_to = "value") |>
  ggplot(aes(x = n_random, y = value, colour = measure)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of random explanatory variables", y = "Value", colour = "")
# Look at: R-squared goes up every time we add a random variable (it can
# never go down), from 0.54 to 0.74 here, even though the random variables
# contain no information at all. Adjusted R-squared does not do this: here it
# falls from 0.48 to below zero, because it penalises each extra explanatory
# variable. This is why we use adjusted R-squared in multiple regression.
# Be aware: with only 17 species, adjusted R-squared is itself quite
# variable. Change the number in set.seed() and run this part again a few
# times. Sometimes adjusted R-squared also goes up by chance, because with
# 10 random variables only 17 - 13 = 4 residual degrees of freedom are left.
# But unadjusted R-squared ALWAYS goes up.
# Answer (why does unadjusted R-squared increase?): by chance alone, a random
# variable usually explains a small amount of the variation in the response,
# and the model uses this chance pattern to improve the fit.


# Part 3: Confounding: read the data ----
# (Course book Chapter 6, "Confounding: why we include other explanatory
# variables". The data are simulated for teaching.)
stream_mayflies <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/stream_mayflies.csv")
stream_mayflies
nrow(stream_mayflies)

# Degrees of freedom, before fitting:
# Model with farmland_percent and altitude_m: 90 - 3 = 87.
# Answer: 87.
# With an extra categorical variable with 4 levels (3 extra parameters):
# 87 - 3 = 84.
# Answer: 84.


# Part 3: Graphs ----
p_mayfly_farmland <- ggplot(stream_mayflies, aes(x = farmland_percent, y = mayfly_density)) +
  geom_point() +
  labs(x = "Farmland in catchment (%)", y = "Mayfly density (per m2)")
p_mayfly_altitude <- ggplot(stream_mayflies, aes(x = altitude_m, y = mayfly_density)) +
  geom_point() +
  labs(x = "Altitude (m)", y = "Mayfly density (per m2)")
p_farmland_altitude <- ggplot(stream_mayflies, aes(x = altitude_m, y = farmland_percent)) +
  geom_point() +
  labs(x = "Altitude (m)", y = "Farmland in catchment (%)")
p_mayfly_farmland + p_mayfly_altitude + p_farmland_altitude
# Look at: fewer mayflies with more farmland; more mayflies at higher altitude;
# and less farmland at higher altitude. So altitude is related to BOTH the
# explanatory variable of interest and the response: a possible confounder.


# Part 3: Fit the two models ----
m_mayfly_farmland <- lm(mayfly_density ~ farmland_percent, data = stream_mayflies)
m_mayfly_farmland_altitude <- lm(mayfly_density ~ farmland_percent + altitude_m,
                                 data = stream_mayflies)

# Check the assumptions of the model with both variables:
autoplot(m_mayfly_farmland_altitude, smooth.colour = NA)
# Look at: no clear patterns in the residuals, points close to the QQ line,
# no points with both high leverage and a large residual. The assumptions
# look reasonably well met.

summary(m_mayfly_farmland)
# Answer (slope of farmland, simple model): -0.65 mayflies per m2 per % farmland.

summary(m_mayfly_farmland_altitude)
confint(m_mayfly_farmland_altitude)
# Answer (slope of farmland, adjusted for altitude): -0.35
# (95% CI -0.47 to -0.22). About half the size of the simple-model slope.
# Answer (why so different?): altitude is a confounder. It is associated with
# farmland (less farmland at high altitude) and with mayflies (more mayflies
# at high altitude). Without altitude in the model, farmland "takes the
# credit" for part of the effect of altitude. (The true simulated value,
# -0.25, is inside the CI of the adjusted model, but far from -0.65.)


# Part 3: The cost of adjusting (standard errors and VIF) ----
# Standard error of the farmland slope in each model:
summary(m_mayfly_farmland)$coefficients["farmland_percent", "Std. Error"]
summary(m_mayfly_farmland_altitude)$coefficients["farmland_percent", "Std. Error"]
vif(m_mayfly_farmland_altitude)
# Answer: the standard error is larger with altitude in the model (0.064 vs
# 0.034), because farmland and altitude are correlated (VIF = 4.7). Including
# the confounder makes the estimate less biased but also less precise.
# A VIF above 1 is NOT a reason to remove altitude: leaving it out gives a
# biased estimate.

# Answer (can we conclude that 10% more farmland CAUSES a fall of about 3.5
# mayflies per m2?): No. This is an observational study. We can only adjust
# for confounders we measured. An unmeasured variable associated with both
# farmland and mayflies (e.g. geology, stream size) could still bias the
# estimate. Only an experiment with random assignment would break those links.

# Model reporting sentence:
# "After adjusting for altitude, mayfly density was lower in streams with more
# farmland in their catchment: density decreased by 0.35 individuals per m2
# for each additional percent of farmland (95% CI 0.22 to 0.47; t = -5.40,
# df = 87, p < 0.001), i.e. by about 3.5 individuals per m2 for 10% more
# farmland. Without adjusting for altitude the estimated decrease was almost
# twice as large (0.65 per percent), because farmland is more common at low
# altitude, where mayfly densities are naturally lower."


# Check that the script is reproducible ----
# Finally, check that the whole script runs without errors from a clean start:
# Session > Restart R, then run the whole script from the top (Ctrl+Shift+Enter,
# or Cmd+Shift+Enter on a Mac). If it runs to the end without errors, your
# analysis is reproducible.
