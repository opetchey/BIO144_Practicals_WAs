# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 8 practical: example solution
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
# Theory: course book Chapter 8 (count data, Poisson GLM), with the GLM
# checklist from Chapters 8 and 9, and collinearity from Chapter 6.
# =============================================================================


# Load the packages ----

library(tidyverse)  # read_csv(), ggplot2, dplyr, and more
library(ggfortify)  # autoplot() for model diagnostic plots
library(GGally)     # ggpairs(): all pairwise scatterplots
library(car)        # vif(): variance inflation factors (collinearity)


# =============================================================================
# Practical part 1: ticks on red grouse chicks (Poisson GLM) ----
# =============================================================================

# Question: how does the number of ticks on a chick change with altitude,
# taking into account differences between years?


# Part 1, step 1: Load the data and make some basic checks ----

grouse_ticks <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/grouse_ticks.csv")

# Make year a factor: we want a separate mean for each year, not a linear
# trend across only three years.
grouse_ticks <- grouse_ticks |>
  mutate(year = factor(year))

glimpse(grouse_ticks)

# How many chicks are in the dataset? Each row is one chick.
nrow(grouse_ticks)
# Answer: 403 chicks.

# Some more checks: missing values, and the number of broods and locations.
sum(is.na(grouse_ticks))
n_distinct(grouse_ticks$brood)
n_distinct(grouse_ticks$location)
table(grouse_ticks$year)

# Histogram of the response variable.
ggplot(grouse_ticks, aes(x = ticks)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Number of ticks on the chick's head", y = "Number of chicks")

# Why is a linear model with normal errors a poor choice here?
# Answer: (1) the response is a count: whole numbers that cannot be negative;
# (2) the distribution is strongly right-skewed, with many zeros and a few
# chicks with very many ticks; (3) for counts the variance usually increases
# with the mean, so the constant-variance assumption is violated.
# (A linear model CAN include a categorical variable such as year, so that
# answer is wrong.)


# Part 1, step 2: Visualise ----

# Number of ticks against altitude, one colour per year.
ggplot(grouse_ticks, aes(x = altitude_m, y = ticks, colour = year)) +
  geom_jitter(width = 2, height = 0, alpha = 0.6) +
  labs(x = "Altitude (m)", y = "Number of ticks", colour = "Year")

# The same with a log scale on the y-axis. We add 1 because log(0) is not
# defined. On this scale the decline with altitude looks roughly linear,
# which is what a Poisson GLM with a log link assumes.
ggplot(grouse_ticks, aes(x = altitude_m, y = ticks + 1, colour = year)) +
  geom_jitter(width = 2, height = 0, alpha = 0.6) +
  scale_y_log10() +
  labs(x = "Altitude (m)", y = "Number of ticks + 1 (log scale)", colour = "Year")

# What we see: far fewer ticks at higher altitude, in all three years, and
# fewer ticks in 1997 than in the other years.


# Part 1, step 3: Plan and fit the model ----

# Expected residual degrees of freedom: 403 observations minus 4 parameters
# (intercept, altitude slope, and 2 year differences for 3 years).
403 - 4
# Answer: 399.

m_ticks_poisson <- glm(ticks ~ altitude_m + year, family = poisson,
                       data = grouse_ticks)
summary(m_ticks_poisson)
# Check: "Residual deviance: ... on 399 degrees of freedom", as expected.


# Part 1, step 4: Check the model (GLM checklist) ----

# Diagnostic plots. For a GLM these are only a rough guide (Chapter 8).
autoplot(m_ticks_poisson, which = 1:4, add.smooth = TRUE)
# Look at: the QQ-plot points go far above the line on the right, and the
# scale-location plot shows many large residuals: signs of overdispersion.

# Dispersion = residual deviance / residual degrees of freedom.
deviance(m_ticks_poisson) / df.residual(m_ticks_poisson)
# Answer: 8.63 (3443.8 / 399). Far above 1 (and above the rule of thumb of
# 1.5 to 2): the data are strongly overdispersed.

# Consequence of ignoring overdispersion:
# Answer: the standard errors and p-values are too small (too many false
# positives). A pragmatic fix is a quasi-Poisson model, which estimates the
# dispersion and inflates the standard errors. (The estimates themselves are
# not biased.)

# Zeros: observed number of chicks with no ticks ...
sum(grouse_ticks$ticks == 0)
# ... and the number the Poisson model expects (sum over all chicks of the
# Poisson probability of a zero, given each chick's fitted mean).
sum(dpois(0, lambda = fitted(m_ticks_poisson)))
# Answer: 126 observed, about 63 expected. There are about twice as many zeros
# as a Poisson model predicts (possible zero inflation). This is one reason
# for the overdispersion. Zero-inflated or negative binomial models could deal
# with it, but they are beyond BIO144. We do NOT delete the zeros.

# Independence: this comes from the study design, not from the plots.
n_distinct(grouse_ticks$brood)
# Answer: 403 chicks come from only 118 broods. Chicks from the same brood
# share a nest and parents, so they probably have more similar tick numbers:
# the observations are not independent (pseudoreplication). A mixed model with
# brood as a random effect (Chapter 11) would deal with this. Here we continue
# with a GLM as an approximation, and remember that the p-values are probably
# too small.

# Now fit the quasi-Poisson model.
m_ticks_quasi <- glm(ticks ~ altitude_m + year, family = quasipoisson,
                     data = grouse_ticks)
summary(m_ticks_quasi)

# Compare the estimates and standard errors of the two models.
coef(summary(m_ticks_poisson))[, 1:2]
coef(summary(m_ticks_quasi))[, 1:2]
# Look at: the estimates are identical. The quasi-Poisson standard errors are
# larger, by the square root of the estimated dispersion parameter (shown in
# the quasi-Poisson summary; it is about 12.5, so the factor is about 3.5).
sqrt(summary(m_ticks_quasi)$dispersion)


# Part 1, step 5: Test whether altitude is associated with tick numbers ----

# Compare the Poisson models with and without altitude.
m_ticks_poisson_year <- glm(ticks ~ year, family = poisson, data = grouse_ticks)
anova(m_ticks_poisson_year, m_ticks_poisson, test = "Chisq")
# Answer: the deviance falls by 1105.6, and the test has 1 degree of freedom,
# because the larger model estimates one more parameter (the altitude slope).
# But this chi-squared test ignores the overdispersion, so it is far too
# optimistic.

# With the quasi-Poisson models, use an F-test instead.
m_ticks_quasi_year <- glm(ticks ~ year, family = quasipoisson,
                          data = grouse_ticks)
anova(m_ticks_quasi_year, m_ticks_quasi, test = "F")
# Answer: F = 88.6 on 1 and 399 degrees of freedom, p < 0.0001. Even after
# allowing for overdispersion, there is very strong evidence that tick numbers
# are associated with altitude.


# Part 1, step 6: Interpret the effect sizes ----

# The coefficients are on the log (link) scale. exp() turns them into
# multiplicative effects on the expected number of ticks.
coef(m_ticks_quasi)

# Effect of an increase in altitude of 100 m.
exp(100 * coef(m_ticks_quasi)["altitude_m"])
# Answer: 0.117. exp(100 x -0.02145) = 0.117: each extra 100 m of altitude
# multiplies the expected number of ticks by 0.117, i.e. about 88% fewer.
# (The data span only about 400 to 530 m, so this is a strong effect.)

# 95% confidence interval for this effect: calculate it on the log scale,
# then back-transform.
exp(100 * confint.default(m_ticks_quasi)["altitude_m", ])
# Look at: about 0.07 to 0.19.

# Year 1997 compared with 1995 (the reference year).
exp(coef(m_ticks_quasi)["year1997"])
# Answer: the year1997 coefficient (-1.685) means that, at the same altitude,
# the expected number of ticks in 1997 was exp(-1.685) = 0.19 times that in
# 1995, i.e. about 81% fewer. (It does NOT mean 1.685 fewer ticks.)


# Part 1, step 7: Predictions and a figure ----

# Expected number of ticks at 400 m and 500 m in 1996.
new_sites <- data.frame(altitude_m = c(400, 500),
                        year = factor("1996", levels = levels(grouse_ticks$year)))
predict(m_ticks_quasi, newdata = new_sites, type = "response")
# Answer: 28.8 ticks at 400 m (and about 3.4 at 500 m).

# Figure: data with the fitted relationship and 95% CI for each year.
# Predict on the link scale with standard errors, calculate the interval
# there, then back-transform the fit and both limits with exp().
new_ticks <- expand_grid(
  altitude_m = seq(min(grouse_ticks$altitude_m), max(grouse_ticks$altitude_m),
                   length.out = 100),
  year = factor(levels(grouse_ticks$year), levels = levels(grouse_ticks$year))
)
pred_ticks <- predict(m_ticks_quasi, newdata = new_ticks, type = "link",
                      se.fit = TRUE)
new_ticks <- new_ticks |>
  mutate(ticks = exp(pred_ticks$fit),
         lower = exp(pred_ticks$fit - 1.96 * pred_ticks$se.fit),
         upper = exp(pred_ticks$fit + 1.96 * pred_ticks$se.fit))

ggplot(grouse_ticks, aes(x = altitude_m, y = ticks, colour = year)) +
  geom_jitter(width = 2, height = 0, alpha = 0.4) +
  geom_ribbon(data = new_ticks, aes(ymin = lower, ymax = upper, fill = year),
              alpha = 0.2, colour = NA) +
  geom_line(data = new_ticks, linewidth = 1) +
  labs(x = "Altitude (m)", y = "Number of ticks per chick",
       colour = "Year", fill = "Year") +
  theme_bw()


# Part 1, step 8: Report ----

# Answer (best reporting sentence): "The number of ticks on grouse chicks
# declined steeply with altitude: each additional 100 m was associated with an
# 88% reduction in the expected number of ticks (multiplicative effect 0.12,
# 95% CI 0.07-0.19; quasi-Poisson GLM with year as a covariate,
# F(1, 399) = 88.6, p < 0.0001)."
# WHY: it gives the direction and size of the effect on an understandable
# scale, its uncertainty, and the test, from a model that accounts for the
# overdispersion.

# Critique and reflection (model answer):
# - Chicks are grouped in broods (and broods in locations), so the 403 chicks
#   are not independent: the p-values are too small. A mixed model (Chapter 11)
#   would be better. Note that altitude is measured per location, so the real
#   number of independent altitude values is much smaller than 403.
# - There are about twice as many zeros as the Poisson model expects.
#   Quasi-Poisson only inflates the standard errors; it does not model the
#   extra zeros.
# - This is an observational study: altitude may not be the cause. Things that
#   change with altitude (temperature, humidity, vegetation, host density)
#   could be the real causes.


# =============================================================================
# Practical part 2 (optional): collinearity in a Poisson GLM (abalone) ----
# =============================================================================

# Step 0 (the biological question): can non-lethal measurements of size
# (length, diameter, height, whole weight) tell us the age of an abalone
# (number of shell rings)? The response is a count, so we start with a
# Poisson GLM.


# Part 2, step 1: Load the data and make some basic checks ----

abalone_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/abalone_age.csv")
glimpse(abalone_data)

# How many observations have NA in at least one of the five variables we use?
# Check this BEFORE removing NAs.
abalone_data |>
  filter(is.na(Rings) | is.na(Length_mm) | is.na(Diameter_mm) |
           is.na(Height_mm) | is.na(Whole_weight_g)) |>
  nrow()
# Answer: 0. No missing values in these five variables.

# Keep only the variables we use (only non-lethal measurements).
abalone_data <- abalone_data |>
  select(Rings, Length_mm, Diameter_mm, Height_mm, Whole_weight_g) |>
  na.omit()
nrow(abalone_data)


# Part 2, step 2: Visualise ----

# All pairwise scatterplots (pairs(abalone_data) does the same in base R).
ggpairs(abalone_data)
# Answer: all four explanatory variables (Length_mm, Diameter_mm, Height_mm,
# Whole_weight_g) look associated with Rings.
# Answer (why all four?): because the explanatory variables are all measures
# of size and are very strongly correlated with each other (look at the
# correlation coefficients, most above 0.8).
# Answer (why does this concern us?): strong correlations between explanatory
# variables (collinearity) make coefficient estimates unstable and difficult
# to interpret, and inflate their standard errors (Chapter 6).


# Part 2, step 3: Fit the model ----

m_rings_all <- glm(Rings ~ Length_mm + Diameter_mm + Height_mm + Whole_weight_g,
                   data = abalone_data,
                   family = poisson)

# Variance inflation factors (Chapter 6). Values above about 5 to 10 mean
# strong collinearity.
vif(m_rings_all)
# Look at: VIFs of about 35 for Length_mm and Diameter_mm, about 8 for
# Whole_weight_g and about 5 for Height_mm: strong collinearity.


# Part 2, step 4: Check the model ----

autoplot(m_rings_all, smooth.colour = NA)
# (The warnings "Removed 400 rows ... geom_line()" come from switching off the
# smooth lines with smooth.colour = NA. You can ignore them.)
# Answer (most concerning feature): the QQ-plot shows that the larger
# residuals are considerably larger than expected under the model.

summary(m_rings_all)

# How to assess overdispersion: residual deviance / residual degrees of freedom.
deviance(m_rings_all) / df.residual(m_rings_all)
# Answer: 0.52 (204.79 / 395). Below 1: this suggests UNDERdispersion, even
# though the QQ-plot suggested some very large residuals. The two checks look
# at different things: the QQ-plot shows the extreme residuals, the ratio
# summarises all residuals. Here a few residuals are too large, but most are
# smaller than a Poisson model expects.


# Part 2, step 5: Interpret the model ----

summary(m_rings_all)
# Answer: Diameter_mm and Height_mm have p < 0.05; Length_mm and
# Whole_weight_g do not (after accounting for the others).

# To SEE the effect of collinearity (not to choose a model!), fit a model
# without Height_mm and Diameter_mm ...
m_rings_length_weight <- glm(Rings ~ Length_mm + Whole_weight_g,
                             data = abalone_data,
                             family = poisson)
summary(m_rings_length_weight)
# Look at: Length_mm is now significant; Whole_weight_g is still not.

# ... and a model with only Whole_weight_g.
m_rings_weight <- glm(Rings ~ Whole_weight_g,
                      data = abalone_data,
                      family = poisson)
summary(m_rings_weight)
# Look at: Whole_weight_g is now highly significant. Alone, it is strongly
# associated with Rings; together with correlated variables its effect cannot
# be separated from theirs (shared explanatory power).

# How good is the model? Compare the residual deviance with the null deviance.
m_rings_all$null.deviance
deviance(m_rings_all)
1 - deviance(m_rings_all) / m_rings_all$null.deviance
# Look at: the explanatory variables together reduce the deviance by about
# 39% (a rough "proportion of deviance explained").

# Predicted against observed values.
abalone_data <- abalone_data |>
  mutate(Predicted_Rings = predict(m_rings_all, type = "response"))

ggplot(abalone_data, aes(x = Predicted_Rings, y = Rings)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Predicted number of rings", y = "Observed number of rings")
# The dashed line is the 1:1 line (perfect prediction).

pred_obs_model <- lm(Rings ~ Predicted_Rings, data = abalone_data)
summary(pred_obs_model)
confint(pred_obs_model)
# If the model predicted well we would expect intercept = 0, slope = 1 and a
# high r-squared.
# Answer: No. The 95% CI of the intercept includes 0 and the 95% CI of the
# slope includes 1, so there is no evidence of systematic bias. But the
# r-squared is only moderate (about 0.35), so predictions for individual
# abalone are not very precise.


# Part 2, step 6: Make a visualisation ----

# Effect of Length_mm with the other three variables held at their means
# (full model).
new_data <- expand.grid(
  Length_mm = seq(min(abalone_data$Length_mm), max(abalone_data$Length_mm),
                  length.out = 100),
  Diameter_mm = mean(abalone_data$Diameter_mm),
  Height_mm = mean(abalone_data$Height_mm),
  Whole_weight_g = mean(abalone_data$Whole_weight_g)
)
new_data <- new_data |>
  mutate(Predicted_Rings = predict(m_rings_all, newdata = new_data,
                                   type = "response"))
ggplot(new_data, aes(x = Length_mm, y = Predicted_Rings)) +
  geom_line() +
  labs(title = "Predicted Rings vs Length_mm\n(Full Model)",
       x = "Length (arbitrary scale)", y = "Predicted rings")
# Look at: predicted Rings DECREASE with length, which is biologically
# unlikely. Holding diameter, height and weight constant while changing length
# describes abalone that hardly exist (all four are strongly correlated).
# This is a consequence of collinearity.

# The same for the reduced model (Whole_weight_g held at its mean).
new_data2 <- expand.grid(
  Length_mm = seq(min(abalone_data$Length_mm), max(abalone_data$Length_mm),
                  length.out = 100),
  Whole_weight_g = mean(abalone_data$Whole_weight_g)
)
new_data2 <- new_data2 |>
  mutate(Predicted_Rings = predict(m_rings_length_weight, newdata = new_data2,
                                   type = "response"))
ggplot(new_data2, aes(x = Length_mm, y = Predicted_Rings)) +
  geom_line() +
  labs(title = "Predicted Rings vs Length_mm\n(Reduced Model)",
       x = "Length (arbitrary scale)", y = "Predicted rings")
# Look at: now predicted Rings increase with length. Same data, opposite
# conclusion: this is why collinearity is dangerous for interpretation.


# Part 2, step 7: Write reporting sentences ----

# Model answer: "In a Poisson GLM of the number of shell rings (n = 400
# abalone), the four non-lethal size measurements together explained about 39%
# of the deviance, and predicted ring numbers without systematic bias
# (regression of observed on predicted values: intercept and slope not
# different from 0 and 1). Diameter and height had significant partial
# associations with ring number, but because all four size measurements were
# very strongly correlated (high VIFs), the effects of the individual
# measurements cannot be reliably separated."


# Part 2, step 8: Critique and reflection ----

# Model answer: we learned more about collinearity than about count data.
# Collinearity is a property of the explanatory variables, so it affects
# linear models and GLMs in the same way. If the aim is prediction, the
# combined model may still be useful; if the aim is to understand which
# measurement matters, these data cannot tell us. The QQ-plot and the
# underdispersion also show that the Poisson distribution does not describe
# these data perfectly.


# =============================================================================
# Practical part 3: what does a Poisson GLM slope mean? ----
# =============================================================================

# (Uses abalone_data from Part 2, step 1.)


# Part 3, step 1: Fit the model with only Length_mm ----

m_rings_length <- glm(Rings ~ Length_mm,
                      data = abalone_data,
                      family = poisson)
summary(m_rings_length)
# Answer: the slope for Length_mm is 1.53.

# Does a slope of 1.53 mean that Rings increase by 1.53 for each 1 mm?
# Answer: No. A Poisson GLM uses a log link, so the coefficient is on the log
# scale and describes a MULTIPLICATIVE effect: a 1-unit increase in length
# multiplies the expected number of rings by exp(1.53).
exp(coef(m_rings_length)["Length_mm"])


# Part 3, step 2: Calculate an expected number of rings ----

# 10 rings on average at length 0.5; what at length 1.5 (1 unit more)?
# Use the accurate slope and round only at the end.
10 * exp(1.532968)
# Answer: 46.3 rings (10 x 4.631904). Not 10 + 1.53 = 11.5, and not 15.4.

# The same with the model's own (unrounded) slope:
10 * exp(coef(m_rings_length)[["Length_mm"]])


# Check that the script is reproducible ----

# Restart R (Session > Restart R) and run the whole script again from the
# top (Code > Run Region > Run All). If it runs without errors, your analysis
# is reproducible.
