# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 9 practical: example solution
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
# Theory: course book Chapter 9 (binomial and binary data, GLM checklist),
# Chapter 6 (collinearity) and Chapter 12 (deciding which terms go into a
# model).
#
# Parts 2 and 3 are open tasks. There are many good solutions. Here we choose
# the explanatory variables BEFORE fitting, from our hypotheses, and fit only
# that one planned model (no trying of many models, no stepwise selection).
# =============================================================================


# Load the packages ----

library(tidyverse)  # read_csv(), ggplot2, dplyr, and more
library(ggfortify)  # autoplot() for model diagnostic plots
library(GGally)     # ggpairs(): all pairwise scatterplots
library(car)        # vif(): variance inflation factors (collinearity)


# =============================================================================
# Practical part 1: insecticide dose-response (aggregated binomial data) ----
# =============================================================================

# Question: how does the probability of death increase with dose, and does
# the dose-response relationship differ among the three products?


# Part 1, step 1: Load the data ----

pesticide <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/pesticide_mortality.csv")
pesticide <- pesticide |>
  mutate(proportion_dead = dead / n)

glimpse(pesticide)
nrow(pesticide)          # 63 groups
table(pesticide$product)

# Total number of insects tested.
sum(pesticide$n)
# Answer: 700 insects in 63 groups. Each row is one binomial observation:
# a number of deaths out of a number of insects.

# Proportion dead against log dose, one colour per product.
ggplot(pesticide, aes(x = logdose, y = proportion_dead, colour = product)) +
  geom_point(aes(size = n), alpha = 0.7) +
  labs(x = "Log dose (0 = standard field dose)", y = "Proportion dead",
       colour = "Product", size = "Insects in group")
# Look at: S-shaped increase with dose for all products; product B seems to
# rise less steeply.


# Part 1, step 2: Why not a linear model? ----

m_pest_lm <- lm(proportion_dead ~ logdose, data = pesticide)
range(fitted(m_pest_lm))
# Answer: the fitted proportions go from about -0.07 to 1.13. A linear model
# is unsuitable because (1) it predicts impossible proportions below 0 and
# above 1; (2) the variance of a proportion depends on its mean (largest near
# 0.5), so the variance is not constant; (3) it ignores the group sizes: a
# proportion from 14 insects is more precise than one from 8.
# (The number of rows is NOT a problem for a linear model.)


# Part 1, step 3: Fit a binomial GLM ----

# Expected residual degrees of freedom: 63 groups minus 6 parameters
# (intercept and slope for product A, plus a difference in intercept and a
# difference in slope for each of products B and C).
63 - 6
# Answer: 57. (Degrees of freedom are based on the 63 groups, not the 700
# insects.)

# The response is cbind(successes, failures) = cbind(dead, alive).
m_pest_interaction <- glm(cbind(dead, n - dead) ~ logdose * product,
                          family = binomial, data = pesticide)
summary(m_pest_interaction)


# Part 1, step 4: Check the model ----

# Diagnostic plots (a rough guide only for GLMs; Chapter 9).
autoplot(m_pest_interaction, which = 1:4, add.smooth = TRUE)
# Look at: no strong pattern in the residuals, no point with a very large
# Cook's distance, QQ-plot reasonably close to the line.

# Dispersion: residual deviance / residual degrees of freedom. This check is
# informative for AGGREGATED binomial data like these.
deviance(m_pest_interaction) / df.residual(m_pest_interaction)
# Answer: 55.65 / 57 = 0.98, close to 1: no evidence of overdispersion, so
# the binomial model is fine (no need for quasibinomial).


# Part 1, step 5: Does the dose-response relationship differ among products? ----

m_pest_additive <- glm(cbind(dead, n - dead) ~ logdose + product,
                       family = binomial, data = pesticide)
anova(m_pest_additive, m_pest_interaction, test = "Chisq")
# Answer: the deviance falls by 9.19 on 2 degrees of freedom (the interaction
# adds two slope differences), p = 0.01. There is evidence that the slope of
# the dose-response relationship differs among products.


# Part 1, step 6: Interpret the coefficients ----

coef(m_pest_interaction)

# Product A is the reference level, so "logdose" is the slope for product A
# on the log-odds scale. exp() gives the odds ratio per unit of log dose.
exp(coef(m_pest_interaction)["logdose"])
exp(confint.default(m_pest_interaction))["logdose", ]
# Answer: 2.68 (95% CI about 2.1 to 3.4). Each one-unit increase in log dose
# multiplies the ODDS of death by about 2.7 for product A.

# Slope for product B = slope for A + difference in slope for B.
coef(m_pest_interaction)[["logdose"]] + coef(m_pest_interaction)[["logdose:productB"]]
# Answer: 0.63 (0.986 - 0.352). For product B each unit of log dose
# multiplies the odds of death by only about 1.9:
exp(coef(m_pest_interaction)[["logdose"]] + coef(m_pest_interaction)[["logdose:productB"]])


# Part 1, step 7: Predicted probabilities ----

# Calculate on the link (log-odds) scale, then back-transform with plogis(),
# so that the confidence interval stays between 0 and 1.
new_doses <- data.frame(logdose = 0, product = c("A", "B", "C"))
pred_link <- predict(m_pest_interaction, newdata = new_doses, type = "link",
                     se.fit = TRUE)
new_doses <- new_doses |>
  mutate(prob = plogis(pred_link$fit),
         lower = plogis(pred_link$fit - 1.96 * pred_link$se.fit),
         upper = plogis(pred_link$fit + 1.96 * pred_link$se.fit))
new_doses
# Answer: product B at the field dose: 0.56 (95% CI about 0.48 to 0.65).
# A: 0.53 and C: 0.59. At the field dose the products kill similar
# proportions; they differ in how steeply mortality rises with dose.

# Figure: data with fitted curves and 95% CIs for each product.
new_curves <- expand_grid(
  logdose = seq(min(pesticide$logdose), max(pesticide$logdose), length.out = 100),
  product = c("A", "B", "C")
)
pred_curves <- predict(m_pest_interaction, newdata = new_curves, type = "link",
                       se.fit = TRUE)
new_curves <- new_curves |>
  mutate(proportion_dead = plogis(pred_curves$fit),
         lower = plogis(pred_curves$fit - 1.96 * pred_curves$se.fit),
         upper = plogis(pred_curves$fit + 1.96 * pred_curves$se.fit))

ggplot(pesticide, aes(x = logdose, y = proportion_dead, colour = product)) +
  geom_ribbon(data = new_curves, aes(ymin = lower, ymax = upper, fill = product),
              alpha = 0.2, colour = NA) +
  geom_line(data = new_curves, linewidth = 1) +
  geom_point(alpha = 0.7) +
  labs(x = "Log dose (0 = standard field dose)", y = "Proportion of insects dead",
       colour = "Product", fill = "Product") +
  theme_bw()


# Part 1, step 8: Report ----

# Answer (best reporting sentence): "Mortality increased with dose for all
# three products, but less steeply for product B: each unit increase in log
# dose multiplied the odds of death by 2.7 for product A (95% CI 2.1-3.4) but
# only by 1.9 for product B (binomial GLM; dose x product interaction,
# chi-squared = 9.2, df = 2, p = 0.01). At the field dose, about 53-59% of
# insects died with each product."
# WHY: it gives interpretable effect sizes (odds ratios, probabilities) with
# uncertainty and the test. A log-odds coefficient is NOT a change in
# probability.


# Part 1, step 9: Separation ----

# Answer: if every insect died above dose 0 and none below, dose perfectly
# separates deaths from survivals (complete separation). The best-fitting
# slope is then infinitely steep, so R reports a huge estimate (38.2) with an
# enormous standard error (2950) and warns "fitted probabilities numerically
# 0 or 1 occurred". The p-value is meaningless: the effect of dose is in fact
# very strong. (It is not overdispersion.)


# =============================================================================
# Practical part 2: White-tailed Ptarmigan presence/absence (binary data) ----
# =============================================================================

# Question: which environmental variables explain where White-tailed
# Ptarmigan are present (pres = 1) or absent (pres = 0)?


# Part 2, step 1: Load the data and make some basic checks ----

ptarm <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/presabs_both_IndYears_allvars_final.csv")
glimpse(ptarm)

nrow(ptarm)
table(ptarm$pres)          # 344 presences, 688 absences
colSums(is.na(ptarm))      # only "datatype" has NAs (see below)
table(ptarm$datatype, ptarm$pres, useNA = "ifany")
# The absences are "pseudo-absences" (points chosen by the researchers, not
# surveyed sites), so they have no datatype. We do not use datatype.


# Part 2, step 2: List the environmental factors ----

# From the README.txt on the Dryad page:
# - BEC: biogeoclimatic zone: AT = Alpine Tundra, MH = Mountain Hemlock,
#   CWH = Coastal Western Hemlock (categorical)
# - Elevation: elevation (m)
# - Aspect: aspect reclassified by solar incidence: low values = cooler
#   (north-facing), high values = warmer (south-facing)
# - Slope: slope (degrees)
# - CTI: compound topographic index (wetness, from slope and upstream area)
# - Rugg9: terrain ruggedness (vector ruggedness measure)
# - Tave_sm: mean summer temperature (degrees C)
# - MSP: mean summer precipitation, May to September (mm)
# - PAS: precipitation as snow (mm)
# FNETID, Year, datatype and the coordinates BCAlbX, BCAlbY are not
# environmental factors.


# Part 2, step 3: Hypotheses (written BEFORE looking at the data) ----

# White-tailed Ptarmigan live all year in alpine habitat above the treeline:
# open, rocky, cold places with snow, where they feed on low shrubs and use
# snow for winter roosting. Our hypotheses:
# - Elevation: positive (alpine habitat is high).
# - Tave_sm: negative (they are adapted to cold, avoid warm summers).
# - PAS: positive (snow-rich places).
# - Aspect: negative (cooler, north-facing slopes keep snow and are cooler).
# - Rugg9: positive (rocky, rugged terrain gives cover).
# - Slope: negative (very steep slopes have less vegetation to feed on).
# - BEC: presence highest in Alpine Tundra, lower in the forest zones.
# - MSP and CTI: no clear expectation.


# Part 2, step 4: Graphs of presence/absence against each factor ----

# Proportion of points with ptarmigan in each biogeoclimatic zone.
ptarm |>
  group_by(BEC) |>
  summarise(n = n(), presences = sum(pres), proportion_present = mean(pres))
# Look at: almost all presences are in Alpine Tundra (315 of 437 points);
# only 1 of 17 points in CWH has a presence.

# Presence (0/1) against each continuous variable. The points are jittered a
# little vertically so that we can see them; the curve is a simple logistic
# regression of presence on that one variable.
env_vars <- c("Elevation", "Aspect", "Slope", "CTI", "Rugg9", "Tave_sm",
              "MSP", "PAS")
ptarm_long <- ptarm |>
  select(pres, all_of(env_vars)) |>
  pivot_longer(-pres, names_to = "variable", values_to = "value")

ggplot(ptarm_long, aes(x = value, y = pres)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = binomial)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 4) +
  labs(x = "Value of the environmental variable", y = "Presence (1) / absence (0)")
# Look at: presence increases strongly with Elevation and PAS, and decreases
# with Tave_sm and with Aspect. These fit our hypotheses. The other variables
# show weaker patterns.


# Part 2, step 5: Correlations among the environmental factors ----

ptarm |>
  select(all_of(env_vars)) |>
  cor() |>
  round(2)
ggpairs(select(ptarm, all_of(env_vars)))
# Look at: Elevation and Tave_sm are very strongly correlated (r = -0.89):
# high places are cold. PAS is correlated with Tave_sm (-0.61), MSP (0.64)
# and Elevation (0.49).
# Why this matters: correlated variables share explanatory power. If both
# Elevation and Tave_sm are in the model, the model cannot tell which one
# "explains" presence, so their separate coefficients become unstable and
# have large standard errors (collinearity, Chapter 6). Their shared effect
# is not lost, but it cannot be given to one variable.


# Part 2, step 6: Plan and fit the model ----

# Decisions made BEFORE fitting (from the hypotheses and from the
# correlations among the explanatory variables, not from their relationships
# with presence):
# - Elevation and Tave_sm measure nearly the same gradient (r = -0.89). We
#   include only Elevation (it is measured directly; Tave_sm is modelled from
#   it). Its coefficient then stands for the whole "high and cold" gradient.
# - BEC zones are defined largely by elevation and climate, and CWH has only
#   1 presence (risk of separation). So we do not include BEC.
# - We include PAS, Aspect, Rugg9 and Slope, for which we have hypotheses.
#   MSP and CTI are left out (no clear hypothesis).
# There are 344 presences, so with 6 parameters we have far more than 10-20
# observations of the rarer outcome per parameter (Chapter 12).

# Rescale elevation and snow to units of 100, so the coefficients are easier
# to read ("per 100 m", "per 100 mm").
ptarm <- ptarm |>
  mutate(elevation_100m = Elevation / 100,
         pas_100mm = PAS / 100)

m_ptarm <- glm(pres ~ elevation_100m + pas_100mm + Aspect + Rugg9 + Slope,
               family = binomial, data = ptarm)
summary(m_ptarm)


# Part 2, step 7: Check the model (GLM checklist) ----

autoplot(m_ptarm, which = 1:4, add.smooth = TRUE)
# Look at: with 0/1 data the residual plots show two bands of points (one for
# 0s and one for 1s). This is expected and not a problem. The QQ-plot is not
# useful for binary data. Look instead for points with a much larger Cook's
# distance than the others: there are none that dominate.

# Collinearity among the variables in the model.
vif(m_ptarm)
# Look at: all VIFs are close to 1, so no collinearity problem (because we
# left out Tave_sm).

# Dispersion: with individual 0/1 data the ratio residual deviance / df is
# NOT informative, so we do not use it to judge overdispersion.

# Separation: no very large coefficients or standard errors, so no sign of
# separation.

# Independence: points close together in space (and repeated years) may not
# be independent, and the absences are pseudo-absences. Keep this in mind
# when reading the p-values.


# Part 2, step 8: Test the hypotheses and interpret the model ----

# Likelihood ratio (chi-squared) tests of each variable: Anova() (car
# package, capital A) compares the planned model with the model without that
# variable, for each variable in turn. This tests each hypothesis; it is not
# model selection: we keep the planned model whatever the results.
Anova(m_ptarm)

# Odds ratios with 95% CIs.
exp(cbind(odds_ratio = coef(m_ptarm), confint.default(m_ptarm)))
# Look at (and compare with the hypotheses):
# - Elevation: odds ratio about 2.6 per 100 m (strongly positive, supported).
# - PAS: about 1.08 per 100 mm of snow (positive, supported).
# - Aspect: about 0.28 per unit of aspect index (negative: fewer ptarmigan
#   on warm, south-facing slopes; supported).
# - Slope: weakly negative, with a CI that includes 1 (not clearly supported).
# - Rugg9: CI very wide and includes 1 (no evidence, after accounting for
#   elevation; ruggedness is correlated with elevation, r = 0.42).
#   (Rugg9 only ranges from 0 to 0.22, so its odds ratio "per 1 unit" is for
#   an impossible change; the main point is that there is no clear effect.)

# How much does the model explain? Compare with the null model.
m_ptarm_null <- glm(pres ~ 1, family = binomial, data = ptarm)
anova(m_ptarm_null, m_ptarm, test = "Chisq")
# Look at: the deviance falls from 1313.8 to 609.9 (5 df): the environmental
# variables together explain a lot of the variation in presence/absence.


# Part 2, step 9: Show the modelled relationships ----

# Predicted probability of presence against elevation, with all other
# variables held at their means. Link scale first, then plogis().
new_ptarm <- tibble(
  elevation_100m = seq(min(ptarm$elevation_100m), max(ptarm$elevation_100m),
                       length.out = 100),
  pas_100mm = mean(ptarm$pas_100mm),
  Aspect = mean(ptarm$Aspect),
  Rugg9 = mean(ptarm$Rugg9),
  Slope = mean(ptarm$Slope)
)
pred_ptarm <- predict(m_ptarm, newdata = new_ptarm, type = "link", se.fit = TRUE)
new_ptarm <- new_ptarm |>
  mutate(Elevation = elevation_100m * 100,
         pres = plogis(pred_ptarm$fit),
         lower = plogis(pred_ptarm$fit - 1.96 * pred_ptarm$se.fit),
         upper = plogis(pred_ptarm$fit + 1.96 * pred_ptarm$se.fit))

ggplot(ptarm, aes(x = Elevation, y = pres)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.2) +
  geom_ribbon(data = new_ptarm, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = new_ptarm, linewidth = 1) +
  labs(x = "Elevation (m)",
       y = "Probability of White-tailed Ptarmigan presence",
       caption = "Line: binomial GLM prediction (95% CI), other variables at their means") +
  theme_bw()

# The same for aspect, with the other variables at their means.
new_aspect <- tibble(
  elevation_100m = mean(ptarm$elevation_100m),
  pas_100mm = mean(ptarm$pas_100mm),
  Aspect = seq(-1, 1, length.out = 100),
  Rugg9 = mean(ptarm$Rugg9),
  Slope = mean(ptarm$Slope)
)
pred_aspect <- predict(m_ptarm, newdata = new_aspect, type = "link", se.fit = TRUE)
new_aspect <- new_aspect |>
  mutate(pres = plogis(pred_aspect$fit),
         lower = plogis(pred_aspect$fit - 1.96 * pred_aspect$se.fit),
         upper = plogis(pred_aspect$fit + 1.96 * pred_aspect$se.fit))

ggplot(new_aspect, aes(x = Aspect, y = pres)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(linewidth = 1) +
  labs(x = "Aspect index (low = cool, north-facing; high = warm, south-facing)",
       y = "Probability of presence") +
  theme_bw()


# Part 2, step 10: Write a sentence or two ----

# Model answer: "White-tailed Ptarmigan were much more likely to be present at
# higher elevations: each additional 100 m multiplied the odds of presence by
# about 2.6 (95% CI 2.3-2.9; binomial GLM with elevation, snow, aspect,
# ruggedness and slope). Presence was also more likely where more
# precipitation fell as snow and on cooler, north-facing slopes. Because
# elevation and summer temperature are almost perfectly correlated, the
# elevation effect is best seen as the effect of the whole 'high and cold'
# alpine gradient."

# Critique and reflection (model answer): the absences are pseudo-absences,
# not confirmed absences; nearby points and repeated years may not be
# independent (spatial autocorrelation), so the p-values are too small; the
# model assumes straight-line effects on the log-odds scale; and we cannot
# separate elevation from temperature with these data.


# =============================================================================
# Practical part 3: predicting death after a heart attack (echocardiogram) ----
# =============================================================================

# Aim: predict death within one year after a heart attack from echocardiogram
# features. The practical tells us to use alive_at_1 as the response.


# Part 3, step 1: Load the data and add the header row ----

# The file has no header row, and "?" means a missing value.
echo_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/echocardiogram.data",
                      col_names = FALSE, na = c("?", ""))
# R warns about a parsing problem: one row (row 50) has 14 values instead of
# 13. Its values are shifted by one column, so its alive_at_1 is "2,?". We
# remove it below.
problems(echo_data)

# Add the variable names, in the order given in the names file.
names(echo_data) <- c("survival", "still_alive", "age_at_heart_attack",
                      "pericardial_effusion", "fractional_shortening", "epss",
                      "lvdd", "wall_motion_score", "wall_motion_index", "mult",
                      "name", "group", "alive_at_1")
glimpse(echo_data)

# Basic checks.
nrow(echo_data)
colSums(is.na(echo_data))
table(echo_data$alive_at_1, useNA = "ifany")
# Look at: alive_at_1 is missing for 57 patients, and one value is "2,?"
# (the broken row).

# Keep only patients with a valid response, make the variables numeric, and
# make the variable we want to predict: death within one year (1 = died).
echo_data <- echo_data |>
  filter(alive_at_1 %in% c("0", "1")) |>
  mutate(alive_at_1 = as.numeric(alive_at_1),
         survival = as.numeric(survival),
         death_within_1 = 1 - alive_at_1)
nrow(echo_data)
table(echo_data$death_within_1)
# 74 patients: 50 with death_within_1 = 1 and 24 with 0.

# Check the response against how it should have been derived. The names file
# says alive_at_1 = 1 means "alive at 1 year". So patients with survival of
# 12 months or more should have alive_at_1 = 1.
echo_data |>
  count(alive_at_1, still_alive, survival_12_months_plus = survival >= 12)
# IMPORTANT: this does not fit the documentation! The patients with
# alive_at_1 = 1 were all still alive but had been followed for LESS than 12
# months (23 of 24; one has no survival time), and 43 of the 50 patients
# with alive_at_1 = 0 died AFTER 12 months or more. So in this
# file, alive_at_1 does not mean what the names file says. Always check
# derived variables like this! We continue with alive_at_1 because the
# practical asks us to, but we must interpret the results with great care.


# Part 3, step 2: List the echocardiogram features ----

# From the names file:
# - age_at_heart_attack: age (years) at the heart attack
# - pericardial_effusion: fluid around the heart (0 = no, 1 = yes)
# - fractional_shortening: contractility; lower = more abnormal
# - epss: E-point septal separation (contractility); larger = more abnormal
# - lvdd: left ventricle size at end-diastole; large hearts tend to be sick
# - wall_motion_score / wall_motion_index: how the segments of the left
#   ventricle move (higher = more abnormal). Use the index, not the score.
# Not features: survival and still_alive (they contain the outcome), mult
# (derived, ignore), name and group (meaningless).


# Part 3, step 3: Hypotheses ----

# A weaker, damaged heart should mean a higher risk of death:
# - age_at_heart_attack: positive (older patients die more often)
# - pericardial_effusion: positive
# - fractional_shortening: negative (less contraction = weaker heart)
# - epss: positive
# - lvdd: positive
# - wall_motion_index: positive (abnormal wall movement after the attack)


# Part 3, step 4: Graphs of death against each feature ----

echo_vars <- c("age_at_heart_attack", "pericardial_effusion",
               "fractional_shortening", "epss", "lvdd", "wall_motion_index")
echo_long <- echo_data |>
  select(death_within_1, all_of(echo_vars)) |>
  pivot_longer(-death_within_1, names_to = "variable", values_to = "value") |>
  drop_na()

ggplot(echo_long, aes(x = value, y = death_within_1)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial)) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Value of the feature", y = "Death within 1 year (1) or not (0)")
# Look at: several relationships go in the OPPOSITE direction to our
# hypotheses (e.g. "death" is less likely in older patients and with higher
# wall-motion index). This is a strong clue that the response does not
# measure what we think (see the check in step 1).


# Part 3, step 5: Correlations among features ----

echo_data |>
  select(age_at_heart_attack, fractional_shortening, epss, lvdd,
         wall_motion_index) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)
# Look at: the contractility and heart-size measures are moderately
# correlated (epss with lvdd 0.56, with wall_motion_index 0.48, with
# fractional_shortening -0.41). Age is hardly correlated with the others.


# Part 3, step 6: Plan and fit the model ----

# How many parameters can we afford? The rarer outcome occurs 24 times. With
# the rough guide of 10-20 observations of the rarer outcome per parameter
# (Chapter 12), we can estimate only about 1-2 slopes.
# So, BEFORE fitting, we choose two features with the strongest prior
# reasons: wall_motion_index (recommended in the names file, and a direct
# measure of damage to the heart wall) and age_at_heart_attack (a well-known
# risk factor, and nearly uncorrelated with wall_motion_index). We fit only
# this model. (No stepwise selection: it gives misleading p-values.)

echo_model_data <- echo_data |>
  select(death_within_1, age_at_heart_attack, wall_motion_index) |>
  drop_na()
nrow(echo_model_data)   # 72 complete cases

m_echo <- glm(death_within_1 ~ age_at_heart_attack + wall_motion_index,
              family = binomial, data = echo_model_data)
summary(m_echo)


# Part 3, step 7: Check the model (GLM checklist) ----

autoplot(m_echo, which = 1:4, add.smooth = TRUE)
# Look at: two bands of residuals, as expected for 0/1 data. Check Cook's
# distance for single patients with a very large influence.
# Separation: the estimates and standard errors are not huge, so no sign of
# separation. The dispersion ratio is not informative for 0/1 data.


# Part 3, step 8: Interpret the model ----

exp(cbind(odds_ratio = coef(m_echo), confint.default(m_echo)))
Anova(m_echo)
# Look at: both odds ratios are BELOW 1: older patients and patients with a
# higher wall-motion index are predicted to be LESS likely to "die within one
# year". This is the opposite of our hypotheses. Given the check in step 1,
# the most likely explanation is that alive_at_1 in this file does not mean
# "alive one year after the heart attack" (it looks more like "followed up
# for less than a year"), not that a damaged heart protects patients.

# How well does the model predict (on the same data used to fit it)?
echo_model_data <- echo_model_data |>
  mutate(predicted_prob = predict(m_echo, type = "response"))
table(predicted_death = echo_model_data$predicted_prob > 0.5,
      observed_death = echo_model_data$death_within_1)
mean((echo_model_data$predicted_prob > 0.5) == echo_model_data$death_within_1)
mean(echo_model_data$death_within_1)
# Look at: about 79% of patients are classified correctly, but simply
# predicting "death" for everybody would already be right for about 69%.
# And this is measured on the data used to fit the model, so it is too
# optimistic (see "Final thoughts on prediction" in the practical).


# Part 3, step 9: Show model predictions ----

# Predicted probability against wall-motion index, with age at its mean.
new_echo <- tibble(
  wall_motion_index = seq(min(echo_model_data$wall_motion_index),
                          max(echo_model_data$wall_motion_index),
                          length.out = 100),
  age_at_heart_attack = mean(echo_model_data$age_at_heart_attack)
)
pred_echo <- predict(m_echo, newdata = new_echo, type = "link", se.fit = TRUE)
new_echo <- new_echo |>
  mutate(death_within_1 = plogis(pred_echo$fit),
         lower = plogis(pred_echo$fit - 1.96 * pred_echo$se.fit),
         upper = plogis(pred_echo$fit + 1.96 * pred_echo$se.fit))

ggplot(echo_model_data, aes(x = wall_motion_index, y = death_within_1)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_ribbon(data = new_echo, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = new_echo, linewidth = 1) +
  labs(x = "Wall-motion index",
       y = "Predicted probability of 'death within 1 year'",
       caption = "Age at heart attack held at its mean") +
  theme_bw()


# Part 3, step 10: Write a sentence or two ----

# Model answer: "In 72 heart-attack patients, a binomial GLM with age and
# wall-motion index (chosen in advance) predicted the recorded one-year
# outcome only modestly better than chance (79% correct on the training data,
# compared with 69% by always predicting the commoner outcome). The estimated
# effects were opposite to physiological expectation, and the outcome variable
# in the data file does not agree with its documented definition, so these
# results should not be used to draw conclusions about risk of death."

# Critique and reflection (model answer): very small dataset with missing
# values; the response variable seems to be coded differently from its
# description; prediction accuracy should be tested on new data (a test set
# or cross-validation); complete-case analysis may bias the results. The most
# important lesson: check that your data mean what you think they mean
# before you model them.


# Check that the script is reproducible ----

# Restart R (Session > Restart R) and run the whole script again from the
# top (Code > Run Region > Run All). If it runs without errors, your analysis
# is reproducible.
