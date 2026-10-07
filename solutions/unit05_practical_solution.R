# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 5 practical: example solution
# Course book chapter: Analysis of variance (ANOVA)
# https://opetchey.github.io/BIO144_Course_Book/5.1-ANOVA.html
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
# =============================================================================

# This script analyses the effect of four treatments on the change in
# systolic blood pressure (simulated clinical study), with a one-way ANOVA.


# Load the packages ----
# multcomp is loaded BEFORE tidyverse: multcomp also loads the MASS package,
# whose select() function would otherwise hide dplyr's select().
library(multcomp)   # Tukey HSD post-hoc tests with glht()
library(tidyverse)  # readr, dplyr, forcats, ggplot2 and more
library(ggfortify)  # autoplot() for model checking plots


# Step 1: Get organised: read the data and check it ----
systbp_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/systbp_drug_effect.csv")
systbp_data
glimpse(systbp_data)

# Number of rows:
nrow(systbp_data)
# Answer: 100 rows.

# Is each row a different participant? Count the unique IDs.
length(unique(systbp_data$id))
# Check: 100 unique IDs, so yes, one row per participant. (Do not assume this:
# check it every time.)

# Quality control: is sbp_change really followup_sbp - baseline_sbp?
systbp_data |>
  mutate(difference = sbp_change - (followup_sbp - baseline_sbp)) |>
  summarise(max_abs_difference = max(abs(difference), na.rm = TRUE))
# Check: the largest difference is 0.1 mmHg, which is only rounding (the
# values have one decimal place). Good.

# How many observations of the response (sbp_change) do we have?
length(na.omit(systbp_data$sbp_change))
# Answer: 98. Two participants have no follow-up measurement, so their
# sbp_change is NA.

# Make a version of the dataset without missing values of the response.
# (We keep the name systbp_data, as the practical does.)
systbp_data <- systbp_data |>
  filter(!is.na(sbp_change))
nrow(systbp_data)

# How many treatment groups, and how many participants in each?
unique(systbp_data$treatment)
systbp_data |>
  count(treatment)
# Answer: 4 treatment groups (24 or 25 participants each).

# Expected degrees of freedom:
# treatment df = number of groups - 1 = 4 - 1 = 3
# error (residual) df = number of observations - number of groups
#                     = 98 - 4 = 94  (or: (98 - 1) - 3 = 94)
# Answer: 94 error degrees of freedom.


# Step 2: Visualise ----
ggplot(systbp_data, aes(x = treatment, y = sbp_change)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Change in Systolic Blood Pressure by Treatment Group",
       x = "Treatment Group",
       y = "Change in Systolic Blood Pressure (mmHg)")
# Answer (good type of graph): a box-and-whisker plot for each treatment
# group (with the data points on top). It shows the whole distribution in
# each group, not only the mean.

# R puts the groups in alphabetical order. A more logical order puts
# StandardCare first, as the reference (comparison) group. fct_relevel() is
# in the forcats package, which is part of the tidyverse. It also turns
# treatment into a factor.
systbp_data <- systbp_data |>
  mutate(treatment = fct_relevel(treatment,
                                 "StandardCare", "Placebo", "DrugA_Low", "DrugA_High"))
levels(systbp_data$treatment)

ggplot(systbp_data, aes(x = treatment, y = sbp_change)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Change in Systolic Blood Pressure by Treatment Group",
       x = "Treatment Group",
       y = "Change in Systolic Blood Pressure (mmHg)")
# Answer (interpretation): there is variation in blood pressure change within
# each group, AND at least one group seems to have a different average change
# (Placebo: little change; DrugA_High: the largest decrease). The graph does
# not prove anything about causes or exact equality.
# Answer (assumptions from the graph): we can see if the spread is similar in
# all groups, and if each group looks roughly symmetric without extreme
# outliers. (Note one high value in DrugA_High, about +13 mmHg.)
# Answer (expected p-value): quite a lot less than 0.05, because the
# differences between the group means are large compared with the
# variation within groups.

# Group means and standard deviations, to compare with the model later:
systbp_data |>
  group_by(treatment) |>
  summarise(mean_change = mean(sbp_change),
            sd_change = sd(sbp_change),
            n = n())


# Step 3: Fit a one-way ANOVA model ----
# A one-way ANOVA is a linear model with one categorical explanatory variable.
m_sbp_treatment <- lm(sbp_change ~ treatment, data = systbp_data)


# Step 4: Check assumptions ----
autoplot(m_sbp_treatment)
# Look at:
#  - Residuals vs Fitted: four vertical stripes (one per group), centred on
#    zero. No pattern (with groups there can be no curve).
#  - Normal Q-Q: the points are close to the line, except one high point.
#  - Scale-Location: the spread is roughly similar in all groups.
#  - Residuals vs Leverage: all points have the same, low leverage (equal
#    group sizes). One point (row 50) has a large residual.
# Answer (agreed statements): "The normal qq plot does not show major
# deviations from normality of the residuals" and "The scale-location plot
# shows that the variance of the residuals is roughly constant".
# The assumptions seem reasonable.

# A closer look at the points with large residuals (more in the Critique):
systbp_data |>
  mutate(std_residual = rstandard(m_sbp_treatment),
         cooks_d = cooks.distance(m_sbp_treatment)) |>
  arrange(desc(abs(std_residual))) |>
  head(3)
# Look at: participant 51 (DrugA_High) had a 12.9 mmHg INCREASE, while most
# others in that group had a decrease. Its standardised residual is 3.9, but
# its Cook's distance is only 0.16 (far below 0.5 or 1).


# Step 5: Interpret the ANOVA table ----
anova(m_sbp_treatment)
# Look at: treatment Df = 3, Residuals Df = 94, F = 10.59, p < 0.0001.
# Answer (df as expected?): Yes: 3 and 94, as we calculated in step 1.
# Answer (interpretation): the F-test tests whether all treatment group means
# are equal, against the alternative that at least one differs. It does not
# tell us which groups differ. Because participants were RANDOMLY assigned to
# treatments, a significant result is evidence that the treatments caused
# the differences. (The causal interpretation comes from the study design,
# not from the ANOVA.)


# Step 6: Make a great visualisation ----
# One good option: all the data points, plus the estimated group means with
# their 95% confidence intervals from the model.
treatment_means <- tibble(treatment = levels(systbp_data$treatment)) |>
  mutate(treatment = fct_relevel(treatment, levels(systbp_data$treatment)))
treatment_preds <- predict(m_sbp_treatment, newdata = treatment_means,
                           interval = "confidence")
treatment_means <- treatment_means |>
  mutate(fit = treatment_preds[, "fit"],
         lwr = treatment_preds[, "lwr"],
         upr = treatment_preds[, "upr"])
treatment_means

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_jitter(data = systbp_data, aes(x = treatment, y = sbp_change),
              width = 0.1, alpha = 0.4, size = 2) +
  geom_pointrange(data = treatment_means,
                  aes(x = treatment, y = fit, ymin = lwr, ymax = upr),
                  colour = "red", size = 0.8) +
  scale_x_discrete(labels = c("Standard care", "Placebo",
                              "Drug A (low dose)", "Drug A (high dose)")) +
  labs(x = "Treatment",
       y = "Change in systolic blood pressure\nafter 8 weeks (mmHg)") +
  theme_bw(base_size = 14)
# Grey points: participants. Red: estimated mean change with 95% confidence
# interval. Values below the dashed line are reductions in blood pressure.


# Step 7: Reporting sentence ----
# StandardCare is the reference level, so the coefficients in summary() are
# differences from StandardCare.
summary(m_sbp_treatment)
confint(m_sbp_treatment)
# Look at:
#  - (Intercept) = -7.66: mean change in the StandardCare group.
#  - treatmentDrugA_High = -5.48: DrugA_High reduced blood pressure 5.48 mmHg
#    MORE than StandardCare (95% CI -9.35 to -1.61).
# Answer (effect of DrugA_High relative to StandardCare): -5.48 mmHg.
#  - Multiple R-squared = 0.25: treatment group explains about a quarter of
#    the variation in blood pressure change. Most variation among
#    participants is not explained by treatment.

# Model reporting sentence:
# Mean change in systolic blood pressure after 8 weeks differed among the
# four treatment groups (one-way ANOVA, F(3, 94) = 10.6, p < 0.0001,
# R squared = 0.25). Blood pressure fell by 7.7 mmHg on average with standard
# care, and by a further 5.5 mmHg with the high dose of Drug A (95% CI 1.6 to
# 9.3 mmHg larger reduction than standard care). The low dose of Drug A gave
# a similar reduction to standard care (difference -0.9 mmHg, 95% CI -4.8 to
# 3.0), and the placebo group had a 5.3 mmHg smaller reduction than standard
# care (95% CI 1.5 to 9.2).


# Step 8 (optional): Post-hoc comparisons ----
# Bonferroni correction, using pairwise t-tests:
pairwise.t.test(systbp_data$sbp_change, systbp_data$treatment,
                p.adjust.method = "bonferroni")

# Tukey's Honest Significant Difference (HSD), with multcomp:
tukey_results <- glht(m_sbp_treatment, linfct = mcp(treatment = "Tukey"))
summary(tukey_results)
# Look at the adjusted p-values (column Pr(>|t|)):
#  Placebo - StandardCare       p = 0.036  significant
#  DrugA_Low - StandardCare     p = 0.96   not significant
#  DrugA_High - StandardCare    p = 0.030  significant
#  DrugA_Low - Placebo          p = 0.009  significant
#  DrugA_High - Placebo         p < 0.001  significant
#  DrugA_High - DrugA_Low       p = 0.098  not significant
# Answer: Placebo differs from StandardCare; DrugA_High differs from
# StandardCare; DrugA_Low differs from Placebo; DrugA_High differs from
# Placebo. (The Bonferroni p-values lead to the same conclusions.)

# Confidence intervals of the differences, adjusted for multiple comparisons:
confint(tukey_results)


# Critique ----
# Model answers to the points in the practical:
# 1. Covariates: we did not account for age, sex or baseline blood pressure,
#    which might also affect the change in blood pressure. Random assignment
#    makes them unlikely to bias the comparison, but including them (two-way
#    ANOVA or ANCOVA, Units 6 and 7) could make the estimates more precise.
# 2. Study design: we do not know if Placebo and Drug A groups also received
#    standard care. A better design: (a) standard care only; (b) standard
#    care + placebo pill; (c) standard care + Drug A low dose; (d) standard
#    care + Drug A high dose. Then (b) vs (a) is the placebo effect, and (c)
#    and (d) vs (b) are the effects of the drug itself.
# 3. Points with large residuals: we refit the model without the two
#    participants with |standardised residual| > 2.5 and compare.
systbp_data_no_extremes <- systbp_data |>
  filter(abs(rstandard(m_sbp_treatment)) <= 2.5)
nrow(systbp_data_no_extremes)
# Check: 96 participants (participants 51 and 84 removed).
m_sbp_treatment_no_extremes <- lm(sbp_change ~ treatment,
                                  data = systbp_data_no_extremes)
anova(m_sbp_treatment_no_extremes)
coef(m_sbp_treatment)
coef(m_sbp_treatment_no_extremes)
# Look at: the F value gets larger and the DrugA_High effect gets larger
# (more negative). The conclusions do not change, so the results are not
# driven by these two participants. We keep them in the main analysis: there
# is no evidence that they are errors, and removing data just because they
# do not fit is not good practice. We could report this check.


# Check that the script is reproducible ----
# When you have finished: click Session > Restart R, then run the whole
# script again from the top (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac).
# It should run without errors and give the same answers. If it does not,
# something in your script depends on something you did outside the script.
