# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 12 practical: example solution (capstone case study: chamber warming)
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
# - Theory: course book, Chapter 12 (Review), and Chapters 7 (interactions) and
#   11 (mixed models).
# =============================================================================


# Load the packages ----
library(tidyverse)  # read_csv(), dplyr, forcats, ggplot2
library(lme4)       # lmer(): linear mixed models
library(lmerTest)   # adds degrees of freedom and p-values to lmer() output


# Step 1: Plan before you look at the data ----
# No R in this step: these are the planning answers.

# Question: what are the experimental units for the two treatments?
# Answer: for warming, the chamber (12 units); for nitrogen, the pot (96
# units). WHY: the experimental unit is the unit to which a treatment is
# independently and randomly assigned. Warming was assigned to whole chambers,
# nitrogen to pots within chambers (a split-plot design).

# Question: how many df for testing warming, if we respect the experimental unit?
# Answer: 10. WHY: 12 chambers - 2 parameters at the chamber level (the
# overall mean and the warming effect) = 10. A linear model would wrongly
# report 96 pots - 4 parameters = 92 residual df.

# Question: which is the best planning sentence?
# Answer: "The response (biomass, continuous) will be modelled with a linear
# mixed model with warming, nitrogen and their interaction as fixed effects
# and chamber as a random effect, because pots are grouped in chambers; the
# warming effect will be tested with about 10 df (12 chambers), the nitrogen
# effect and the interaction within chambers."


# Step 2: Import, check and visualise ----
chamber_warming <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/chamber_warming.csv")
# Make "low" the reference level of nitrogen (otherwise R would use "high",
# because it comes first in the alphabet), and make warming a factor.
chamber_warming <- chamber_warming |>
  mutate(nitrogen = fct_relevel(nitrogen, "low"),
         warming = factor(warming))

# Check the data
chamber_warming
nrow(chamber_warming)                    # 96 rows (pots)
n_distinct(chamber_warming$chamber)      # 12 chambers
chamber_warming |> count(warming, nitrogen)        # 24 pots per combination
chamber_warming |> count(chamber, warming, nitrogen) |> head()  # 4 per chamber
# Check: 6 chambers per warming level, and in every chamber 4 low and 4 high
# nitrogen pots. The design is balanced, as described.

# Graph that shows the answer to the question, with chambers identifiable.
# Points are moved a little sideways (jitter) so they do not overlap; lines
# join the mean of each chamber at low and high nitrogen.
chamber_means <- chamber_warming |>
  group_by(chamber, warming, nitrogen) |>
  summarise(mean_biomass = mean(biomass_g), .groups = "drop")
ggplot(chamber_warming, aes(x = nitrogen, y = biomass_g, color = chamber)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
  geom_line(data = chamber_means, aes(y = mean_biomass, group = chamber)) +
  facet_wrap(~ warming) +
  labs(x = "Nitrogen", y = "Above-ground biomass (g)", color = "Chamber")
# Look at: biomass is higher with high nitrogen in every chamber, and the lines
# are steeper in warmed chambers (a larger nitrogen effect: an interaction).
# Chambers also differ in their overall level (some lines are higher than
# others), so pots in the same chamber are not independent.

# Mean biomass for each combination of warming and nitrogen
chamber_warming |>
  group_by(warming, nitrogen) |>
  summarise(mean_biomass = mean(biomass_g), .groups = "drop")

# Question: mean biomass of high-nitrogen plants in warmed chambers (1 d.p.)?
# Answer: 15.6 g. (Ambient high 14.46 g, ambient low 10.71 g, warmed low 10.11 g.)


# Step 3: Fit the models ----
# First the model that ignores the chambers (wrong), then the mixed model.
m_biomass_lm <- lm(biomass_g ~ warming * nitrogen, data = chamber_warming)
m_biomass_mixed <- lmer(biomass_g ~ warming * nitrogen + (1 | chamber),
                        data = chamber_warming)
summary(m_biomass_lm)
summary(m_biomass_mixed)
anova(m_biomass_mixed)

# Look at: the coefficient estimates are the same in both models (the design
# is balanced), but the standard errors and df differ:
#   coefficient                 SE (lm)  SE (mixed)  df (mixed, summary)
#   warmingwarmed                0.49     0.81        12
#   nitrogenhigh                 0.49     0.35        82
#   warmingwarmed:nitrogenhigh   0.69     0.50        82
# In anova(m_biomass_mixed), warming is tested with 10 denominator df (DenDF).

# Question: which statements about the SE and df of warming are correct?
# Answer (two are correct):
# - The linear model is overconfident about warming: it treats the 8 pots in
#   a chamber as independent replicates of warming (pseudoreplication).
# - In the mixed model warming is tested with about 10 denominator df (in
#   anova(); about 12 for the coefficient in summary()), matching the 12
#   chambers, not the 96 pots.
# The larger SE does NOT make the mixed model worse: it is honest. And the
# SEs of nitrogen and the interaction are SMALLER in the mixed model, because
# these effects are estimated within chambers, so the variation among chambers
# no longer adds to their uncertainty.


# Step 4: Check the model ----
plot(m_biomass_mixed)                # residuals vs fitted (autoplot() does not work)
qqnorm(resid(m_biomass_mixed))       # QQ-plot of the residuals
qqline(resid(m_biomass_mixed))
ranef(m_biomass_mixed)               # estimated effect of each chamber
# Look at: no pattern and even spread in the residuals vs fitted plot; points
# close to the line in the QQ-plot; chamber effects between about -1.3 and
# +2.4 g, with C05 the largest but not extreme. No obvious problems.


# Step 5: Answer the question ----

# Question: F-value for the warming x nitrogen interaction in anova() (1 d.p.)?
# Answer: 12.8 (F = 12.76 on 1 and 82 df, p = 0.0006). There is clear evidence
# that the effect of nitrogen depends on warming.

# Effect of high nitrogen in warmed chambers: with an interaction, add the
# interaction coefficient to the nitrogen coefficient (which is the nitrogen
# effect at the reference level of warming, ambient).
coef_mixed <- fixef(m_biomass_mixed)
coef_mixed
coef_mixed["nitrogenhigh"] + coef_mixed["warmingwarmed:nitrogenhigh"]

# Question: effect of high (vs low) nitrogen in warmed chambers (2 d.p.)?
# Answer: 5.53 g = 3.75 + 1.78. (In ambient chambers it is 3.75 g.)

# 95% confidence intervals of the fixed effects
confint(m_biomass_mixed, parm = "beta_", method = "Wald")
# Look at: interaction 0.80 to 2.76 g; warmingwarmed -2.19 to 0.98 g.

# Question: what does the coefficient warmingwarmed (-0.60) mean?
# Answer: it is the estimated difference between warmed and ambient chambers
# FOR LOW-NITROGEN PLANTS (the reference level of nitrogen). Its CI includes
# zero, so the data are compatible with no effect of warming at low nitrogen.
# (At high nitrogen the warming effect is -0.60 + 1.78 = 1.18 g.)
coef_mixed["warmingwarmed"] + coef_mixed["warmingwarmed:nitrogenhigh"]

# Variance components: column vcov has the variances
biomass_varcomp <- as.data.frame(VarCorr(m_biomass_mixed))
biomass_varcomp
biomass_varcomp$vcov[1] / sum(biomass_varcomp$vcov)

# Question: proportion of the remaining variance among chambers (2 d.p.)?
# Answer: 0.52 = 1.60 / (1.60 + 1.49). (SD 1.26 g among chambers, 1.22 g among
# pots within chambers.) Pots in the same chamber are far from independent,
# which is why chamber must be in the model.

# Figure for publication: data plus model predictions with 95% CIs.
# 1) new data: every combination of warming and nitrogen
new_biomass <- expand_grid(warming = levels(chamber_warming$warming),
                           nitrogen = levels(chamber_warming$nitrogen)) |>
  mutate(warming = factor(warming, levels = levels(chamber_warming$warming)),
         nitrogen = factor(nitrogen, levels = levels(chamber_warming$nitrogen)))
# 2) predictions for an average chamber: re.form = NA ignores the random effects
new_biomass <- new_biomass |>
  mutate(fit = predict(m_biomass_mixed, newdata = new_biomass, re.form = NA))
# 3) predict() for mixed models gives no confidence intervals, so we calculate
# the standard error of each prediction from the model's coefficient
# uncertainty (vcov), and use fit +/- 1.96 x SE (a Wald interval, like
# confint(..., method = "Wald")). You do not need to know this matrix
# calculation for the exam.
X <- model.matrix(~ warming * nitrogen, data = new_biomass)
new_biomass <- new_biomass |>
  mutate(se = sqrt(diag(X %*% as.matrix(vcov(m_biomass_mixed)) %*% t(X))),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se)
new_biomass
# Check: the fitted values equal the four cell means from step 2.

# 4) plot chamber means (the data at the level of the experimental unit for
# warming) and the model predictions with their 95% CI
ggplot(chamber_means, aes(x = nitrogen, y = mean_biomass, color = warming)) +
  geom_point(position = position_dodge(width = 0.4), alpha = 0.4, size = 2) +
  geom_pointrange(data = new_biomass,
                  aes(y = fit, ymin = lwr, ymax = upr),
                  position = position_dodge(width = 0.4), size = 0.6) +
  geom_line(data = new_biomass, aes(y = fit, group = warming),
            position = position_dodge(width = 0.4)) +
  labs(x = "Nitrogen", y = "Above-ground biomass (g)", color = "Temperature") +
  theme_classic()
# Small points: the mean of each chamber; large points and bars: model
# prediction for an average chamber with its 95% CI. The steeper line for
# warmed chambers shows the interaction.


# Step 6: Report ----
# Answer (best reporting sentence):
# "Warming increased the response of plant biomass to nitrogen: high nitrogen
# increased biomass by 3.7 g in ambient chambers but by 5.5 g in warmed
# chambers (difference 1.8 g, 95% CI 0.8-2.8 g; linear mixed model with chamber
# as a random effect, warming x nitrogen F(1, 82) = 12.8, p < 0.001). At low
# nitrogen, warming had no clear effect (-0.6 g, 95% CI -2.2 to 1.0 g; 12 chambers)."

# Reflection: how would you redesign the experiment to study mainly warming?
# Model answer: the precision of the warming effect is limited by the number of
# CHAMBERS (12, so 10 df), not the number of pots: more pots per chamber would
# hardly help, because the chambers differ a lot (52% of the variance). So use
# more chambers (e.g. 24 chambers with fewer pots each), if possible. Reducing
# the differences among chambers (e.g. identical chambers, blocking chambers by
# location or time) would also make the warming effect more precise.


# Check that the script is reproducible ----
# Finally: Session > Restart R, then run the whole script again from the top
# (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac). If it runs without errors
# and gives the same answers, your analysis is reproducible.
