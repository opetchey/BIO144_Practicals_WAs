# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 11 practical: example solution (mixed models; what next)
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
# - The datasets are simulated in the script or come with the lme4 package,
#   so this week you do not need to download anything.
# - Theory: course book, Chapter 11 (Mixed models; What next).
# =============================================================================


# Load the packages ----
library(tidyverse)  # dplyr, tidyr, ggplot2
library(lme4)       # lmer() and the sleepstudy data
library(lmerTest)   # adds degrees of freedom and p-values to lmer() output
# Note: once lmerTest is loaded, lmer() is the lmerTest version. It fits
# exactly the same model as lme4::lmer(); it only adds df and p-values.


# ============================== Practical part 1 ==============================
# Partial pooling: test scores of students in classes


# Part 1, step 1: Simulate the data ----
# This is the code from the practical, with the same set.seed(), so you get
# the same numbers as everyone else.
set.seed(123)
n_classes <- 10
# The true average score of each class
class_average_score <- runif(n_classes, min = 60, max = 90)
# Number of students in each class (between 2 and 25)
n_students_per_class <- sample(2:25, n_classes, replace = TRUE)
class_features <- tibble(
  class = paste0("class_", 1:n_classes),
  n_students = n_students_per_class,
  class_effect = class_average_score
)
# Simulate the score of each student: class average plus noise (sd = 15)
student_scores <- class_features |>
  rowwise() |>
  mutate(scores = list(rnorm(n_students, mean = class_effect, sd = 15))) |>
  unnest(cols = c(scores)) |>
  select(class, score = scores) |>
  mutate(student_id = paste0("student_", row_number()))

student_scores

# Number of students per class
student_scores |>
  group_by(class) |>
  summarise(n_students = n())
# Look at: class sizes vary from 4 (class_7) to 23 (class_3); 127 students in all.


# Part 1, step 2: A standard linear model (class as a fixed effect) ----
m_scores_lm <- lm(score ~ class, data = student_scores)

# Question: in lm(score ~ class), what does the model estimate for each class?
# Answer: a separate mean score for each class, estimated independently of the
# other classes (no pooling of information).

# Class means predicted by the linear model
new_data <- data.frame(class = unique(student_scores$class))
new_data$predicted_lm <- predict(m_scores_lm, newdata = new_data)
new_data

# The same means, calculated directly with dplyr
class_means <- student_scores |>
  group_by(class) |>
  summarise(mean_score = mean(score))
class_means
# Check: identical to predicted_lm. The linear model simply estimates the mean
# of each class.


# Part 1, step 3: A linear mixed model (class as a random effect) ----
# (1 | class): an overall mean, plus a random deviation for each class.
m_scores_lmm <- lmer(score ~ 1 + (1 | class), data = student_scores)
summary(m_scores_lmm)
# Look at: the overall mean (Intercept, about 76.3), the SD among classes
# (about 10.0) and the residual SD among students within classes (about 13.9).

# Predictions for each class from the mixed model.
# re.form = NULL means "include the random effect of each class".
new_data <- new_data |>
  mutate(predicted_lmm = predict(m_scores_lmm, newdata = new_data, re.form = NULL))
new_data

# Question: when you compare the LM and LMM predictions, what do you see?
# Answer: the lower LMM predictions tend to be larger than the LM predictions,
# and the higher LMM predictions tend to be smaller. For example class_6: LM
# 55.4, LMM 58.8 (moved up); class_5: LM 93.8, LMM 92.3 (moved down).
# WHY: partial pooling shrinks extreme class estimates towards the overall mean.


# Part 1, step 4: Visualise the shrinkage ----
ggplot(new_data, aes(x = class)) +
  geom_point(aes(y = predicted_lm, color = "LM Predictions"), size = 3, alpha = 0.5) +
  geom_point(aes(y = predicted_lmm, color = "LMM Predictions"), size = 3, alpha = 0.5) +
  geom_hline(yintercept = mean(student_scores$score), linetype = "dashed", color = "black") +
  labs(y = "Score", color = "Legend") +
  theme_minimal() +
  coord_flip()

# Question: what do you observe in the graph?
# Answer: the LMM predictions are closer to the overall mean (dashed line) than
# the LM predictions: extreme class estimates are pulled towards the mean.

# Which classes were pulled most strongly towards the overall mean?
# Calculate the shrinkage: the proportion of the distance to the overall mean
# by which each class estimate was moved (0 = not moved, 1 = moved all the way).
overall_mean <- fixef(m_scores_lmm)[1]
new_data |>
  left_join(select(class_features, class, n_students), by = "class") |>
  mutate(shrinkage = (predicted_lm - predicted_lmm) / (predicted_lm - overall_mean)) |>
  arrange(n_students)
# Answer: the smallest classes are shrunk most: class_7 (4 students, shrinkage
# 0.33) and class_4 (6 students, 0.24); the largest classes least: class_3
# (23 students, 0.08) and class_1 (21 students, 0.08). WHY: the mean of a small
# class is a noisy estimate, so the model borrows more information from the
# other classes. (Note: in absolute terms, a class far from the mean can move
# more points, e.g. class_6; the proportion shows the effect of class size.)


# Part 1, step 5 (shown in the practical): the same simulation with 1000 classes ----
# The practical shows the graphs from this code; here is the code, so you can
# make them yourself. (It takes a few seconds to run.)
set.seed(123)
n_classes <- 1000
class_average_score <- runif(n_classes, min = 60, max = 90)
n_students_per_class <- sample(2:25, n_classes, replace = TRUE)
class_features_1000 <- tibble(
  class = paste0("class_", 1:n_classes),
  n_students = n_students_per_class,
  class_effect = class_average_score
)
student_scores_1000 <- class_features_1000 |>
  rowwise() |>
  mutate(scores = list(rnorm(n_students, mean = class_effect, sd = 15))) |>
  unnest(cols = c(scores)) |>
  select(class, score = scores) |>
  mutate(student_id = paste0("student_", row_number()))
m_scores_lm_1000 <- lm(score ~ class, data = student_scores_1000)
m_scores_lmm_1000 <- lmer(score ~ 1 + (1 | class), data = student_scores_1000)
new_data_1000 <- data.frame(class = unique(student_scores_1000$class))
new_data_1000 <- new_data_1000 |>
  mutate(predicted_lm = predict(m_scores_lm_1000, newdata = new_data_1000),
         predicted_lmm = predict(m_scores_lmm_1000, newdata = new_data_1000, re.form = NULL)) |>
  left_join(select(class_features_1000, class, n_students), by = "class")

# Predictions of both models for every class
ggplot(new_data_1000, aes(x = class)) +
  geom_point(aes(y = predicted_lm, color = "LM Predictions"), size = 1, alpha = 0.5) +
  geom_point(aes(y = predicted_lmm, color = "LMM Predictions"), size = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(student_scores_1000$score), linetype = "dashed") +
  labs(y = "Score", color = "Legend") +
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Distributions of the predictions
ggplot(new_data_1000) +
  geom_density(aes(x = predicted_lm, color = "LM Predictions"), linewidth = 1) +
  geom_density(aes(x = predicted_lmm, color = "LMM Predictions"), linewidth = 1) +
  labs(x = "Predicted score", color = "Legend") +
  theme_minimal()

# Question: how do you interpret the two density curves?
# Answer: the LMM predictions have a narrower distribution than the LM
# predictions, reflecting shrinkage towards the overall mean.

# Shrinkage against class size (classes very close to the overall mean are
# left out, because for them the proportion is unstable)
overall_mean_1000 <- fixef(m_scores_lmm_1000)[1]
shrinkage_data <- new_data_1000 |>
  filter(abs(predicted_lm - overall_mean_1000) > 2) |>
  mutate(shrinkage = (predicted_lm - predicted_lmm) / (predicted_lm - overall_mean_1000))
ggplot(shrinkage_data, aes(x = n_students, y = shrinkage)) +
  geom_point(alpha = 0.3) +
  labs(x = "Number of students in the class",
       y = "Shrinkage towards the overall mean (proportion)") +
  theme_minimal()
# Look at: classes of 2 students are moved more than half way to the overall
# mean (median shrinkage about 0.6); classes of 25 students only about 0.1.

# Question: why does the difference between LM and LMM predictions matter?
# Answer: partial pooling reduces the influence of random noise in small
# groups, leading to more reliable estimates and predictions.

# Question: how does a random-slopes model differ from separate slopes for
# each class in a linear model (an interaction)?
# Answer: the mixed model partially pools the class-specific slopes towards a
# common average slope; the linear model estimates each slope independently.

# Fixed or random effect? Model answer: class names are arbitrary labels
# (class_1, class_2, ...), and we want to generalise to classes in general, so
# class is best a random effect. A meaningful, repeatable factor (e.g. teaching
# method) would be a fixed effect, with class as a random effect.


# ============================== Practical part 2 ==============================
# Sleep deprivation and reaction time


# Part 2: Get the data ----
# The sleepstudy data come with the lme4 package (loaded at the top).
data(sleepstudy)
head(sleepstudy)
nrow(sleepstudy)                 # 180 rows
n_distinct(sleepstudy$Subject)   # 18 subjects
sleepstudy |> count(Subject)     # 10 measurements (days 0 to 9) per subject

# Question: what is the structure of these data?
# Answer: repeated measures. The 10 measurements of each subject are grouped;
# measurements from the same person are likely to be more similar to each
# other than to those of other people. The independent units are the 18 subjects.


# Part 2, step 1: Look at the data ----
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Subject) +
  labs(x = "Days of sleep deprivation", y = "Reaction time (ms)")
# Look at: subjects differ in their reaction time at day 0 (intercept) and in
# how fast their reaction time increases (slope). E.g. subjects 309 and 335
# hardly change (335 even gets slightly faster), subject 308 slows down a lot.


# Part 2, step 2: The wrong model: ignoring the subjects ----
m_sleep_lm <- lm(Reaction ~ Days, data = sleepstudy)
summary(m_sleep_lm)
# Look at: Days slope 10.47 ms per day, SE 1.24, on 178 residual df.

# Question: why are 178 residual degrees of freedom a warning sign?
# Answer: there are only 18 independent units (subjects). The model treats
# the 180 repeated measurements as independent, so it thinks it has much more
# information than it has (pseudoreplication). Its SEs and p-values are not
# trustworthy.


# Part 2, step 3: One honest solution: one slope per subject ----
# Fit a regression for each subject, and keep the slope (2nd coefficient).
subject_slopes <- sleepstudy |>
  group_by(Subject) |>
  summarise(slope = coef(lm(Reaction ~ Days))[2])
subject_slopes
t.test(subject_slopes$slope)
# Look at: mean slope 10.47 ms per day, t = 6.77, df = 17, p < 0.0001
# (95% CI 7.2 to 13.7). Valid (one value per independent unit), but it ignores
# how precisely each slope is estimated, and it is hard to add other
# explanatory variables.


# Part 2, step 4: A random intercept model ----
m_sleep_ri <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(m_sleep_ri)
# Look at the "Random effects" part: Variance and Std.Dev. among subjects
# (Subject (Intercept)) and within subjects (Residual).

# Question: what is the SD among subjects (random intercept), in ms?
# Answer: 37.1 ms (Std.Dev. of Subject (Intercept) = 37.12). The residual SD
# within subjects is 31.0 ms.

# Variance components as a data frame: column vcov has the variances
sleep_ri_varcomp <- as.data.frame(VarCorr(m_sleep_ri))
sleep_ri_varcomp
# Proportion of variance among subjects (use variances, not SDs!)
sleep_ri_varcomp$vcov[1] / sum(sleep_ri_varcomp$vcov)

# Question: what proportion of the (remaining) variance is among subjects?
# Answer: 0.59 = 1378 / (1378 + 960). About 59% of the variation in reaction
# time not explained by Days is due to consistent differences among people,
# so measurements from the same person are strongly correlated.


# Part 2, step 5: A random intercept and random slope model ----
m_sleep_rs <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(m_sleep_rs)

# Question: which statement about Reaction ~ Days + (Days | Subject) is correct?
# Answer: it estimates an average (fixed) slope of Days, and lets both the
# intercept and the slope vary among subjects (random intercepts and slopes).

# Question: what is the SD among subjects in their slope (ms per day)?
# Answer: 5.9 ms per day (Std.Dev. of Days in Random effects = 5.922).
# So roughly 95% of people would have slopes between about
# 10.5 - 2 x 5.9 = -1 and 10.5 + 2 x 5.9 = 22 ms per day.


# Part 2, step 6: Compare the fixed effect of Days across the models ----
# Make the table: estimate, SE and df of the Days slope in each model.
# coef(summary(m)) gives the table of fixed effects of a model.
coef(summary(m_sleep_lm))["Days", ]
coef(summary(m_sleep_ri))["Days", ]
coef(summary(m_sleep_rs))["Days", ]
# The table (from the output above):
#   model                              estimate   SE     df
#   lm (ignores subjects)              10.47      1.24   178
#   random intercepts                  10.47      0.80   161
#   random intercepts and slopes       10.47      1.55    17

# Question: which statements are correct?
# Answer (three are correct):
# - The random slope model gives the largest SE, because it recognises that
#   subjects differ in their slopes: the average slope is estimated from 18
#   subjects, so about 17 df.
# - Its test (t = 6.77, 17 df) matches the t-test on the 18 subject slopes
#   from step 3 exactly (the data are balanced).
# - df for fixed effects in mixed models must be approximated (Satterthwaite,
#   from lmerTest); they reflect how many independent units carry information.
# The lm is NOT best: its many df are fake (pseudoreplication). The random
# intercept model is also overconfident (SE 0.80), because it ignores that
# subjects differ in their slopes.


# Part 2, step 7: Check the model ----
plot(m_sleep_rs)            # residuals vs fitted values (autoplot() does not work)
qqnorm(resid(m_sleep_rs))   # QQ-plot of the residuals
qqline(resid(m_sleep_rs))
# Look at: no strong pattern in the residuals vs fitted plot; the QQ-plot is
# close to the line in the middle, but there are a few large residuals in both
# tails (e.g. one measurement about 130 ms slower than the model predicts).
# These few outliers are worth checking, but there are no serious problems.

# Question: which are assumptions or limitations of this mixed model?
# Answer (three are correct): subject intercepts and slopes come from normal
# distributions, and residuals are normal with constant variance; the effect
# of Days is linear over the 10 days; with only 18 subjects the among-subject
# variances (especially of the slopes) are estimated imprecisely.
# It does NOT assume that the 180 measurements are independent.


# Part 2, step 8: Report ----
# 95% confidence intervals of the fixed effects
confint(m_sleep_rs, parm = "beta_", method = "Wald")
# Days: 7.4 to 13.5 ms per day.

# Answer (best reporting sentence):
# "Reaction time increased by on average 10.5 ms per day of sleep deprivation
# (95% CI 7.4-13.5 ms; linear mixed model with random intercepts and slopes for
# the 18 subjects, t = 6.8, df = 17, p < 0.0001), although subjects varied
# considerably in their sensitivity (SD of slopes among subjects 5.9 ms per day)."


# ============================== Practical part 3 ==============================
# What next? (No data analysis; read the course book, Chapter 11.2.)

# Question: which studies could be analysed with BIO144 methods?
# Answer: seedlings vs light (Poisson or quasi-Poisson GLM); infection vs body
# size (binomial GLM); tree growth over 5 years vs fertiliser (mixed model
# with tree as random effect); 30 species at 50 sites (ordination).
# NOT the 30-year daily temperature forecast: that needs time series analysis,
# because each observation depends on the previous ones.

# Question: fish survival times, with some fish still alive at day 200?
# Answer: a time-to-event response, where some survival times are only known
# to be longer than 200 days (censored): survival analysis.

# Question: which statements about choosing and planning an analysis are correct?
# Answer: (1) the analysis, including how many replicates are needed (power
# analysis), should be planned when the study is designed, before collecting
# data; (2) the research question should determine the analysis, not the
# techniques we happen to know.


# Check that the script is reproducible ----
# Finally: Session > Restart R, then run the whole script again from the top
# (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac). If it runs without errors
# and gives the same answers, your analysis is reproducible.
