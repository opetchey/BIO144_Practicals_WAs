# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 1 practical: example solution
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
# - The data file is downloaded from the internet, so you need to be online.
#
# The class data are different every year, so we do not give any numbers
# (numbers of participants, means, p-values) here. Your results are the
# results for your class.
# =============================================================================


# Load the add-on packages ----
# All library() lines go at the top of a script.
# library(tidyverse) would load all four of these packages in one line.
library(readr)    # read_csv()
library(dplyr)    # mutate(), group_by(), summarise(), filter()
library(tidyr)    # pivot_longer()
library(ggplot2)  # graphs


# Part 1: Get working in RStudio ----
# This part is not R code in a script: you do it in RStudio.
# - Log in to the UZH RStudio Server: https://rstudio.mnf.uzh.ch/
#   (UZH username and password). You will use RStudio Server in the exam.
# - Find the four panes: script editor (top left), console (bottom left),
#   environment (top right), files/plots/packages/help (bottom right).

# Part 1, step 2: type this in the console and press Enter.
1 + 1
# The result [1] 2 appears in the console.

# Part 1, step 3: run this line from the script (Ctrl+Enter, or Cmd+Enter).
my_number <- 1 + 1
# Nothing is printed, but my_number now appears in the Environment pane.
my_number

# Answer (match the panes): 1B, 2C, 3A.
# The script editor is where you write and save code (B), the console is where
# R runs commands and prints results and errors (C), and the environment shows
# the objects that exist in R's memory (A).


# Part 2, step 1: Make an RStudio project ----
# Not R code: File > New Project... > New Directory > New Project.
# Name it BIO144. Open this project every time you work on BIO144.

# Answer (why use a project?): the first two answers are correct.
# - When the project is open, R looks for files in the project folder, so you
#   can read a data file just by its name.
# - Everything for the work (scripts, data, results) is in one place.
# (R does not run faster in a project, and a project does not stop errors.)


# Part 2, step 2: Install and load add-on packages ----
# Install a package only ONCE on each computer. Do this in the console, not
# in your script, so it is commented out here:
# install.packages("tidyverse")
# Load the packages EVERY time you start R. We did this at the top of the
# script with library().

# Answer (statements about packages): the first two are correct.
# You install once (install.packages() or the Packages pane), and you load
# with library() in every new R session. Installed packages are NOT available
# until you load them, and install.packages() does not belong in a script.


# Part 3: Get the data into R ----
# Download the data file into the project folder (run this once).
download.file("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/reaction_times_2027.csv",
              destfile = "reaction_times_2027.csv")
# Look in the Files pane: reaction_times_2027.csv should be there.
# Do not open it in Excel and save it: Excel can change the data.

# Read the file into R, and look at it.
class_RTs <- read_csv("reaction_times_2027.csv")
class_RTs
str(class_RTs)
summary(class_RTs)
# Look at:
# - The number of rows (one per participant) and columns. There should be
#   7 columns: five reaction times, a random number, and sex at birth.
#   Does the number of rows match about how many people were in the lecture?
# - The five reaction times and the random number should be numbers <dbl>,
#   and sex at birth should be text <chr>.
# - The column names contain spaces (e.g. `Reaction time 1`). That is why the
#   script in Part 4 gives them short, simple names.
# - In summary(), look at the minimum and maximum reaction times. Are there
#   any values that cannot be real (e.g. 2000 ms)? Part 4 deals with these.

# Answer (file does not exist in current working directory): the first three
# are correct. The file name in the code is not exactly the file's name, OR
# the file is not in the project folder (or the project is not open), OR a web
# browser added an extra ending (e.g. .txt) to the name. If readr were not
# loaded, the message would be: could not find function "read_csv".

# Answer (why check variable types?): the first answer. If one value in a
# column of numbers is not a number (e.g. "250ms", or "0,25"), R reads the
# whole column as text <chr>, and calculations on it fail or are wrong.


# Part 4: Fix the reaction time script ----
# Below is the complete corrected script. Each error that was planted in
# reaction_time_analysis_needs_fixing.r is marked with "# FIX n:".
# There were 11 errors (the practical says "about ten").
# (You got the script with this line, so we do not need to run it again:
# download.file("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/scripts/reaction_time_analysis_needs_fixing.r",
#               destfile = "reaction_time_analysis_needs_fixing.r")
# )
#
# FIX 1 is in the library() lines at the top of this script:
# FIX 1: library(tidyr) was missing. You only notice this much later, when
#        pivot_longer() gives: could not find function "pivot_longer".
#        The name is spelled correctly, so the problem is that its package
#        (tidyr) is not loaded.

# Part 4, step 1: Get the data ----
# (The corrected script downloads and reads the data itself, so that it
# works on its own. You already downloaded the file in Part 3.)
download.file("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/reaction_times_2027.csv",
              destfile = "reaction_times_2027.csv")

# Read the data file into R.
# FIX 2: the function is read_csv(), not read_cvs().
#        Error was: could not find function "read_cvs".
# FIX 3: the file is called reaction_times_2027.csv, not reaction_time_2027.csv.
#        Error was: 'reaction_time_2027.csv' does not exist in current working directory.
class_RTs <- read_csv("reaction_times_2027.csv")

# Have a look at the data. Does it look OK?
# FIX 4: the object is called class_RTs, not class_RT.
#        Error was: object 'class_RT' not found.
class_RTs

# Part 4, step 2: Tidy up the data ----
# Give the variables short, simple names.
# FIX 5: a comma was missing after "RT5".
#        Error was: unexpected string constant.
names(class_RTs) <- c("RT1", "RT2", "RT3", "RT4", "RT5",
                      "Random_number",
                      "Sex_at_birth")

# Check that the variable names are now what we set them to be.
# FIX 6: class_RTs (underscore), not class-RTs (minus sign). R read class-RTs
#        as "class minus RTs". Error was: object 'RTs' not found.
names(class_RTs)

# Check the variable types: RT1 to RT5 and Random_number should be <dbl>,
# and Sex_at_birth should be <chr>.
str(class_RTs)

# Add an identifier for each participant (ID-1, ID-2, ...).
# FIX 7: row_number is a function, so it needs brackets: row_number().
#        Error was: cannot coerce type 'closure' to vector of type 'character'
#        ("closure" is R's word for a function).
class_RTs <- class_RTs |>
  mutate(ID = paste0("ID-", row_number()))

# How many participants are there of each sex at birth?
# FIX 8: the variable is Sex_at_birth with a capital S (R is case sensitive).
#        Error was: Column `sex_at_birth` is not found.
class_RTs |>
  group_by(Sex_at_birth) |>
  summarise(number = n())

# Rearrange the data so that each reaction time is on its own row
# ("long" format: one row per participant per trial).
# (This is where FIX 1, library(tidyr), is needed.)
RTs_long <- class_RTs |>
  pivot_longer(cols = starts_with("RT"),
               names_to = "Trial",
               values_to = "RT_value")
RTs_long
# Look at: five times as many rows as class_RTs.

# Calculate the mean of the five reaction times of each participant.
mean_RTs <- RTs_long |>
  group_by(ID, Sex_at_birth) |>
  summarise(mean_RT = mean(RT_value), .groups = "drop")
mean_RTs

# Part 4, step 3: Look at the data in graphs ----
# Histogram of the mean reaction times.
# (The message about bins is information, not an error.)
ggplot(data = mean_RTs, aes(x = mean_RT)) +
  geom_histogram()

# One histogram for each sex at birth.
ggplot(data = mean_RTs, aes(x = mean_RT)) +
  geom_histogram() +
  facet_grid(~ Sex_at_birth)

# Box plot with the data points on top.
ggplot(data = mean_RTs, aes(x = Sex_at_birth, y = mean_RT)) +
  geom_boxplot() +
  geom_jitter(width = 0.05)
# Look at: are there any points far away from the others? These are probably
# mistakes in measuring or recording, and the next step removes them.

# Part 4, step 4: Remove implausible values ----
# Keep only mean reaction times greater than 50 ms and less than 500 ms.
# FIX 9: the conditions were the wrong way round (mean_RT < 50, mean_RT > 500).
#        No value can be below 50 AND above 500, so no rows were kept.
#        There was NO error message here: this is a "silent" error. You only
#        see it if you check the result (nrow() gave 0), or when the t-test
#        then fails (with: grouping factor must have exactly 2 levels).
mean_RTs_filtered <- mean_RTs |>
  filter(mean_RT > 50, mean_RT < 500)

# How many participants are left? (It should be most of them!)
# FIX 10: the closing bracket was missing: nrow(mean_RTs_filtered).
#         The console showed + (R waiting for the rest of the command), or
#         the next line gave an "unexpected symbol" error.
nrow(mean_RTs_filtered)
# Compare with nrow(mean_RTs): only a few participants should be removed.
nrow(mean_RTs)

# Part 4, step 5: The statistical test ----
# A two-sample t-test of mean reaction time between the sexes at birth.
RT_ttest <- t.test(mean_RT ~ Sex_at_birth,
                   data = mean_RTs_filtered,
                   var.equal = TRUE)

# Look at the result of the t-test.
# FIX 11: we want to see our result, RT_ttest, not t.test. Typing t.test
#         prints the code of the t.test function itself. There was no error
#         message, but not the output we wanted.
RT_ttest
# Look at: the t value, the degrees of freedom (df), the p-value, the mean
# of each group, and the 95% confidence interval of the difference.

# Answers to the questions in Part 4:
# - could not find function read_cvs: a typo in the function name; it should
#   be read_csv (FIX 2).
# - Match error messages to causes: 1B, 2D, 3A, 4C.
# - could not find function "pivot_longer" (spelled correctly): the tidyr
#   package is not loaded; library(tidyr) was missing (FIX 1).
# - 0 participants after filtering, no error from filter(): the filter
#   condition was the wrong way round (FIX 9).
# - Appropriate use of AI: the first two answers. Ask it to EXPLAIN an error
#   or a concept, then check it and fix the code yourself. Do not copy answers
#   without trying, and you cannot use AI in the exam.

# Using an AI assistant to understand an error (not R code).
# Example: you paste "cannot coerce type 'closure' to vector of type
# 'character'" and the line with row_number. A good explanation: "closure"
# means a function; paste0() was given the function row_number itself, not
# the result of calling it, row_number(). Compare this with the table in the
# practical, then fix the code yourself.

# Answer (why restart R and run everything?): to check that the script works
# on its own, and does not depend on objects you made earlier by running lines
# out of order or by typing in the console.


# Part 4, if you finish early (optional) ----
# A box plot with more informative axis labels (always give units).
ggplot(data = mean_RTs_filtered, aes(x = Sex_at_birth, y = mean_RT)) +
  geom_boxplot() +
  geom_jitter(width = 0.05) +
  labs(x = "Sex at birth", y = "Mean reaction time (ms)")

# The estimated difference between the groups and its 95% confidence interval.
# estimate holds the mean of each group (in alphabetical order: Female, Male).
RT_ttest$estimate
# The difference between the two means (Female minus Male). unname() just
# removes the (now misleading) label "mean in group Female".
unname(RT_ttest$estimate[1] - RT_ttest$estimate[2])
# The 95% confidence interval of this difference (also Female minus Male):
RT_ttest$conf.int
# How to read it: if the interval includes 0, the data are consistent with no
# difference between the groups. Think also about whether a difference of this
# size (a few ms?) would matter biologically. We look at reporting results in
# later units.


# Part 5: Previous knowledge check-in ----
# Not R code.
# - Work through the BIO144 Previous Knowledge self-tests (link in the practical).
# - Write down the two or three topics you feel least sure about, ask a TA
#   about one of them, and plan when to revise the others (before Unit 3).
# Answer (where to find the expected previous knowledge?): the Previous
# knowledge page of the course information website, and the BIO144 Previous
# Knowledge self-tests.


# Check that the script is reproducible ----
# Session > Restart R, then run the whole script from the top
# (Code > Run Region > Run All). If it runs without errors from a fresh
# start, it is a complete record of your analysis, and gives the same
# results every time, for you and for anyone you share it with.
