
#Anthony Zhang
# SDS 315

library(ggplot2)
library(tidyverse)
library(mosaic)
library(MatchIt)

# Problem 1: 
# Part A
#load dataset
armfold = read.csv("armfold.csv")
# The number of male and female students in the dataset
male <- sum(armfold$Sex == "Male")
female <- sum(armfold$Sex == "Female")
# The sample proportion of males who folded their left arm on top.
male_prop <- sum(armfold$Sex == "Male" & armfold$LonR_fold == 1) / sum(armfold$Sex == "Male")
# The sample proportion of females who folded their left arm on top
female_prop <- sum(armfold$Sex == "Female" & armfold$LonR_fold == 1) / sum(armfold$Sex == "Female")
male
female
male_prop
female_prop

# Part B
# observed diff in prop
obs_diff <- male_prop - female_prop
obs_diff

# PART C
# prop.test built in R func for two prop z test for diff in folding pref
a <- table(armfold$Sex, armfold$LonR_fold)
prop.test(x = c(a["Male", "1"], a["Female", "1"]),
          n = c(sum(armfold$Sex == "Male"), sum(armfold$Sex == "Female")),
          correct = FALSE)

#vars needed for formula + formula itself
n1 <- sum(armfold$Sex == "Male")
n2 <- sum(armfold$Sex == "Female")
SE <- sqrt((male_prop * (1 - male_prop)) / n1 + 
             (female_prop * (1 - female_prop)) / n2)
SE
# z star for 95% interval
z_star <- 1.96
MOE <- z_star * SE
# create confidence interval
lower_bound <- obs_diff - MOE
upper_bound <- obs_diff + MOE 
lower_bound
upper_bound

#PART D
# Written answer in R MARKDOWN

#PART E
# Written answer in R MARKDOWN

#PART F
# Written answer in R MARKDOWN

#PART G
# Written answer in R MARKDOWN

#PART H
# Written answer in R MARKDOWN

#PART I
# Written answer in R MARKDOWN


# Problem 2:
#PART A
# load data
turnout = read.csv("turnout.csv")
gotv_yes <- sum(turnout$GOTV_call == 1) 
gotv_no  <- sum(turnout$GOTV_call == 0) 
# The proportion of those receiving a GOTV call who voted in 1998
gotv_yes_prop <- sum(turnout$GOTV_call == 1 & turnout$voted1998 == 1) / gotv_yes
# The sample proportion of those not receiving a GOTV call who voted in 1998.
gotv_no_prop <- sum(turnout$GOTV_call == 0 & turnout$voted1998 == 1) / gotv_no
gotv_yes_prop
gotv_no_prop

# prop.test for the two prop z test for diff, only look at confidence interval 
b <- table(turnout$GOTV_call, turnout$voted1998)
prop.test(x = c(b["1", "1"], b["0", "1"]),
          n = c(sum(turnout$GOTV_call == 1), sum(turnout$GOTV_call == 0)),
          correct = FALSE)

#PART B
mean(AGE ~ GOTV_call, data=turnout)    # Age by GOTV call
mean(AGE ~ voted1998, data=turnout)     # Age by voted in 1998

t.test(AGE ~ GOTV_call, data=turnout)      # CI for AGE difference by GOTV
t.test(AGE ~ voted1998, data=turnout)     # CI for AGE difference by whether they voted in 1998
       

mean(GOTV_call ~ MAJORPTY, data=turnout) # GOTV call by major party registration
mean(voted1998 ~ MAJORPTY, data=turnout) # Voting in 1998 by major party registration

tab1 <- table(turnout$GOTV_call, turnout$MAJORPTY)
prop.test(x = tab1[,2], n = rowSums(tab1), correct = FALSE)  # CI for GOTV call by party reg
tab2 <- table(turnout$voted1998, turnout$MAJORPTY)
prop.test(x = tab2[,2], n = rowSums(tab2), correct = FALSE)  # CI for Voting by party reg

mean(GOTV_call ~ voted1996, data=turnout) # GOTV call by voted in 1996
mean(voted1998 ~ voted1996, data=turnout) # Voting in 1998 by voting in 1996

tab3 <- table(turnout$GOTV_call, turnout$voted1996)
prop.test(x = tab3[,2], n = rowSums(tab3), correct = FALSE)  # CI for GOTV call by 1996 vote
tab4 <- table(turnout$voted1998, turnout$voted1996)
prop.test(x = tab4[,2], n = rowSums(tab4), correct = FALSE)  # CI for 1998 vote by 1996 vote

#PART C
#matching using matchit
match_model <- matchit(GOTV_call ~ AGE + MAJORPTY + voted1996,
                       data = turnout, ratio = 5)
# summarize results
summary(match_model)
#create matched dataset
turnout_matched <- match.data(match_model)

#mean, prop balancing check
mean(AGE ~ GOTV_call, data = turnout_matched)
mean(MAJORPTY ~ GOTV_call, data = turnout_matched)
mean(voted1996 ~ GOTV_call, data = turnout_matched)

# Confidence interval for AGE difference by GOTV (t-test)
t.test(AGE ~ GOTV_call, data = turnout_matched)

# Confidence interval for MAJORPTY difference by GOTV (prop test)
tab_party <- table(turnout_matched$GOTV_call, turnout_matched$MAJORPTY)
prop.test(x = tab_party[,2], n = rowSums(tab_party), correct = FALSE)

# Confidence interval for voted1996 difference by GOTV (prop test)
tab_vote96 <- table(turnout_matched$GOTV_call, turnout_matched$voted1996)
prop.test(x = tab_vote96[,2], n = rowSums(tab_vote96), correct = FALSE)

#repeat analysis from PART A
gotv_yes_matched <- sum(turnout_matched$GOTV_call == 1)
gotv_no_matched  <- sum(turnout_matched$GOTV_call == 0)

voted_yes_gotv <- sum(turnout_matched$GOTV_call == 1 & turnout_matched$voted1998 == 1) / gotv_yes_matched
voted_yes_nogotv <- sum(turnout_matched$GOTV_call == 0 & turnout_matched$voted1998 == 1) / gotv_no_matched

voted_yes_gotv
voted_yes_nogotv

prop.test(
  x = c(sum(turnout_matched$GOTV_call == 1 & turnout_matched$voted1998 == 1),
        sum(turnout_matched$GOTV_call == 0 & turnout_matched$voted1998 == 1)),
  n = c(gotv_yes_matched, gotv_no_matched),
  correct = FALSE
)
