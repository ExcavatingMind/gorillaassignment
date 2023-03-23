library(tidyverse)
library(rstatix)
library(lme4)
library(lmerTest)
data <- read_csv("analysis.csv")


tidy_data <- data %>%
  mutate(colour = factor(colour),
         condition = factor(condition))

summary_q1 <- tidy_data %>%
  group_by(condition) %>% #specify the grouping
  get_summary_stats(rt, type = "common")
summary_q1

model <- lmer(rt ~ condition + (1 | colour),
              data = tidy_data)

model_null <- lmer(rt ~ (1 | colour),
                   data = tidy_data)
anova(model, model_null)


