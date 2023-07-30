# Load packages
library(pwr) #loading package that includes power functions
library(tidyverse)

# Import data

data <- read_csv("2818108.csv")

# Data analysis - Filtering and selecting data

data.select <- select(data, condition, spatial.reasoning)
data.control <- filter(data.select, condition == "Control")
data.mozart <- filter(data.select, condition == "Mozart")
data.select_control <- select(data.control, spatial.reasoning)

# Descriptive statistics

descriptive.statistics = data.select %>%
  group_by(condition) %>%
  summarise(
    mean = mean(spatial.reasoning),
    sd = sd(spatial.reasoning),
    n = n()
  )

#Independent samples t-test

t.test(spatial.reasoning ~ condition,
       var.equal = TRUE,
       data = data.select)

# SD and mean for both conditions

group_by(data.select, condition) %>%
  summarise(mean = mean(spatial.reasoning),
            sd = sd(spatial.reasoning))

# Visualisation of data

final <- ggplot(data.select, aes(x = condition, y = spatial.reasoning)) +
  geom_violin(aes(fill = condition)) +
  geom_jitter(width = 0.3,
              shape = 17,
              color = "black",
              size=1.5) +
  geom_boxplot(width = 0.15) +
  geom_smooth(formula = y ~ x, method = "lm") +
  stat_summary(fun = "mean",
               geom = "point",
               color = "Red",
               size=2.5) +
  xlab("Group condition") +
  ylab("Spatial Reasoning Score") +
  theme_classic()

# Show visualisation in plots

final

# Save file, adjusting size

ggsave(final, file="mozart-spatial-reasoning.png", width=6, height=4, dpi=400)
