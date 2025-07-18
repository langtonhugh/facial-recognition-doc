
rm(list = ls())

library(readxl)
library(here)
library(lubridate)
library(ggplot2)
library(pscl)
library(sjPlot)
library(ggpubr)
library(dplyr)

# Read deployments sheet, skipping the first 3 lines
df <- read_excel(here("data/Live Facial Recognition Deployments.xlsx"),
                 sheet = "Deployment Data",
                 skip = 3)

# Remove empty rows
df <- df %>%
  filter(!is.na(Year))

# Convert to total minutes within dplyr
df <- df %>%
  mutate(
    duration_format = ymd_hms(Duration),
    total_minutes = hour(duration_format) * 60 + minute(duration_format)
  )

# Count deployments in wards
wards <- df %>%
  group_by(`Ward (approx)`) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    .groups = "drop"
  )

# Read population groups sheet, skipping the first 3 lines
census <- read_excel(here("data/Live Facial Recognition Deployments.xlsx"),
                 sheet = "Census data",
                 skip = 1)

# Create database with Black percentage
wards_black <- census %>%
  dplyr::select(`ward name`, `Black Percentage`)

# Read crime data
crime <- read.csv(here('data/MPS Ward Level Crime (most recent 24 months).csv'))

# Step 1: Create valid month column names
years <- 2023:2025
months <- sprintf("%02d", 1:12)
valid_ym <- as.vector(outer(years, months, paste0))
valid_ym <- valid_ym[valid_ym >= "202307" & valid_ym <= "202506"]
month_cols <- paste0("X", valid_ym)

# Step 2: Summarise by WardName
crime_totals <- crime %>%
  group_by(WardName) %>%
  summarise(
    total_crimes = sum(across(all_of(month_cols), as.numeric), na.rm = TRUE),
    .groups = "drop"
  )

# Create data of Wards
wards_all <- crime_totals %>%
  left_join(wards, by = c("WardName" = "Ward (approx)")) %>%
  left_join(wards_black, by = c("WardName" = "ward name"))

# Fill NAs with 0s in facial recognition deployment
wards_all <- wards_all %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes))

# Bivariate correlations
cor.test(wards_all$`Black Percentage`, wards_all$total_deployments, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_deployments, method = "spearman")

cor.test(wards_all$`Black Percentage`, wards_all$total_minutes, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_minutes, method = "spearman")

# Scatter plots

# Common plot style function
make_plot <- function(x, y, xlab, ylab, title) {
  ggplot(wards_all, aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, colour = "blue") +
    stat_cor(method = "spearman", label.x = 0, label.y = max(wards_all[[deparse(substitute(y))]], na.rm = TRUE)) +
    labs(x = xlab, y = ylab, title = title) +
    theme_classic()
}

# Create all six plots
p1 <- make_plot(`Black Percentage`, total_deployments, "Black %", "Deployments", "Black % vs Deployments")
p2 <- make_plot(total_crimes, total_deployments, "Total Crimes", "Deployments", "Crimes vs Deployments")
p3 <- make_plot(`Black Percentage`, total_minutes, "Black %", "Minutes", "Black % vs Minutes")
p4 <- make_plot(total_crimes, total_minutes, "Total Crimes", "Minutes", "Crimes vs Minutes")

# Arrange plots in a 3x2 grid
ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2,
          labels = LETTERS[1:4])

ggsave(here('scatterplots.png'))

# Standardise variables of interest
wards_all <- wards_all %>%
  mutate(
    black_std = scale(`Black Percentage`),
    crime_std = scale(total_crimes)
  )

# Log transform dependent variables
wards_all <- wards_all %>%
  mutate(
    log_deployments = as.integer(log(total_deployments + 1)),
    log_minutes = as.integer(log(total_minutes + 1))
  )

# Hurdle models
hurdle_deployments <- hurdle(
  log_deployments ~ black_std + crime_std,
  data = wards_all,
  dist = "poisson"
)

hurdle_minutes <- hurdle(
  total_minutes ~ black_std + crime_std,
  data = wards_all,
  dist = "poisson"
)

# Summaries

summary(hurdle_deployments)
#Racial composition has a statistically significant effect on whether a ward gets any facial recognition deployment, even after adjusting for crime.
#Crime drives the number of deployments among targeted wards, not race.

summary(hurdle_minutes)
#Both

tab_model(hurdle_deployments, hurdle_minutes,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("Deployments", "Minutes"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models.doc"))

#Summary:
# I wanted to find out whether live facial recognition (LFR) is being used more in areas with higher proportions of Black residents, even after accounting for how much crime happens in those areas. In other words, does ethnic makeup help explain where and how this technology is deployed, beyond just crime rates?
# The results suggest that it does—at least to some extent. Areas with more Black residents were more likely to have LFR used at all, even when crime levels were taken into account. Once LFR was deployed, the amount of crime was the strongest factor explaining how often and for how long it was used. But we also found that in areas with more Black residents, LFR tended to be used for slightly longer periods—even after adjusting for crime. On the other hand, the number of times it was deployed didn’t seem to depend on ethnic makeup once crime was considered.
# So overall, while crime is clearly a key driver of LFR use, the ethnic composition of an area also seems to matter—particularly in the decision about whether to use it there in the first place, and possibly in how long it’s used for.
