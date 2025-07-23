
rm(list = ls())

library(readxl)
library(here)
library(lubridate)
library(ggplot2)
library(pscl)
library(sjPlot)
library(ggpubr)
library(dplyr)

# Functions created.
`%nin%` <- Negate(`%in%`)

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

# Look at dates.



# Count deployments in wards
wards <- df %>%
  group_by(`Ward Code`, `Ward (approx)`) %>%
  mutate(`Faces seen` = as.numeric(`Faces seen`)) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    total_faces = sum(`Faces seen`, na.rm = TRUE),
    .groups = "drop"
  )

# Read population groups sheet, skipping the first 3 lines
census <- read_excel(here("data/Live Facial Recognition Deployments.xlsx"),
                 sheet = "Census data",
                 skip = 1)

# Create database with Black percentage
wards_black <- census %>%
  mutate(nonwhite_percentage = 1 - (`White British` / `All usual residents`)) %>%
  dplyr::select(`ward code`, `ward name`, `Black Percentage`, nonwhite_percentage)

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
  group_by(WardCode, WardName) %>%
  summarise(
    total_crimes = sum(across(all_of(month_cols), as.numeric), na.rm = TRUE),
    .groups = "drop"
  )

# Create data of Wards
#wards_all <- crime_totals %>%
#  left_join(wards, by = c("WardName" = "Ward (approx)")) %>%
#  left_join(wards_black, by = c("WardName" = "ward name"))

wards_all <- crime_totals %>%
  left_join(wards, by = c("WardCode" = "Ward Code")) %>%
  left_join(wards_black, by = c("WardCode" = "ward code"))

# How many missings are about to filled in?
wards_all %>% 
  select(total_deployments, total_minutes, total_faces) %>% 
  lapply(., function(x)sum(is.na(x))) # 608 out of 680.

# Fill NAs with 0s in facial recognition deployment
wards_all <- wards_all %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes),
         total_faces = ifelse(is.na(total_faces), 0 , total_faces))

# Spatial explore.
library(sf)

# Load.
wards_sf <- st_read("data/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")

# Comment: N 657 wards in 2018, but the wards_all data has 680. Which boundaries used?

# Raw boundaries from loaded in above.
ggplot(data = wards_sf) +
  geom_sf()

# What's the overlap with these ward codes and those used earlier?
matched_wards <- wards_all %>% 
  filter(wards_all$WardCode %in% wards_sf$GSS_CODE)

# Which ones don't match?
non_matched_wards <- wards_all %>% 
  filter(wards_all$WardCode %nin% wards_sf$GSS_CODE) # note negation

# N. What explains the non-matches? Have I done something weird/wrong?
nrow(matched_wards) # 149
nrow(non_matched_wards) # 531

# Join. This is based on assumed match with 2018 codes.
wards_joined_sf <- wards_sf %>% 
  left_join(wards_all, by = c("GSS_CODE" = "WardCode"), keep = TRUE)

# Plot to demonstrate. This doesn't tell us much about the analysis because
# the missings could be due to non-matches with the spatial data.
ggplot(data = wards_joined_sf) +
  geom_sf(mapping = aes(fill = total_minutes), colour = "transparent") +
  scale_fill_viridis_c()

# Bivariate correlations
cor.test(wards_all$`Black Percentage`, wards_all$total_deployments, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_deployments, method = "spearman")

cor.test(wards_all$`Black Percentage`, wards_all$total_minutes, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_minutes, method = "spearman")

cor.test(wards_all$`Black Percentage`, wards_all$total_faces, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_faces, method = "spearman")

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
p5 <- make_plot(`Black Percentage`, total_faces, "Black %", "Faces seen", "Black % vs Faces seen")
p6 <- make_plot(total_crimes, total_faces, "Total Crimes", "Faces seen", "Crimes vs Faces seen")

# Arrange plots in a 3x2 grid
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3,
          labels = LETTERS[1:6])

ggsave(here('scatterplots.png'))

# Standardise variables of interest
wards_all <- wards_all %>%
  mutate(
    black_std = scale(`Black Percentage`),
    nonwhite_std = scale(nonwhite_percentage),
    crime_std = scale(total_crimes)
  )

# Log transform dependent variables
#wards_all <- wards_all %>%
#  mutate(
#    log_deployments = as.integer(log(total_deployments + 1)),
#    log_minutes = as.integer(log(total_minutes + 1))
#  )

# Check overdispersion
mean(wards_all$total_deployments)
var(wards_all$total_deployments)

mean(wards_all$total_minutes)
var(wards_all$total_minutes)

mean(wards_all$total_faces)
var(wards_all$total_faces)

# Hurdle models
hurdle_deployments <- hurdle(
  total_deployments ~ black_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_minutes <- hurdle(
  total_minutes ~ black_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_faces <- hurdle(
  total_faces ~ black_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

# Summaries

summary(hurdle_deployments)
summary(hurdle_minutes)
summary(hurdle_faces)

tab_model(hurdle_deployments, hurdle_minutes, hurdle_faces,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("Deployments", "Minutes", "Faces"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models.doc"))

#Summary:
# We analysed whether the use of live facial recognition (LFR) across London wards could be explained by the proportion of Black residents in each area, after accounting for local crime levels. 
# We looked at three different outcomes: whether LFR was used in an area at all (binary use), and if so, how intensively it was used—measured by the number of deployments, the total minutes of deployment, and the number of faces detected.
# The results suggest that the racial composition of a ward is associated with the likelihood that LFR is used at all. Specifically, wards with higher proportions of Black residents were significantly more likely to receive any LFR deployment, even when crime levels were taken into account. 
# A one standard deviation increase in Black population associated with a 31% increase in the odds of any LFR use.
# However, once a ward had received at least one deployment, the intensity of LFR use—how many times it was deployed, how long it ran, or how many faces it detected—was not significantly related to the ethnic composition of the ward. 
# In contrast, local crime levels played a significant role in both parts of the model. 
# Areas with more reported crime were not only more likely to receive LFR deployments in the first place, but also experienced more frequent and longer-lasting use of the technology, and a higher number of faces detected.
# In short, while crime appears to drive both the decision to use LFR and how heavily it is applied, the racial composition of an area seems to matter primarily in the decision of whether to deploy the technology at all.

# Analysis for non whites as sensitivity

# Hurdle models
hurdle_deployments_nw <- hurdle(
  total_deployments ~ nonwhite_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_minutes_nw <- hurdle(
  total_minutes ~ nonwhite_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_faces_nw <- hurdle(
  total_faces ~ nonwhite_std + crime_std,
  data = wards_all,
  dist = "negbin"
)

# Summaries

summary(hurdle_deployments_nw)
summary(hurdle_minutes_nw)
summary(hurdle_faces_nw)

tab_model(hurdle_deployments_nw, hurdle_minutes_nw, hurdle_faces_nw,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("Deployments", "Minutes", "Faces"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models_nw.doc"))
