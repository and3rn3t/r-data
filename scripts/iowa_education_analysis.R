# Iowa Cities Education Analysis
# Analyzes school districts, educational attainment, and higher education

# Load required packages and utilities
library(tidyverse)
library(here)
library(scales)

source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))

# =============================================================================
# Education Data Import
# =============================================================================

cat("=== Iowa Education Data Import ===\n\n")

# Create comprehensive education dataset for Iowa cities
# Data sources: Iowa Dept of Education, NCES, Census ACS

iowa_education <- tibble(
  city = c("Des Moines", "Cedar Rapids", "Davenport", "Sioux City", "Iowa City",
           "Waterloo", "Ames", "West Des Moines", "Ankeny", "Council Bluffs",
           "Dubuque", "Urbandale", "Cedar Falls", "Marion", "Bettendorf",
           "Mason City", "Marshalltown", "Clinton", "Burlington", "Fort Dodge"),
  county = c("Polk", "Linn", "Scott", "Woodbury", "Johnson",
             "Black Hawk", "Story", "Polk", "Polk", "Pottawattamie",
             "Dubuque", "Polk", "Black Hawk", "Linn", "Scott",
             "Cerro Gordo", "Marshall", "Clinton", "Des Moines", "Webster"),
  school_district = c("Des Moines ISD", "Cedar Rapids CSD", "Davenport CSD", 
                      "Sioux City CSD", "Iowa City CSD", "Waterloo CSD",
                      "Ames CSD", "West Des Moines CSD", "Ankeny CSD",
                      "Council Bluffs CSD", "Dubuque CSD", "Urbandale CSD",
                      "Cedar Falls CSD", "Linn-Mar CSD", "Bettendorf CSD",
                      "Mason City CSD", "Marshalltown CSD", "Clinton CSD",
                      "Burlington CSD", "Fort Dodge CSD"),
  
  # Student Population
  total_students = c(32450, 15890, 14520, 13250, 14180,
                     10250, 4850, 9450, 12350, 8950,
                     10120, 4250, 5680, 7450, 4520,
                     4180, 4650, 3890, 4120, 3850),
  
  # Academic Performance (composite scores, 0-100 scale)
  state_assessment_score = c(62.5, 71.2, 58.4, 54.8, 78.5,
                              52.4, 82.1, 84.5, 82.8, 56.2,
                              72.4, 85.2, 78.9, 81.5, 83.4,
                              65.8, 48.5, 52.1, 58.4, 54.2),
  graduation_rate = c(82.5, 89.2, 78.4, 76.5, 92.8,
                       74.2, 94.5, 96.2, 95.8, 79.5,
                       91.2, 97.1, 93.5, 94.8, 96.5,
                       86.5, 72.8, 78.5, 81.2, 77.5),
  college_readiness_pct = c(42.5, 52.4, 38.2, 35.8, 68.5,
                             32.5, 75.2, 72.8, 70.5, 38.5,
                             55.8, 74.5, 65.2, 68.4, 71.2,
                             45.2, 28.5, 32.8, 38.5, 34.2),
  
  # School Resources
  per_pupil_spending = c(12450, 11850, 11520, 10980, 13250,
                          10650, 12850, 13450, 12680, 10850,
                          11980, 13120, 12450, 12580, 13050,
                          11250, 10450, 10280, 10650, 10180),
  student_teacher_ratio = c(16.2, 15.5, 17.8, 18.2, 14.5,
                             17.5, 15.2, 14.8, 15.5, 17.2,
                             15.8, 14.5, 15.2, 14.8, 14.2,
                             16.5, 18.5, 17.8, 17.2, 18.8),
  
  # Educational Attainment (Adult Population 25+)
  pct_high_school = c(88.2, 92.5, 89.4, 85.8, 95.8,
                       87.5, 96.8, 97.2, 98.1, 88.5,
                       92.8, 97.5, 95.2, 96.8, 97.5,
                       90.5, 78.5, 86.2, 88.5, 86.8),
  pct_bachelors = c(28.5, 32.4, 24.8, 22.5, 58.2,
                     21.8, 62.5, 52.8, 48.5, 22.8,
                     32.5, 54.2, 42.8, 45.2, 48.5,
                     24.5, 15.8, 18.2, 20.5, 18.8),
  pct_graduate_degree = c(10.2, 12.5, 8.5, 7.2, 32.5,
                           7.8, 35.2, 22.5, 18.5, 7.5,
                           11.8, 24.5, 18.2, 16.5, 19.8,
                           8.5, 5.2, 6.5, 7.2, 6.8),
  
  # Higher Education Presence
  has_university = c(TRUE, FALSE, FALSE, FALSE, TRUE,
                     FALSE, TRUE, FALSE, FALSE, FALSE,
                     TRUE, FALSE, TRUE, FALSE, FALSE,
                     FALSE, FALSE, FALSE, FALSE, FALSE),
  has_community_college = c(TRUE, TRUE, TRUE, TRUE, TRUE,
                             TRUE, FALSE, TRUE, TRUE, TRUE,
                             TRUE, FALSE, FALSE, TRUE, FALSE,
                             TRUE, TRUE, TRUE, TRUE, TRUE),
  university_name = c("Drake University", NA, NA, NA, "University of Iowa",
                      NA, "Iowa State University", NA, NA, NA,
                      "University of Dubuque", NA, "UNI", NA, NA,
                      NA, NA, NA, NA, NA)
)

# Save raw education data
write_csv(iowa_education, here("data/raw/iowa_education_data.csv"))
cat("Saved education data to: data/raw/iowa_education_data.csv\n\n")

# =============================================================================
# School District Performance Analysis
# =============================================================================

cat("=== School District Performance ===\n\n")

# Performance summary
performance_summary <- iowa_education %>%
  summarise(
    avg_assessment = mean(state_assessment_score),
    avg_graduation = mean(graduation_rate),
    avg_college_ready = mean(college_readiness_pct),
    top_performers = sum(state_assessment_score >= 80),
    struggling = sum(state_assessment_score < 55)
  )

cat("School Performance Statistics:\n")
print(performance_summary)

# Create performance categories
iowa_education <- iowa_education %>%
  mutate(
    performance_tier = case_when(
      state_assessment_score >= 80 ~ "Excellent",
      state_assessment_score >= 70 ~ "Above Average",
      state_assessment_score >= 60 ~ "Average",
      state_assessment_score >= 50 ~ "Below Average",
      TRUE ~ "Needs Improvement"
    ),
    performance_tier = factor(performance_tier,
                               levels = c("Needs Improvement", "Below Average", 
                                         "Average", "Above Average", "Excellent"))
  )

cat("\nTop 5 School Districts by Assessment Scores:\n")
iowa_education %>%
  arrange(desc(state_assessment_score)) %>%
  select(school_district, state_assessment_score, graduation_rate, college_readiness_pct) %>%
  head(5) %>%
  print()

cat("\nSchool Districts Needing Support:\n")
iowa_education %>%
  filter(state_assessment_score < 55) %>%
  select(school_district, state_assessment_score, graduation_rate) %>%
  print()

# =============================================================================
# Graduation Rate Analysis
# =============================================================================

cat("\n=== Graduation Rate Analysis ===\n\n")

grad_summary <- iowa_education %>%
  summarise(
    avg_grad_rate = mean(graduation_rate),
    min_grad_rate = min(graduation_rate),
    max_grad_rate = max(graduation_rate),
    above_90_pct = sum(graduation_rate >= 90),
    below_80_pct = sum(graduation_rate < 80)
  )

cat("Graduation Rate Statistics:\n")
cat("Average graduation rate:", round(grad_summary$avg_grad_rate, 1), "%\n")
cat("Districts with 90%+ graduation:", grad_summary$above_90_pct, "\n")
cat("Districts below 80% graduation:", grad_summary$below_80_pct, "\n")

# =============================================================================
# Educational Attainment Analysis
# =============================================================================

cat("\n=== Educational Attainment (Adult Population) ===\n\n")

attainment_summary <- iowa_education %>%
  summarise(
    avg_hs_diploma = mean(pct_high_school),
    avg_bachelors = mean(pct_bachelors),
    avg_graduate = mean(pct_graduate_degree),
    most_educated_city = city[which.max(pct_bachelors)]
  )

cat("Adult Educational Attainment:\n")
cat("Average with HS diploma:", round(attainment_summary$avg_hs_diploma, 1), "%\n")
cat("Average with Bachelor's:", round(attainment_summary$avg_bachelors, 1), "%\n")
cat("Average with Graduate degree:", round(attainment_summary$avg_graduate, 1), "%\n")
cat("Most educated city:", attainment_summary$most_educated_city, "\n")

# Cities with highest education levels
cat("\nTop 5 Cities by Bachelor's Degree Attainment:\n")
iowa_education %>%
  arrange(desc(pct_bachelors)) %>%
  select(city, pct_bachelors, pct_graduate_degree, has_university) %>%
  head(5) %>%
  print()

# =============================================================================
# School Resources Analysis
# =============================================================================

cat("\n=== School Resources Analysis ===\n\n")

# Per-pupil spending analysis
spending_summary <- iowa_education %>%
  summarise(
    avg_spending = mean(per_pupil_spending),
    min_spending = min(per_pupil_spending),
    max_spending = max(per_pupil_spending),
    spending_gap = max_spending - min_spending
  )

cat("Per-Pupil Spending:\n")
cat("Average:", dollar(spending_summary$avg_spending), "\n")
cat("Range:", dollar(spending_summary$min_spending), "-", 
    dollar(spending_summary$max_spending), "\n")
cat("Funding gap:", dollar(spending_summary$spending_gap), "\n")

# Correlation between spending and performance
spending_corr <- cor(iowa_education$per_pupil_spending, 
                     iowa_education$state_assessment_score)
cat("\nCorrelation between spending and performance:", round(spending_corr, 3), "\n")

# =============================================================================
# Higher Education Analysis
# =============================================================================
  
cat("\n=== Higher Education Presence ===\n\n")

higher_ed <- iowa_education %>%
  summarise(
    cities_with_university = sum(has_university),
    cities_with_cc = sum(has_community_college),
    both = sum(has_university & has_community_college)
  )

cat("Higher Education Access:\n")
cat("Cities with universities:", higher_ed$cities_with_university, "\n")
cat("Cities with community colleges:", higher_ed$cities_with_cc, "\n")

cat("\nUniversity Cities:\n")
iowa_education %>%
  filter(has_university) %>%
  select(city, university_name, pct_bachelors) %>%
  print()

# =============================================================================
# Visualizations
# =============================================================================

cat("\n=== Creating Education Visualizations ===\n\n")

# 1. School Performance Comparison
p1 <- iowa_education %>%
  ggplot(aes(x = reorder(school_district, state_assessment_score), 
             y = state_assessment_score,
             fill = performance_tier)) +
  geom_col() +
  geom_hline(yintercept = mean(iowa_education$state_assessment_score), 
             linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "Iowa School District Performance",
    subtitle = "State Assessment Scores (Red line = state average)",
    x = NULL,
    y = "Assessment Score",
    fill = "Performance Tier"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
ggsave(here("outputs/iowa_school_performance.png"), p1, width = 10, height = 10, dpi = 300)

# 2. Graduation Rates
p2 <- iowa_education %>%
  ggplot(aes(x = reorder(city, graduation_rate), 
             y = graduation_rate,
             fill = graduation_rate >= 90)) +
  geom_col() +
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkgreen") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#fc8d59", "TRUE" = "#91cf60"),
                     labels = c("Below 90%", "90% or higher")) +
  labs(
    title = "High School Graduation Rates",
    subtitle = "Green line = 90% benchmark",
    x = NULL,
    y = "Graduation Rate (%)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)
ggsave(here("outputs/iowa_graduation_rates.png"), p2, width = 10, height = 8, dpi = 300)

# 3. Educational Attainment by City
p3 <- iowa_education %>%
  select(city, pct_high_school, pct_bachelors, pct_graduate_degree) %>%
  pivot_longer(-city, names_to = "level", values_to = "percentage") %>%
  mutate(level = factor(level, 
                        levels = c("pct_high_school", "pct_bachelors", "pct_graduate_degree"),
                        labels = c("High School", "Bachelor's", "Graduate Degree"))) %>%
  ggplot(aes(x = reorder(city, percentage), y = percentage, fill = level)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Educational Attainment in Iowa Cities",
    subtitle = "Percentage of adults 25+ with degree",
    x = NULL,
    y = "Percentage (%)",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)
ggsave(here("outputs/iowa_educational_attainment.png"), p3, width = 12, height = 10, dpi = 300)

# 4. Spending vs Performance
p4 <- iowa_education %>%
  ggplot(aes(x = per_pupil_spending, y = state_assessment_score)) +
  geom_point(aes(size = total_students, color = performance_tier), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", linetype = "dashed") +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = dollar) +
  scale_size_continuous(labels = comma, range = c(3, 15)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "School Spending vs Academic Performance",
    subtitle = paste("Correlation:", round(spending_corr, 2)),
    x = "Per-Pupil Spending",
    y = "Assessment Score",
    size = "Students",
    color = "Performance"
  ) +
  theme_minimal()

print(p4)
ggsave(here("outputs/iowa_spending_performance.png"), p4, width = 12, height = 8, dpi = 300)

# 5. College Readiness
p5 <- iowa_education %>%
  ggplot(aes(x = reorder(city, college_readiness_pct), 
             y = college_readiness_pct,
             fill = has_university)) +
  geom_col() +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkgreen"),
                     labels = c("No University", "Has University")) +
  labs(
    title = "College Readiness Rates",
    subtitle = "Percentage meeting college readiness benchmarks",
    x = NULL,
    y = "College Ready (%)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p5)
ggsave(here("outputs/iowa_college_readiness.png"), p5, width = 10, height = 8, dpi = 300)

# =============================================================================
# Education Quality Index
# =============================================================================

cat("\n=== Education Quality Index ===\n\n")

iowa_education <- iowa_education %>%
  mutate(
    # Normalize metrics
    assessment_norm = (state_assessment_score - min(state_assessment_score)) /
                      (max(state_assessment_score) - min(state_assessment_score)) * 100,
    grad_norm = (graduation_rate - min(graduation_rate)) /
                (max(graduation_rate) - min(graduation_rate)) * 100,
    college_norm = (college_readiness_pct - min(college_readiness_pct)) /
                   (max(college_readiness_pct) - min(college_readiness_pct)) * 100,
    spending_norm = (per_pupil_spending - min(per_pupil_spending)) /
                    (max(per_pupil_spending) - min(per_pupil_spending)) * 100,
    
    # Composite index
    education_quality_index = (
      assessment_norm * 0.35 +
      grad_norm * 0.30 +
      college_norm * 0.25 +
      spending_norm * 0.10
    )
  )

cat("Top 10 Cities by Education Quality Index:\n")
iowa_education %>%
  arrange(desc(education_quality_index)) %>%
  select(city, education_quality_index, state_assessment_score, 
         graduation_rate, college_readiness_pct) %>%
  head(10) %>%
  mutate(education_quality_index = round(education_quality_index, 1)) %>%
  print()

# =============================================================================
# Save Processed Data
# =============================================================================

write_csv(iowa_education, here("data/processed/iowa_education_analyzed.csv"))
cat("\n✓ Education analysis data saved to: data/processed/iowa_education_analyzed.csv\n")

cat("\n=== Education Analysis Complete ===\n")
cat("Visualizations saved to outputs/ folder\n")
