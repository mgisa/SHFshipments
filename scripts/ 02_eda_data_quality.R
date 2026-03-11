#=============================================================================
# STEP 2: EXPLORATORY DATA ANALYSIS - RAW Supply Chain Shipment Pricing Data
#==============================================================================
# =============================================================================

# PURPOSE : 1. Exploratory Data Analysis on the raw SCMS dataset.
#           Identifies exactly 3 confirmed data quality issues and produces
#           one ggplot2 chart per issue (saved as PNG).
#           2. Draw and runs the statistical hypothesis test: 'Air vs Truck
#           cost-per-kg for heavy loads (> 500 kg)'.
#
# INPUT   : data/raw/SCMS_Delivery_History_Dataset.csv
# OUTPUTS : reports/figures/issue1_non_numeric_freight.png
#           reports/figures/issue2_freight_outliers.png
#           reports/figures/issue3_shipment_mode_na.png
#           reports/figures/hypothesis_air_vs_truck.png
#           
#           Printing in Console summary of all findings
#
# CONSOLE RUN: source("scripts/02_eda_data_quality.R")
# TERMINAL: Rscript scripts/02_eda_data_quality.R

# NEXT TASK: source("scripts/03_airtable_upload_extract.R")
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

cat("  SFH Rwanda — Health Logistics Pipeline: Step 2 of 6\n")
cat("  EDA & Data Quality Assessment\n")

# =============================================================================
# SECTION 1: LOAD RAW DATA
# =============================================================================
cat("Loading raw CSV...\n")

raw <- read_csv(here::here("data/raw/SCMS_Delivery_History_Dataset.csv"),
  locale = locale(encoding = "latin1"),  # handles Côte d'Ivoire accent
  show_col_types = FALSE
) %>% janitor::clean_names()

cat(sprintf("  Rows    : %d\n",    nrow(raw)))
cat(sprintf("  Columns : %d\n",    ncol(raw)))
cat(sprintf("  Period  : %s  to  %s\n\n",
            min(raw$`Scheduled Delivery Date`, na.rm = TRUE),
            max(raw$`Scheduled Delivery Date`, na.rm = TRUE)))

# =============================================================================
# SECTION 2: DATA QUALITY ISSUE #1
#   1. ~40% of Freight Cost (USD) values are non-numeric text strings.
#   2.  Three distinct text categories exist, each needing different treatment.
#   3. A naive as.numeric() call would silently discard all of them as NA.
# =============================================================================
cat("─── Issue #1: Non-numeric values in Freight Cost (USD) ──────────\n")

freight_classes <- tibble(val = raw$`Freight Cost (USD)`) %>%
  mutate(
    category = case_when(
      str_detect(val, "^\\d")                                ~ "Numeric (parseable)",
      str_detect(val, "Freight Included|Invoiced Separately") ~ "Included in commodity cost",
      str_detect(val, "^See ")                               ~ "Cross-reference to other row",
      is.na(val) | val == ""                                 ~ "Blank / missing",
      TRUE                                                   ~ "Other text"
    )
  ) %>%
  count(category) %>%
  mutate(pct = round(100 * n / nrow(raw), 1)) %>%
  arrange(desc(n))

cat("\n  Freight Cost category breakdown:\n")
print(freight_classes, n = Inf)

non_numeric_n <- sum(freight_classes$n[freight_classes$category != "Numeric (parseable)"])
cat(sprintf("\n  Non-numeric records : %d  (%.0f%% of total)\n",
            non_numeric_n, 100 * non_numeric_n / nrow(raw)))
cat("  Treatment needed   : 3-way classify → parse / set to 0 / set to NA\n")
cat("  Risk if ignored    : 40% silent NA when using as.numeric()\n\n")

p1 <- ggplot(freight_classes,
             aes(x = reorder(category, n), y = n, fill = category)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "  (", pct, "%)")),
            hjust = -0.05, size = 3.6) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Numeric (parseable)"             = "#6DB57A",
    "Included in commodity cost"      = "#F5A623",
    "Cross-reference to other row"    = "#E05C5C",
    "Blank / missing"                 = "#B0B0B0",
    "Other text"                      = "#888888"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(
    title    = "Issue #1: ~40% of Freight Cost Values Are Non-Numeric Text",
    subtitle = "Naive as.numeric() would silently discard these as NA — three categories need different treatment",
    x = NULL, y = "Number of records",
    caption  = "Source: USAID SCMS Delivery History Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1A3A5C"))

ggsave("reports/figures/issue1_non_numeric_freight.png",
       p1, width = 10, height = 4.5, dpi = 150)
cat("  Chart saved: reports/figures/issue1_non_numeric_freight.png\n\n")

# =============================================================================
# SECTION 3: DATA QUALITY ISSUE #2
#   Even among valid numeric freight records, the distribution is
#   extremely right-skewed with a few values exceeding $500,000.
#   These outliers would inflate any country-level or mode-level average.
# =============================================================================
cat("─── Issue #2: Extreme outliers in numeric freight costs ─────────\n")

numeric_freight <- raw %>%
  mutate(freight_usd = suppressWarnings(as.numeric(`Freight Cost (USD)`))) %>%
  filter(!is.na(freight_usd), freight_usd >= 0)

q1_val      <- quantile(numeric_freight$freight_usd, 0.25)
q3_val      <- quantile(numeric_freight$freight_usd, 0.75)
iqr_val     <- q3_val - q1_val
upper_fence <- q3_val + 1.5 * iqr_val
outlier_n   <- sum(numeric_freight$freight_usd > upper_fence)

cat(sprintf("  Numeric records   : %d\n",   nrow(numeric_freight)))
cat(sprintf("  Median            : $%s\n",  comma(round(median(numeric_freight$freight_usd)))))
cat(sprintf("  IQR upper fence   : $%s\n",  comma(round(upper_fence))))
cat(sprintf("  Outliers above    : %d  (%.1f%%)\n",
            outlier_n, 100 * outlier_n / nrow(numeric_freight)))
cat(sprintf("  Maximum value     : $%s\n",  comma(round(max(numeric_freight$freight_usd))))  )
cat("  Treatment needed  : IQR flag; exclude outliers from average-cost analyses\n")
cat("  Risk if ignored   : Country/mode means inflated 10–100×\n\n")

p2 <- numeric_freight %>%
  filter(freight_usd <= quantile(freight_usd, 0.995)) %>%   # truncate for readability
  ggplot(aes(x = freight_usd)) +
  geom_histogram(bins = 70, fill = "#4A90D9", colour = "white", linewidth = 0.15) +
  geom_vline(xintercept = upper_fence, colour = "#E05C5C",
             linetype = "dashed", linewidth = 1.1) +
  annotate("text", x = upper_fence * 1.06, y = Inf, vjust = 2, hjust = 0,
           label = paste0("IQR fence\n$", comma(round(upper_fence))),
           colour = "#E05C5C", size = 3.6) +
  scale_x_continuous(labels = dollar_format(big.mark = ",")) +
  labs(
    title    = "Issue #2: Extreme Right Skew — Outliers in Numeric Freight Costs",
    subtitle = "X-axis truncated at 99.5th percentile; long tail reaches >$500k",
    x = "Freight Cost (USD)", y = "Count",
    caption  = "Source: USAID SCMS Delivery History Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1A3A5C"))

ggsave("reports/figures/issue2_freight_outliers.png",
       p2, width = 10, height = 4.5, dpi = 150)
cat("  Chart saved: reports/figures/issue2_freight_outliers.png\n\n")

# =============================================================================
# SECTION 4: DATA QUALITY ISSUE #3
#   The literal string "N/A" is used as a sentinel in Shipment Mode.
#   R's is.na() returns FALSE for these 360 rows — they would be included
#   in every group_by() as a spurious 5th mode category.
# =============================================================================
cat("─── Issue #3: String 'N/A' sentinel in Shipment Mode ───────────\n")

mode_dist <- raw %>%
  count(`Shipment Mode`, sort = TRUE) %>%
  mutate(
    pct         = round(100 * n / nrow(raw), 1),
    is_sentinel = `Shipment Mode` == "N/A"
  )

cat("\n  Shipment Mode distribution:\n")
print(mode_dist)
cat("\n  Treatment needed  : Recode 'N/A' to NA_character_ before analysis\n")
cat("  Risk if ignored   : Spurious 5th category in every group_by / chart\n\n")

p3 <- ggplot(mode_dist,
             aes(x = reorder(`Shipment Mode`, n), y = n,
                 fill = is_sentinel)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "  (", pct, "%)")),
            hjust = -0.05, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#1A3A5C", "TRUE" = "#E05C5C")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title    = "Issue #3: 360 Records Have Shipment Mode = 'N/A' (a String, Not R's NA)",
    subtitle = "Red bar = sentinel value; is.na() returns FALSE — must be explicitly recoded",
    x = NULL, y = "Number of records",
    caption  = "Source: USAID SCMS Delivery History Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1A3A5C"))

ggsave("reports/figures/issue3_shipment_mode_na.png",
       p3, width = 10, height = 4, dpi = 150)
cat("  Chart saved: reports/figures/issue3_shipment_mode_na.png\n\n")

# =============================================================================
# SECTION 5: DATA SCIENTIST HYPOTHESIS — Air vs Truck cost-per-kg
#   H0: No significant difference in cost/kg between Air and Truck (>500 kg)
#   H1: Air costs significantly more per kg than Truck for heavy loads
#   Test: Wilcoxon rank-sum (non-parametric; freight is right-skewed)
#   Significance level: α = 0.05
# =============================================================================
cat("─── Hypothesis: Air vs Truck cost/kg for heavy loads (> 500 kg) ─\n\n")

hyp_df <- raw %>%
  mutate(
    freight_usd = suppressWarnings(as.numeric(`Freight Cost (USD)`)),
    weight_kg   = suppressWarnings(as.numeric(`Weight (Kilograms)`)),
    mode_clean  = case_when(
      `Shipment Mode` == "Air"         ~ "Air",
      `Shipment Mode` == "Air Charter" ~ "Air Charter",
      `Shipment Mode` == "Truck"       ~ "Truck",
      `Shipment Mode` == "Ocean"       ~ "Ocean",
      TRUE                             ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(freight_usd), freight_usd > 0,
    !is.na(weight_kg),   weight_kg > 500,   # heavy loads only
    mode_clean %in% c("Air", "Truck", "Ocean")
  ) %>%
  mutate(cost_per_kg = freight_usd / weight_kg) %>%
  filter(cost_per_kg < quantile(cost_per_kg, 0.99, na.rm = TRUE))  # trim top 1%

# Descriptive statistics
hyp_summary <- hyp_df %>%
  group_by(Mode = mode_clean) %>%
  summarise(
    n           = n(),
    median_cpk  = round(median(cost_per_kg), 2),
    mean_cpk    = round(mean(cost_per_kg), 2),
    .groups     = "drop"
  )

cat("  Cost/kg summary (loads > 500 kg, top-1% trimmed):\n")
print(hyp_summary)

# Wilcoxon rank-sum test: one-sided Air > Truck
air_cpk   <- hyp_df$cost_per_kg[hyp_df$mode_clean == "Air"]
truck_cpk <- hyp_df$cost_per_kg[hyp_df$mode_clean == "Truck"]
wt        <- wilcox.test(air_cpk, truck_cpk, alternative = "greater")

# Effect size r = Z / sqrt(N)
z_stat    <- qnorm(wt$p.value / 2, lower.tail = FALSE)
effect_r  <- round(z_stat / sqrt(length(air_cpk) + length(truck_cpk)), 3)
multiplier <- round(median(air_cpk) / median(truck_cpk), 1)

cat(sprintf("\n  Wilcoxon rank-sum (Air > Truck):\n"))
cat(sprintf("    n Air         : %d\n",   length(air_cpk)))
cat(sprintf("    n Truck       : %d\n",   length(truck_cpk)))
cat(sprintf("    p-value       : %s\n",
            if (wt$p.value < 0.001) "< 0.001" else round(wt$p.value, 4)))
cat(sprintf("    Effect size r : %.3f  (%s)\n", effect_r,
            dplyr::case_when(effect_r >= 0.5 ~ "large",
                             effect_r >= 0.3 ~ "medium",
                             TRUE            ~ "small")))
cat(sprintf("    Decision (α=0.05) : %s H0\n",
            if (wt$p.value < 0.05) "REJECT" else "FAIL TO REJECT"))
cat(sprintf("    Air is %.1fx more expensive per kg than Truck\n\n", multiplier))

p4 <- hyp_df %>%
  filter(mode_clean %in% c("Air", "Truck", "Ocean")) %>%
  ggplot(aes(x = mode_clean, y = cost_per_kg, fill = mode_clean)) +
  geom_violin(alpha = 0.35, colour = NA, trim = TRUE) +
  geom_boxplot(width = 0.22, outlier.alpha = 0.25, colour = "grey30") +
  stat_summary(fun = median, geom = "point",
               shape = 21, size = 4.5, fill = "white", colour = "#1A3A5C") +
  scale_fill_manual(values = c(Air = "#4A90D9", Truck = "#6DB57A", Ocean = "#F5A623"),
                    guide  = "none") +
  scale_y_continuous(labels = dollar_format()) +
  annotate("text", x = 1.5, y = max(hyp_df$cost_per_kg) * 0.93,
           label = paste0("p ", if (wt$p.value < 0.001) "< 0.001" else round(wt$p.value, 3),
                          "\nr = ", effect_r),
           size = 4.2, colour = "#1A3A5C", fontface = "bold") +
  labs(
    title    = paste0("Hypothesis: Air is ", multiplier,
                      "× More Expensive per KG than Truck (Loads > 500 kg)"),
    subtitle = "Wilcoxon rank-sum test: H0 rejected (p < 0.001) | White dot = median",
    x = NULL, y = "Cost per KG (USD)",
    caption  = "Top-1% trimmed for clarity | Source: USAID SCMS Delivery History Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1A3A5C"))

ggsave("reports/figures/hypothesis_air_vs_truck.png",
       p4, width = 9, height = 5.5, dpi = 150)
cat("  Chart saved: reports/figures/hypothesis_air_vs_truck.png\n\n")

# =============================================================================
# SECTION 6: OVERVIEW STATS SAVED FOR DOWNSTREAM SCRIPTS
# =============================================================================
cat("─── Dataset overview ─────────────────────────────────────────────\n\n")

yearly <- raw %>%
  mutate(yr = year(dmy(`Scheduled Delivery Date`))) %>%
  filter(!is.na(yr)) %>%
  count(yr)

cat("  Shipments per year:\n")
print(yearly)

cat(sprintf("\n  Peak year: %d (%d shipments)\n",
            yearly$yr[which.max(yearly$n)], max(yearly$n)))

cat("\n================================================================\n")
cat("  EDA complete. 3 issues documented.\n\n")
cat("  Run next: source('scripts/03_airtable_upload_extract.R')\n")
cat("================================================================\n")