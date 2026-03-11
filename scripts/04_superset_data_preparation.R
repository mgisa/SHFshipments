# =============================================================================
# 06_superset_export.R
# PURPOSE : Part 3 — Visualization & Storytelling (The "Impact" Stage)
#
#   Builds 4 ggplot2 charts that answer the 3 required dashboard questions,
#   exports a Preset.io / Apache Superset-ready CSV, and prints a
#   step-by-step guide for building the executive dashboard.
#
# THE THREE QUESTIONS ANSWERED
#   Q1  Supply Chain Health: which countries have the highest avg freight costs?
#   Q2  Operational Efficiency: on-time delivery trend — last 12 months
#   Q3  The Story: Big Number + Time-Series to convince a Director
#
# INPUT   : data/processed/cleaned_shipments.csv  (from script 04)
# OUTPUTS : data/processed/superset_export.csv
#           reports/figures/q1_country_freight.png
#           reports/figures/q2_ontime_trend.png
#           reports/figures/q3_big_number.png
#           reports/figures/q3_timeseries.png
#
# RUN     : source("scripts/06_superset_export.R")
#           (run AFTER script 04 and AFTER knitting the Rmd report)
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(glue)

dir.create("reports/figures",  recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed",   recursive = TRUE, showWarnings = FALSE)

cat("================================================================\n")
cat("  SFH Rwanda — Health Logistics Pipeline: Step 6 of 6\n")
cat("  Superset Export & Storytelling Charts\n")
cat("================================================================\n\n")

# =============================================================================
# STEP 1: LOAD CLEANED DATA
# =============================================================================
cat("Step 1: Loading cleaned data...\n")

df <- read_csv("data/processed/cleaned_shipments.csv", show_col_types = FALSE)
cat(sprintf("  Loaded: %d rows\n\n", nrow(df)))

# Shared plot theme
theme_story <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, colour = "#1A3A5C"),
      plot.subtitle = element_text(colour = "grey40", size = 11,
                                   margin = margin(b = 8)),
      plot.caption  = element_text(colour = "grey60", size = 9),
      panel.grid.minor = element_blank()
    )
}

# =============================================================================
# STEP 2: BUILD SUPERSET EXPORT CSV
#   Flat, analysis-ready file for Preset.io upload.
#   Pre-computed binary columns (is_on_time, is_air, is_truck) make
#   building Preset metrics trivial: AVG(is_on_time) * 100 = on-time rate %.
# =============================================================================
cat("Step 2: Building Preset.io export CSV...\n")

superset_df <- df %>%
  filter(!is.na(year)) %>%
  transmute(
    id, country, product_group,
    shipment_mode   = shipment_mode_clean,
    vendor          = if ("vendor" %in% names(df)) vendor else NA_character_,
    managed_by      = if ("managed_by" %in% names(df)) managed_by else NA_character_,
    fulfill_via     = if ("fulfill_via" %in% names(df)) fulfill_via else NA_character_,
    scheduled_date  = as.character(scheduled_date),
    delivered_date  = as.character(delivered_date),
    year, month, year_month,
    freight_cost_usd, weight_kg, cost_per_kg, freight_category,
    delivery_status, days_variance,
    line_item_value,
    # Binary helpers — allow AVG() to compute rates in Preset
    is_on_time = as.integer(delivery_status == "On-Time"),
    is_late    = as.integer(delivery_status == "Late"),
    is_air     = as.integer(shipment_mode_clean == "Air"),
    is_truck   = as.integer(shipment_mode_clean == "Truck")
  )

write_csv(superset_df, "data/processed/superset_export.csv")
cat(sprintf("  Saved: data/processed/superset_export.csv  (%d rows)\n\n",
            nrow(superset_df)))

# =============================================================================
# STEP 3: Q1 — SUPPLY CHAIN HEALTH
#   Which countries have the highest average freight costs?
# =============================================================================
cat("Step 3: Q1 — Supply Chain Health chart...\n")

q1_data <- df %>%
  filter(freight_category == "Numeric",
         !freight_outlier_flag,
         freight_cost_usd > 0) %>%
  group_by(country) %>%
  summarise(avg_freight = mean(freight_cost_usd, na.rm = TRUE),
            n_shipments = n(), .groups = "drop") %>%
  arrange(desc(avg_freight)) %>%
  slice_head(n = 15)

p_q1 <- ggplot(q1_data,
               aes(x = reorder(country, avg_freight), y = avg_freight)) +
  geom_col(aes(fill = avg_freight), show.legend = FALSE, width = 0.72) +
  geom_text(aes(label = dollar(round(avg_freight))),
            hjust = -0.07, size = 3.5, colour = "grey30") +
  coord_flip() +
  scale_fill_gradient(low = "#7EC8F5", high = "#1A3A5C") +
  scale_y_continuous(labels = dollar_format(),
                     expand = expansion(mult = c(0, 0.22))) +
  labs(
    title    = "Supply Chain Health: Top 15 Countries by Avg Freight Cost",
    subtitle = "Numeric costs only | IQR outliers excluded | 2006–2015",
    x = NULL, y = "Average Freight Cost (USD)",
    caption  = "Source: USAID SCMS Delivery History"
  ) +
  theme_story()

ggsave("reports/figures/q1_country_freight.png",
       p_q1, width = 11, height = 6, dpi = 150)
cat("  Saved: reports/figures/q1_country_freight.png\n\n")

# =============================================================================
# STEP 4: Q2 — OPERATIONAL EFFICIENCY
#   On-time delivery trend — last 12 months of available data
# =============================================================================
cat("Step 4: Q2 — Operational Efficiency chart...\n")

last_12 <- df %>%
  filter(delivery_status != "Unknown", !is.na(year_month)) %>%
  group_by(year_month) %>%
  summarise(total = n(),
            on_time = sum(delivery_status == "On-Time"),
            ontime_pct = round(100 * on_time / total, 1),
            .groups = "drop") %>%
  arrange(year_month) %>%
  tail(12)

p_q2 <- ggplot(last_12, aes(x = year_month, y = ontime_pct, group = 1)) +
  geom_area(fill = "#4A90D9", alpha = 0.10) +
  geom_line(colour = "#1A3A5C", linewidth = 1.4) +
  geom_point(aes(colour = ontime_pct < 70), size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c("FALSE" = "#6DB57A", "TRUE" = "#E05C5C")) +
  geom_hline(yintercept = 80, linetype = "dashed",
             colour = "grey50", linewidth = 0.9) +
  annotate("text", x = last_12$year_month[1], y = 82,
           label = "80% target", hjust = 0, size = 3.5, colour = "grey60") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title    = "Operational Efficiency: On-Time Delivery — Last 12 Months",
    subtitle = "Red points = months below 70% | Dashed line = 80% programme target",
    x = NULL, y = "On-Time Delivery Rate (%)",
    caption  = "Source: USAID SCMS Delivery History"
  ) +
  theme_story()

ggsave("reports/figures/q2_ontime_trend.png",
       p_q2, width = 11, height = 5, dpi = 150)
cat("  Saved: reports/figures/q2_ontime_trend.png\n\n")

# =============================================================================
# STEP 5: Q3 — THE STORY  (Director-level charts)
#   ONE BIG NUMBER CHART + ONE TIME-SERIES CHART
# =============================================================================
cat("Step 5: Q3 — Director's Story charts...\n")

# ── Compute headline metrics ───────────────────────────────────────────────────
air_cpk   <- median(df$cost_per_kg[df$shipment_mode_clean == "Air"],   na.rm = TRUE)
truck_cpk <- median(df$cost_per_kg[df$shipment_mode_clean == "Truck"], na.rm = TRUE)
mult      <- round(air_cpk / truck_cpk, 1)

air_ot    <- round(100 * mean(df$delivery_status[df$shipment_mode_clean == "Air"]   == "On-Time", na.rm = TRUE), 0)
truck_ot  <- round(100 * mean(df$delivery_status[df$shipment_mode_clean == "Truck"] == "On-Time", na.rm = TRUE), 0)

cat(sprintf("  Air median $/kg   : $%.2f\n", air_cpk))
cat(sprintf("  Truck median $/kg : $%.2f\n", truck_cpk))
cat(sprintf("  Multiplier        : %.1fx\n", mult))
cat(sprintf("  Air on-time       : %d%%\n",  air_ot))
cat(sprintf("  Truck on-time     : %d%%\n\n", truck_ot))

# ── Big Number Chart ───────────────────────────────────────────────────────────
p_bn <- ggplot() +
  annotate("rect", xmin=0, xmax=10, ymin=0, ymax=10, fill="#1A3A5C") +
  annotate("text", x=5, y=7.1, label=paste0(mult, "×"),
           colour="#F5A623", size=32, fontface="bold", hjust=0.5) +
  annotate("text", x=5, y=5.0,
           label="Air freight costs MORE per kg than Truck",
           colour="white", size=7, fontface="bold", hjust=0.5) +
  annotate("text", x=5, y=3.1,
           label=paste0(
             "Air on-time rate (", air_ot, "%)  ≈  Truck (", truck_ot, "%)\n",
             "The cost premium buys NO better delivery performance."
           ),
           colour="#7EC8F5", size=4.8, hjust=0.5, lineheight=1.5) +
  annotate("text", x=5, y=0.7,
           label="USAID SCMS Delivery History | 2006–2015 | 10,324 shipments",
           colour="grey60", size=3.2, hjust=0.5) +
  xlim(0,10) + ylim(0,10) +
  theme_void()

ggsave("reports/figures/q3_big_number.png",
       p_bn, width=7, height=7, dpi=150)
cat("  Saved: reports/figures/q3_big_number.png\n\n")

# ── Time-Series Chart ──────────────────────────────────────────────────────────
PALETTE <- c(Air="#4A90D9", Truck="#6DB57A", Ocean="#F5A623", "Air Charter"="#7EC8F5")

ts_cpk <- df %>%
  filter(!is.na(cost_per_kg), !is.na(year),
         !is.na(shipment_mode_clean),
         cost_per_kg < quantile(cost_per_kg, 0.99, na.rm=TRUE)) %>%
  group_by(year, Mode=shipment_mode_clean) %>%
  summarise(avg_cpk=mean(cost_per_kg, na.rm=TRUE), .groups="drop")

ts_ot <- df %>%
  filter(delivery_status != "Unknown", !is.na(year),
         !is.na(shipment_mode_clean)) %>%
  group_by(year, Mode=shipment_mode_clean) %>%
  summarise(ontime_pct=round(100*mean(delivery_status=="On-Time"),1),
            .groups="drop")

p_ts_top <- ggplot(ts_cpk, aes(x=year, y=avg_cpk, colour=Mode, group=Mode)) +
  geom_line(linewidth=1.6) + geom_point(size=3.2) +
  scale_colour_manual(values=PALETTE) +
  scale_y_continuous(labels=dollar_format()) +
  scale_x_continuous(breaks=2006:2015) +
  labs(
    title    = "The Director's Story: Cost per KG by Mode — 10-Year Trend",
    subtitle = paste0(
      "Air consistently costs more per kg across all 10 years. ",
      "For bulk non-urgent commodities, switching to Truck\n",
      "reduces freight spend without sacrificing delivery reliability."
    ),
    x=NULL, y="Avg Cost per KG (USD)", colour="Mode"
  ) +
  theme_story() + theme(legend.position="top")

p_ts_bot <- ggplot(ts_ot, aes(x=year, y=ontime_pct, colour=Mode, group=Mode)) +
  geom_line(linewidth=1.1, linetype="dashed") + geom_point(size=2.5) +
  scale_colour_manual(values=PALETTE, guide="none") +
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(0,100)) +
  scale_x_continuous(breaks=2006:2015) +
  labs(subtitle="On-time rate by mode — no consistent Air advantage",
       x="Year", y="On-Time Rate (%)") +
  theme_story()

# Stack the two panels
p_ts_combined <- gridExtra::grid.arrange(p_ts_top, p_ts_bot,
                                         nrow=2, heights=c(3, 1.6))
ggsave("reports/figures/q3_timeseries.png",
       p_ts_combined, width=12, height=8, dpi=150)
cat("  Saved: reports/figures/q3_timeseries.png\n\n")

# =============================================================================
# STEP 6: SETTING PRESET.IO
# Sign up on: https://preset.io (free account) then upload the prepared data for dashbaording
# =============================================================================
