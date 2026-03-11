#=============================================================================
# STEP 4: DATA CLEANING AND MANIPULATION FOR ANALYTICS AND DASHBOARDING
#=============================================================================
# PURPOSE:  1. Clean the full 10,324 row dataset and engineer all analytical
#           features needed by the data quality report and the dashboard.
#           2. Then load the result into a local SQLite database (file) with 6 indexes,
#           mirroring the partitioning strategy used in BigQuery ecosytem.
#           3. This database will serve as live data source for our detailed dashbaord.
#
# NEW FEATURES CREATED:
#   1. cost_per_kg: freight_cost_usd / weight_kg  (key required metric)
#   2. delivery_status: "On-Time" | "Late" | "Unknown"  (from date difference)
#   3. days_variance: delivered_date − scheduled_date  (negative means early)
#   4. freight_category: "Numeric" | "Included" | "Cross-reference" | "Missing"
#   5. freight_outlier_flag: 1 if above IQR upper fence
#   6. shipment_mode_clean: normalisation mode for "N/A" recoded to NA
#
# INPUT   : data/raw/SCMS_Delivery_History_Dataset.csv
# OUTPUTS : data/processed/cleaned_shipments.csv
#           db/shipments.sqlite  (with 6 indexes)
#
# RUN     : source("scripts/04_transform_and_model.R")
# NEXT    : rmarkdown::render("reports/05_data_science_report.Rmd")
# =============================================================================

#Loading the libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(DBI)
library(RSQLite)

cat("  SFH Rwanda — Health Logistics Pipeline: Step 4 of 6\n")
cat("  Feature Engineering & SQLite Load\n")
# =============================================================================
# STEP 1: LOAD RAW DATA
# =============================================================================
raw <- read_csv(
  "data/raw/SCMS_Delivery_History_Dataset.csv",
  locale         = locale(encoding = "latin1"),
  show_col_types = FALSE) %>% janitor::clean_names()

cat(sprintf("Data is loaded with %d rows and %d columns\n\n", nrow(raw), ncol(raw)))

# =============================================================================
# STEP 2: FEATURE ENGINEERING
# =============================================================================
# Helper Function to classify a raw freight cost string

classify_freight <- function(x) {
  case_when(
    str_detect(x, "^\\d")                                ~ "Numeric",
    str_detect(x, "Freight Included|Invoiced Separately") ~ "Included",
    str_detect(x, "^See ")                               ~ "Cross-reference",
    is.na(x) | x == ""                                   ~ "Missing",
    TRUE                                                  ~ "Other"
  )
}

cleaned <- raw %>%
  mutate(
  # ── Dates (format in CSV: "2-Jun-06") ────────────────────────────────────
    scheduled_date = dmy(scheduled_delivery_date),
    delivered_date = dmy(delivered_to_client_date),
    recorded_date  = dmy(delivery_recorded_date),
    year           = year(scheduled_date),
    month          = month(scheduled_date),
    year_month     = format(scheduled_date, "%Y-%m"),
    
    # ── Freight cost — three-category treatment ───────────────────────────────
    freight_category = classify_freight(freight_cost_usd),
    
    freight_cost_usd = case_when(
      freight_category == "Numeric"  ~
        suppressWarnings(as.numeric(freight_cost_usd)),
      freight_category == "Included" ~ 0,    # cost is real but embedded
      TRUE                           ~ NA_real_
    ),
    
    freight_flag = freight_category != "Numeric",  # TRUE = non-standard value
    
    # ── Weight ────────────────────────────────────────────────────────────────
    weight_kg = case_when(
      str_detect(weight_kilograms, "^\\d") ~
        suppressWarnings(as.numeric(weight_kilograms)),
      TRUE ~ NA_real_
    ),
    
    # ── FEATURE: Cost per Kilogram ────────────────────────────────────────────
    # Only defined where freight is a real numeric cost AND weight is known > 0.
    # This is the primary derived metric used in all analyses.
    cost_per_kg = if_else(
      freight_category == "Numeric" & !is.na(weight_kg) & weight_kg > 0,
      freight_cost_usd / weight_kg,
      NA_real_
    ),
    
    # ── FEATURE: Delivery Status — Late vs On-Time ────────────────────────────
    # days_variance > 0 → arrived AFTER scheduled date → Late
    # days_variance ≤ 0 → arrived on or before scheduled date → On-Time
    days_variance   = as.numeric(delivered_date - scheduled_date),
    delivery_status = case_when(
      is.na(scheduled_date) | is.na(delivered_date) ~ "Unknown",
      days_variance <= 0                            ~ "On-Time",
      TRUE                                          ~ "Late"
    ),
    
    # ── FEATURE: Normalised Shipment Mode ─────────────────────────────────────
    # The string "N/A" is replaced with true NA so it does not appear as a
    # spurious category in group_by() operations (see Issue #3 in script 02).
    shipment_mode_clean = case_when(
     shipment_mode == "Air"         ~ "Air",
     shipment_mode == "Air Charter" ~ "Air Charter",
     shipment_mode == "Truck"       ~ "Truck",
     shipment_mode == "Ocean"       ~ "Ocean",
      TRUE                             ~ NA_character_   # "N/A" → true NA
    ),
    
    # ── FEATURE: IQR Outlier Flag ─────────────────────────────────────────────
    # Identifies numeric freight values that exceed Q3 + 1.5*IQR.
    # Analyses use freight_outlier_flag = 0 to exclude these from averages.
    freight_outlier_flag = if_else(
      freight_category == "Numeric" & !is.na(freight_cost_usd),
      freight_cost_usd > (
        quantile(freight_cost_usd[freight_category == "Numeric"], 0.75, na.rm = TRUE) +
          1.5 * IQR(freight_cost_usd[freight_category == "Numeric"], na.rm = TRUE)
      ),
      FALSE
    ),
    
    # ── Other useful columns ──────────────────────────────────────────────────
    line_item_value = suppressWarnings(as.numeric(line_item_value))
  )

# Report what was created
cat(sprintf("  Rows cleaned: %d\n",   nrow(cleaned)))
cat(sprintf("  cost_per_kg valid: %d  (%.1f%%)\n",
            sum(!is.na(cleaned$cost_per_kg)),
            100 * mean(!is.na(cleaned$cost_per_kg))))
cat(sprintf("  delivery_status:\n"))
cat(sprintf("    On-Time  : %d\n", sum(cleaned$delivery_status == "On-Time")))
cat(sprintf("    Late     : %d\n", sum(cleaned$delivery_status == "Late")))
cat(sprintf("    Unknown  : %d\n", sum(cleaned$delivery_status == "Unknown")))
cat(sprintf("  Outlier records      : %d\n\n",
            sum(cleaned$freight_outlier_flag, na.rm = TRUE)))

# =============================================================================
# STEP 3: SELECT FINAL COLUMNS & SAVE CSV
# =============================================================================
cat("Step 3: Selecting final columns and saving CSV...\n")

final <- cleaned %>%
  transmute(
    id,
    project_code,
    country,
    managed_by,
    fulfill_via,
    vendor_inco_term,
    product_group,
    sub_class = sub_classification,
    vendor,
    shipment_mode,
    shipment_mode_clean,
    scheduled_date,
    delivered_date,
    recorded_date,
    year, month, year_month,
    weight_kg,
    freight_cost_usd,
    freight_category,
    freight_flag, 
    freight_outlier_flag,
    line_item_value,
    cost_per_kg,
    delivery_status,
    days_variance
  ) %>%
  filter(!is.na(country))   # drop the small number of rows with no country


cat(sprintf("Final rows has cols: %d  and rows: %d\n", nrow(final), ncol(final)))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(final, "data/processed/cleaned_shipments.csv")
cat("Saved: data/processed/cleaned_shipments.csv\n\n")

# =============================================================================
# STEP 4: LOAD INTO SQLITE WITH INDEXED SCHEMA
#   SQLite is used as a local reproducible database.
#   The schema and indexes mirror the partitioning and clustering strategy
#   We  would use in a cloud data warehouse like BigQuery, Redshift, and Snowflake.
#
#   For our case, we considered BigQuery Warehouse whose partioning equivalent would be:
#     PARTITION BY DATE_TRUNC(scheduled_date, YEAR)
#     CLUSTER BY country, shipment_mode_clean
# =============================================================================
cat("Step 4: Loading into SQLite database...\n\n")

dir.create("db", showWarnings = FALSE)
db_path <- "db/shipments.sqlite"
con     <- dbConnect(SQLite(), db_path)

dbExecute(con, "DROP TABLE IF EXISTS shipments;") #To be clear if the DB is empty

# ── Optimised schema ──────────────────────────────────────────────────────────

dbExecute(con, "
CREATE TABLE shipments (
    id                    TEXT,
    project_code          TEXT,
    country               TEXT     NOT NULL,  -- R02: never blank
    managed_by            TEXT,
    fulfill_via           TEXT,
    vendor_inco_term      TEXT,
    product_group         TEXT,
    sub_class             TEXT,
    vendor                TEXT,
    shipment_mode         TEXT,
    shipment_mode_clean   TEXT,    -- normalised; 'N/A' recoded to NULL
    scheduled_date        TEXT,    -- ISO 'YYYY-MM-DD'
    delivered_date        TEXT,
    recorded_date         TEXT,
    year                  INTEGER, -- BigQuery PARTITION equivalent
    month                 INTEGER,
    year_month            TEXT,    -- 'YYYY-MM' for time-series grouping
    weight_kg             REAL,
    freight_cost_usd      REAL,
    freight_category      TEXT,
    freight_flag          INTEGER, -- 0/1 boolean
    freight_outlier_flag  INTEGER, -- 0/1 boolean
    line_item_value       REAL,
    cost_per_kg           REAL,    -- derived: freight_cost_usd / weight_kg
    delivery_status       TEXT,    -- 'On-Time' | 'Late' | 'Unknown'
    days_variance         REAL     -- delivered − scheduled (days)
);")

# ── Write data ─────────────────────────────────────────────────────────────────

final_db <- final %>%
  mutate(
    across(c(scheduled_date, 
             delivered_date, 
             recorded_date), as.character),
    freight_flag         = as.integer(freight_flag),
    freight_outlier_flag = as.integer(freight_outlier_flag)
  )

dbWriteTable(con, "shipments", final_db, append = TRUE)

# ── Create 6 indexes ──────────────────────────────────────────────────────────
# In BigQuery / Redshift these columns would be partition / sort keys.
# Index on the most common WHERE and GROUP BY columns.

indexes <- c(
  "CREATE INDEX idx_country       ON shipments(country);",
  "CREATE INDEX idx_year_month    ON shipments(year_month);",
  "CREATE INDEX idx_delivery_stat ON shipments(delivery_status);",
  "CREATE INDEX idx_mode_clean    ON shipments(shipment_mode_clean);",
  "CREATE INDEX idx_product_group ON shipments(product_group);",
  "CREATE INDEX idx_country_year  ON shipments(country, year_month);"   # composite
)

for (idx in indexes) {
  dbExecute(con, gsub("CREATE INDEX", "CREATE INDEX IF NOT EXISTS", idx))
}



# ── Verification queries ────────────────────────────────────────────────────

cat("  Top 5 countries by avg freight cost (SQL verification):\n")

q1 <- dbGetQuery(con, "
  SELECT country,
         COUNT(*)                         AS n,
         ROUND(AVG(freight_cost_usd), 0)  AS avg_freight_usd
  FROM   shipments
  WHERE  freight_category     = 'Numeric'
    AND  freight_outlier_flag = 0
    AND  freight_cost_usd     > 0
  GROUP  BY country
  ORDER  BY avg_freight_usd DESC
  LIMIT  5;")

print(q1) #It turns the table of top countries with their respective average flight cost in USD

cat("\n  Annual on-time delivery rate (SQL verification):\n")

q2 <- dbGetQuery(con, "
  SELECT year,
         COUNT(*) AS total,
         ROUND(100.0 * SUM(CASE WHEN delivery_status='On-Time' THEN 1 ELSE 0 END)
               / COUNT(*), 1) AS on_time_pct
  FROM   shipments
  WHERE  delivery_status != 'Unknown' AND year IS NOT NULL
  GROUP  BY year ORDER BY year;")

print(q2)

row_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM shipments")$n
cat(sprintf("\n  Rows in SQLite: %d | Indexes: %d | File: %s\n",
            row_n, length(indexes), db_path))

dbDisconnect(con)

cat("\n================================================================\n")
cat("  Step 4 complete.\n\n")
cat("  Run next:\n")
cat("    rmarkdown::render('reports/05_data_science_report.Rmd',\n")
cat("                      output_file = 'reports/05_data_science_report.html')\n")
cat("================================================================\n")