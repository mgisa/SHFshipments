#=============================================================================
# STEP 1: PROJECT INNITIALIZATION AND ENVIRONMENT SETUP
#==============================================================================
# PURPOSE
#   Install all packages and lock their versions with renv so that anyone
#   cloning this repo gets the EXACT same environment.
#
# HOW TO RUN  1. Assume you have installed R and Rstudio
#             2. Open RStudio and go directly to console

#   source("scripts/01_project_setting.R")   ← Run 1: renv inits, R restarts
#   source("scripts/01_project_setting.R")   ← Run 2: packages install + lockfile
#
# WHY TWO RUNS?
#   renv::init() must restart R to activate the new library paths.
#   This is built-in renv behaviour — NOT an error.
#   The script detects which stage it is in and resumes automatically.
#
# ALTERNATIVELY FROM TERMINAL (no restart issue — run only once)
#   Rscript scripts/01_project_setting.R
# =============================================================================

cat("============================================================\n")
cat("  Health-Tech Logistics — Project Setup\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# STAGE DETECTION
# Stage 1: renv folder does NOT exist yet  → initialise renv
# Stage 2: renv folder exists, no lockfile → install packages + snapshot
# Complete: lockfile exists               → restore from lockfile
# ------------------------------------------------------------------
stage <- if (file.exists("renv.lock")) {
  "complete"
} else if (dir.exists("renv")) {
  "stage2"
} else {
  "stage1"
}

cat(sprintf("Detected stage : %s\n\n", stage))

# ==================================================================
# STAGE 1  — Initialise renv (R will restart after this)
# ==================================================================
if (stage == "stage1") {
  
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing renv from CRAN...\n")
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  
  cat("Initialising renv in bare mode...\n")
  cat(">> R WILL RESTART automatically after this line. <<\n")
  cat(">> SOURCE THIS SCRIPT ONCE MORE after the restart. <<\n\n")
  
  renv::init(bare = TRUE)
  # ── R session restarts here ──────────────────────────────────────
  # Script stops. On the next source() run, stage == "stage2"
}

# ==================================================================
# STAGE 2  — Install packages then snapshot
# ==================================================================
if (stage == "stage2") {
  
  cat("Stage 2: Installing project packages into renv library.\n")
  cat("First-time install may take 3-5 minutes.\n\n")
  
  # ---- Package list (grouped for readability) --------------------
  packages <- list(
    
    "Data wrangling" = c(
      "readr",       # fast CSV reading
      "dplyr",       # data manipulation (pipes, verbs)
      "tidyr",       # pivot tables (pivot_wider & pivot_longer)
      "lubridate",   # date parsing (dmy, ymd, ...)
      "stringr",     # string helpers (str_detect, str_remove)
      "purrr",        # functional tools (map, pmap)
      "janitor"
    ),
    
    "Visualisation" = c(
      "ggplot2",     # grammar of graphics
      "scales",      # axis formatters (dollar, percent)
      "ggthemes"     # additional ggplot themes
    ),
    
    "Shiny dashboard" = c(
      "shiny",           # web app framework
      "shinydashboard",  # sidebar + box layout
      "shinymanager",    # login / authentication screen
      "highcharter",     # interactive Highcharts.js plots
      "ggiraph",         # interactive ggplot2 (tooltips)
      "leaflet",         # interactive maps
      "DT"               # interactive data tables
    ),
    
    "Airtable (rairtable)" = c(
      "rairtable"    # tidyverse-friendly Airtable client
      # CRAN: https://cran.r-project.org/package=rairtable
      # Wraps the Airtable REST API with dplyr-style verbs
    ),
    
    "Database" = c(
      "DBI",         # database interface standard
      "RSQLite"      # SQLite driver (local reproducible DB)
    ),
    
    "Reporting" = c(
      "rmarkdown",   # knit Rmd to HTML / PDF
      "knitr",       # code-chunk engine for Rmd
      "gt",           # publication-quality tables
      "prettydoc",    #Formatting report
      "kableExtra"
    ),
    
    "Utilities" = c(
      "glue",        # string interpolation (glue("{var}"))
      "here",        # robust relative paths (here("data/raw/..."))
      "cli"          # coloured console messages
    )
  )
  
  all_pkgs <- unlist(packages, use.names = FALSE)
  n_total  <- length(all_pkgs)
  n_done   <- 0L
  
  for (grp in names(packages)) {
    cat(sprintf("  [ %s ]\n", grp))
    for (pkg in packages[[grp]]) {
      n_done <- n_done + 1L
      if (!requireNamespace(pkg, quietly = TRUE)) {
        cat(sprintf("    (%02d/%02d) Installing  %-20s ...", n_done, n_total, pkg))
        tryCatch({
          renv::install(pkg)
          cat(" done\n")
        }, error = function(e) {
          cat(sprintf(" FAILED — %s\n", conditionMessage(e)))
        })
      } else {
        cat(sprintf("    (%02d/%02d) Already OK  %s\n", n_done, n_total, pkg))
      }
    }
    cat("\n")
  }
  
  # ---- Snapshot: freeze package versions -------------------------
  cat("Creating renv.lock (freezes exact package versions)...\n")
  renv::snapshot(prompt = FALSE)
  cat("renv.lock created.\n")
  cat("Tip: Commit renv.lock to Git so collaborators run renv::restore()\n")
  cat("     to get the exact same versions.\n\n")
}

# ==================================================================
# COMPLETE  — lockfile exists; restore library (e.g. after git clone)
# ==================================================================
if (stage == "complete") {
  cat("renv.lock found. Checking library sync...\n\n")
  renv::restore(prompt = FALSE)
  cat("Library is in sync with lockfile.\n\n")
}

# ==================================================================
# ALL STAGES — verify folder structure
# ==================================================================
cat("Verifying project folder structure...\n")

dirs <- c("data/raw", "data/processed", "scripts", "reports", "db", "dashboard")

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("  Created  : %s/\n", d))
  } else {
    cat(sprintf("  OK       : %s/\n", d))
  }
}

# ==================================================================
# FINAL SUMMARY
# ==================================================================
cat("\n============================================================\n")
cat("  Setup Summary\n")
cat("------------------------------------------------------------\n")
cat(sprintf("  renv version : %s\n", packageVersion("renv")))
cat(sprintf("  renv.lock    : %s\n",
            if (file.exists("renv.lock")) "present  (commit to Git)" else "NOT YET CREATED"))
cat(sprintf("  R version    : %s\n", R.version$version.string))
cat(sprintf("  Working dir  : %s\n", getwd()))
cat("============================================================\n\n")

if (stage == "stage1") {
  cat("  !! R restarted — SOURCE THIS SCRIPT ONE MORE TIME !!\n\n")
} else {
  cat("  All done. Run the pipeline in this order:\n\n")
  cat("  1.  source('scripts/02_eda.R')\n")
  cat("  2.  source('scripts/03_data_validation_and_Airtable_loading.R')\n")
  cat("  3.  rmarkdown::render('reports/04_data_transformation_visualization.Rmd')\n")
  cat("  4.  shiny::runApp('dashboard/')\n\n")
}