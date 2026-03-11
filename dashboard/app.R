# =============================================================================
# dashboard/app.R — SFH Rwanda Health-Tech Logistics Dashboard
#
# DATA SOURCE
#   Reads exclusively from data/processed/cleaned_shipments.csv
#   No database, no Airtable, no external dependencies.
#   Deploy to shinyapps.io by including the CSV in the dashboard/ folder.
#
# DEPLOY TO SHINYAPPS.IO — 3 steps, no database, no Airtable needed
#   1. cp data/processed/cleaned_shipments.csv   dashboard/
#   2. mkdir -p dashboard/www/figures
#      cp reports/figures/*.png                  dashboard/www/figures/
#   3. rsconnect::deployApp("dashboard/")
#
#   The CSV and all PNG charts travel with the app bundle.
#   shinyapps.io will serve everything from the dashboard/ folder.
#
# TABS
#   1.    Overview          — 4 KPI cards, modexyr bar, mode pie, on-time trend
#   2.    Supply Chain Map  — leaflet map + interactive top-10 bar
#   3.    Efficiency        — on-time trend, status donut, days-variance histogram
#   4.    Director's Story  — big-number KPIs, cost/kg time-series, recommendation
#   5.    EDA Report        — inline figures from script 02 (Issue 1-3 + hypothesis)
#   6.    Data Explorer     — filtered table with CSV / Excel download
#   7.    Downloads         — download full cleaned dataset + HTML report
#
# LOGIN
#   admin   / admin123   (full access)
#   analyst / analyst123 (read-only)
# =============================================================================

# Libraries
library(shiny)
library(shinydashboard)
library(shinymanager)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
# FIX 1: ggiraph removed — caused C++ compile timeout AND crash on shinyapps.io
library(highcharter)
library(leaflet)
library(scales)
library(DT)
library(glue)
library(base64enc)

# Colour constants
NAVY  <- "#1A3A5C"
GREEN <- "#6DB57A"
RED   <- "#E05C5C"
AMBER <- "#F5A623"
BLUE  <- "#4A90D9"

PAL <- c(Air = BLUE, "Air Charter" = "#7EC8F5",
         Truck = GREEN, Ocean = AMBER, `N/A` = "#B0B0B0")

# =============================================================================
# 0.  LOAD DATA
# FIX 2: first path corrected — on shinyapps.io working dir IS dashboard/
#         "dashboard/cleaned_shipments.csv" would resolve to
#         "dashboard/dashboard/..." which does not exist.
# =============================================================================
csv_candidates <- c(
  "cleaned_shipments.csv",                  # shinyapps.io (working dir = dashboard/)
  "data/processed/cleaned_shipments.csv",   # local: running from project root
  "../data/processed/cleaned_shipments.csv" # local: running inside dashboard/
)

csv_path <- Filter(file.exists, csv_candidates)[1]
if (is.na(csv_path)) stop("cleaned_shipments.csv not found. Run script 03 first.")

df_raw <- read_csv(csv_path, show_col_types = FALSE)

if (!"shipment_mode_clean" %in% names(df_raw) && "shipment_mode" %in% names(df_raw)) {
  df_raw$shipment_mode_clean <- df_raw$shipment_mode
}

df_full <- df_raw %>%
  mutate(
    freight_cost_usd    = suppressWarnings(as.numeric(freight_cost_usd)),
    cost_per_kg         = suppressWarnings(as.numeric(cost_per_kg)),
    weight_kg           = suppressWarnings(as.numeric(weight_kg)),
    days_variance       = suppressWarnings(as.numeric(days_variance)),
    year                = suppressWarnings(as.integer(year)),
    delivery_status     = if_else(is.na(delivery_status), "Unknown", delivery_status),
    shipment_mode_clean = if_else(
      shipment_mode_clean %in% c("Air","Air Charter","Truck","Ocean"),
      shipment_mode_clean, NA_character_
    )
  )

message(sprintf("[CSV] Loaded %d rows x %d cols from %s",
                nrow(df_full), ncol(df_full), csv_path))

# Pre-compute summary frames
# FIX 3: isFALSE() is scalar-only — returns a single FALSE for any vector.
#         filter() with a length-1 logical drops ALL rows silently.
#         Replaced with the correct vectorised form: is.na() | == 0
df_country <- df_full %>%
  filter(!is.na(freight_cost_usd), freight_cost_usd > 0,
         !is.na(cost_per_kg),
         is.na(freight_outlier_flag) | freight_outlier_flag == 0) %>%
  group_by(country) %>%
  summarise(
    shipments   = n(),
    avg_freight = round(mean(freight_cost_usd, na.rm = TRUE), 0),
    avg_cpk     = round(mean(cost_per_kg,      na.rm = TRUE), 2),
    ontime_pct  = round(100 * mean(delivery_status == "On-Time"), 1),
    .groups     = "drop"
  ) %>%
  arrange(desc(avg_freight))

df_mode_trend <- df_full %>%
  filter(!is.na(cost_per_kg), !is.na(year),
         !is.na(shipment_mode_clean),
         cost_per_kg < quantile(cost_per_kg, 0.99, na.rm = TRUE)) %>%
  group_by(year, mode = shipment_mode_clean) %>%
  summarise(avg_cpk = round(mean(cost_per_kg, na.rm = TRUE), 2),
            n = n(), .groups = "drop")

df_ontime_yr <- df_full %>%
  filter(delivery_status != "Unknown", !is.na(year)) %>%
  group_by(year) %>%
  summarise(
    total      = n(),
    on_time    = sum(delivery_status == "On-Time"),
    ontime_pct = round(100 * on_time / total, 1),
    .groups    = "drop"
  )

df_ontime_monthly <- df_full %>%
  filter(delivery_status != "Unknown", !is.na(year_month)) %>%
  group_by(year_month) %>%
  summarise(
    total      = n(),
    on_time    = sum(delivery_status == "On-Time"),
    ontime_pct = round(100 * on_time / total, 1),
    .groups    = "drop"
  ) %>%
  arrange(year_month) %>%
  tail(24)

# Scalar KPIs
total_records   <- nrow(df_full)
total_countries <- n_distinct(df_full$country, na.rm = TRUE)
overall_ontime  <- round(100 * mean(df_full$delivery_status == "On-Time", na.rm = TRUE), 1)
avg_air_cpk     <- round(mean(df_full$cost_per_kg[df_full$shipment_mode_clean == "Air"],   na.rm = TRUE), 2)
avg_truck_cpk   <- round(mean(df_full$cost_per_kg[df_full$shipment_mode_clean == "Truck"], na.rm = TRUE), 2)
air_truck_ratio <- if (!is.na(avg_air_cpk) && !is.na(avg_truck_cpk) && avg_truck_cpk > 0)
  round(avg_air_cpk / avg_truck_cpk, 1) else NA_real_

# EDA figures — base64 encoded so they serve correctly on shinyapps.io
fig_dirs <- c("www/figures", "figures", "reports/figures", "../reports/figures")
fig_dir  <- {
  found <- Filter(dir.exists, fig_dirs)
  if (length(found) > 0) found[1] else NA_character_
}
fig_uri <- function(name) {
  if (is.na(fig_dir)) return(NULL)
  p <- file.path(fig_dir, name)
  if (!file.exists(p)) return(NULL)
  paste0("data:image/png;base64,", base64enc::base64encode(p))
}

# =============================================================================
# 1. LOGIN CREDENTIALS
# =============================================================================
credentials <- data.frame(
  user     = c("admin",    "analyst"),
  password = c("admin123", "analyst123"),
  role     = c("admin",    "analyst"),
  stringsAsFactors = FALSE
)

# =============================================================================
# 2.  UI HELPERS
# =============================================================================

kpi_card <- function(out_id, label, sub = NULL, col = NAVY) {
  div(
    style = paste0(
      "background:white;border-radius:10px;padding:18px 20px;margin-bottom:12px;",
      "box-shadow:0 2px 8px rgba(0,0,0,.08);text-align:center;"
    ),
    div(style = glue("font-size:2.2em;font-weight:700;color:{col};line-height:1.1;"),
        uiOutput(out_id)),
    div(style = "font-size:.86em;color:#555;margin-top:5px;", label),
    if (!is.null(sub))
      div(style = "font-size:.74em;color:#999;margin-top:3px;", sub)
  )
}

fig_box <- function(uri, caption_text) {
  if (!is.null(uri) && nchar(uri) > 30) {
    tags$div(
      tags$img(src = uri, style = "width:100%;border-radius:6px;"),
      tags$p(style = "font-size:.8em;color:grey;margin-top:4px;", caption_text)
    )
  } else {
    tags$div(
      style = "padding:20px;background:#f8f8f8;border-radius:6px;color:#888;",
      tags$em(paste0(
        "Chart not found. Copy reports/figures/ to dashboard/www/figures/ then redeploy. (",
        caption_text, ")"
      ))
    )
  }
}

# =============================================================================
# 3.  UI
# FIX 4: passphrase= added — shinymanager requires this on shinyapps.io to
#         encrypt the credential store. Without it: exit status 1 immediately.
# FIX 5: logo img removed — logo/sfh-logo.png was not in the deployed bundle.
#         A broken asset reference in the header can crash startup on shinyapps.io.
# FIX 11: duplicate "Countries:" line in sidebar removed.
# =============================================================================
ui <- secure_app(
  passphrase = "sfh_rwanda_2025",
  dashboardPage(
    skin = "blue",
    
    dashboardHeader(
      title     = "SFH Health Logistics Insights",
      titleWidth = 300
    ),
    
    dashboardSidebar(
      width = 230,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview",          tabName = "overview",   icon = icon("chart-bar")),
        menuItem("Supply Chain Map",  tabName = "map",        icon = icon("globe")),
        menuItem("Efficiency",        tabName = "efficiency", icon = icon("clock")),
        menuItem("Director's Story",  tabName = "story",      icon = icon("bullhorn")),
        menuItem("EDA Report",        tabName = "eda",        icon = icon("microscope")),
        menuItem("Data Explorer",     tabName = "explorer",   icon = icon("table")),
        menuItem("Downloads",         tabName = "downloads",  icon = icon("download"))
      ),
      hr(),
      div(
        style = "padding:8px 14px;font-size:.78em;color:#aaa;",
        div(style = "color:#7ec8f5;font-weight:600;margin-bottom:4px;", "Author: Murera Gisa"),
        div(style = "color:#7EF5A1;font-weight:400;margin-bottom:4px;",
            "Source: USAID SCMS 2006-2015"),
        paste("Records:",   format(total_records, big.mark = ",")), br(),
        paste("Countries:", total_countries), br(),   # FIX 11: was duplicated
        paste("Period: 2006 - 2015")
      )
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML(glue(
        "body,.content-wrapper,.right-side{{background:#F7F9FC!important;",
        "font-family:'Segoe UI',Arial,sans-serif;}}",
        ".main-header .logo,.main-header .navbar{{background:{NAVY}!important;}}",
        ".sidebar{{background:#1e2a38!important;}}",
        ".box{{border-top-color:{NAVY}!important;}}",
        ".btn-dl{{background:{NAVY};color:white;border:none;border-radius:6px;",
        "padding:10px 22px;font-size:1em;cursor:pointer;margin:6px 4px;}}",
        ".btn-dl:hover{{background:#2a5298;}}"
      )))),
      
      tabItems(
        
        # TAB 1: OVERVIEW
        tabItem("overview",
                fluidRow(
                  column(3, kpi_card("kpi_ontime",    "Overall On-Time Rate",       "All modes 2006-2015",        GREEN)),
                  column(3, kpi_card("kpi_total",     "Total Shipment Records",     "USAID SCMS 2006-2015",       NAVY)),
                  column(3, kpi_card("kpi_countries", "Countries Served",           "Across all product groups",  AMBER)),
                  column(3, kpi_card("kpi_ratio",     "Air vs Truck Cost/kg Ratio", "Air / Truck median $/kg",    RED))
                ),
                fluidRow(
                  box(width=7, status="primary", solidHeader=TRUE,
                      title="Shipments by Mode & Year",
                      highchartOutput("hc_mode_year", height="300px")),
                  box(width=5, status="primary", solidHeader=TRUE,
                      title="Mode Share (all years)",
                      highchartOutput("hc_mode_pie", height="300px"))
                ),
                fluidRow(
                  box(width=12, status="primary", solidHeader=TRUE,
                      title="Annual On-Time Delivery Rate (2006-2015)",
                      highchartOutput("hc_ontime_overview", height="240px"))
                )
        ),
        
        # TAB 2: SUPPLY CHAIN MAP
        # FIX 1b: girafeOutput("gg_top10") replaced with highchartOutput("hc_top10")
        tabItem("map",
                fluidRow(
                  box(width=8, status="primary", solidHeader=TRUE,
                      title="Country-Level Supply Chain Map",
                      selectInput("map_metric", "Colour & size circles by:",
                                  choices  = c("Avg Freight Cost (USD)"="avg_freight",
                                               "Avg Cost per KG (USD)" ="avg_cpk",
                                               "On-Time Rate (%)"      ="ontime_pct"),
                                  selected = "avg_freight"),
                      leafletOutput("map_leaflet", height="420px")),
                  box(width=4, status="primary", solidHeader=TRUE,
                      title="Top 10 Countries",
                      highchartOutput("hc_top10", height="420px"))   # was girafeOutput
                )
        ),
        
        # TAB 3: EFFICIENCY
        # FIX 1c: girafeOutput("gg_days_var") replaced with highchartOutput("hc_days_var")
        tabItem("efficiency",
                fluidRow(
                  box(width=8, status="primary", solidHeader=TRUE,
                      title="On-Time Delivery Rate - Last 24 Months",
                      highchartOutput("hc_ontime_monthly", height="290px")),
                  box(width=4, status="primary", solidHeader=TRUE,
                      title="Overall Delivery Status",
                      highchartOutput("hc_status_donut", height="290px"))
                ),
                fluidRow(
                  box(width=6, status="primary", solidHeader=TRUE,
                      title="Annual On-Time Rate Trend",
                      highchartOutput("hc_ontime_annual", height="260px")),
                  box(width=6, status="primary", solidHeader=TRUE,
                      title="Days Variance: Late vs On-Time",
                      highchartOutput("hc_days_var", height="260px"))  # was girafeOutput
                )
        ),
        
        # TAB 4: DIRECTOR'S STORY
        tabItem("story",
                fluidRow(
                  column(3, kpi_card("s_air_cpk",   "Air Avg Cost per KG",
                                     paste0("$", avg_air_cpk, " / kg"),   BLUE)),
                  column(3, kpi_card("s_truck_cpk", "Truck Avg Cost per KG",
                                     paste0("$", avg_truck_cpk, " / kg"), GREEN)),
                  column(3, kpi_card("s_premium",   "Air Premium over Truck",
                                     "Cost-per-kg multiplier",            RED)),
                  column(3, kpi_card("s_ontime",    "Overall On-Time Rate",
                                     "No significant Air advantage",      AMBER))
                ),
                fluidRow(
                  box(width=12, status="danger", solidHeader=TRUE,
                      title="Cost per KG by Shipment Mode - 2006 to 2015",
                      highchartOutput("hc_story_ts", height="360px"))
                ),
                fluidRow(
                  box(width=12, status="warning", solidHeader=FALSE,
                      tags$h4(style="color:#1A3A5C;margin-top:0;font-weight:700;",
                              "Strategic Recommendation for Director"),
                      tags$p(style="font-size:1.05em;",
                             "Air freight costs per kilogram are ",
                             tags$strong("consistently and materially higher"),
                             " than Truck across all 10 years (2006-2015). ",
                             "On-time delivery rates show ",
                             tags$strong("no meaningful advantage"),
                             " for Air - the cost premium does NOT purchase better programme outcomes."),
                      tags$p(style="font-size:1.05em;",
                             tags$b("Recommended action: "),
                             "Pilot a mode-switching policy for bulk, non-urgent health commodities ",
                             "on routes where Truck is operationally feasible. Start with the top-5 ",
                             "highest-cost countries on the Supply Chain Map tab.")
                  )
                )
        ),
        
        # TAB 5: EDA REPORT
        tabItem("eda",
                fluidRow(
                  box(width=12, status="primary", solidHeader=TRUE,
                      title="EDA Summary - Data Quality Issues in Raw Dataset",
                      tags$p(style="color:#555;",
                             "Figures generated by ", tags$code("scripts/02_eda_data_quality.R"),
                             ". Run that script first to populate charts below."))
                ),
                fluidRow(
                  box(width=12, status="danger", solidHeader=TRUE,
                      title="Issue #1 - ~40% of Freight Cost values are Non-Numeric Text",
                      uiOutput("eda_issue1"))
                ),
                fluidRow(
                  box(width=12, status="warning", solidHeader=TRUE,
                      title="Issue #2 - Extreme Outliers in Numeric Freight Costs",
                      uiOutput("eda_issue2"))
                ),
                fluidRow(
                  box(width=12, status="warning", solidHeader=TRUE,
                      title="Issue #3 - String N/A Sentinel in Shipment Mode",
                      uiOutput("eda_issue3"))
                ),
                fluidRow(
                  box(width=12, status="success", solidHeader=TRUE,
                      title="Hypothesis Test - Air vs Truck Cost/kg for Heavy Loads (> 500 kg)",
                      uiOutput("eda_hyp"))
                )
        ),
        
        # TAB 6: DATA EXPLORER
        tabItem("explorer",
                fluidRow(
                  box(width=12, status="primary", solidHeader=TRUE,
                      title="Interactive Shipment Record Explorer",
                      fluidRow(
                        column(3, selectInput("exp_year",    "Year",
                                              choices=c("All", sort(unique(df_full$year), na.last=TRUE)), selected="All")),
                        column(3, selectInput("exp_country", "Country",
                                              choices=c("All", sort(unique(df_full$country))), selected="All")),
                        column(3, selectInput("exp_mode",    "Shipment Mode",
                                              choices=c("All","Air","Air Charter","Truck","Ocean"), selected="All")),
                        column(3, selectInput("exp_status",  "Delivery Status",
                                              choices=c("All","On-Time","Late","Unknown"), selected="All"))
                      ),
                      fluidRow(
                        column(3, selectInput("exp_product", "Product Group",
                                              choices=c("All", sort(unique(df_full$product_group))), selected="All")),
                        column(3, selectInput("exp_freight_cat", "Freight Category",
                                              choices=c("All","Numeric","Included","Cross-reference","Missing"),
                                              selected="All")),
                        column(6, div(style="padding-top:25px;",
                                      downloadButton("dl_filtered", "Download Filtered CSV", class="btn-dl")))
                      ),
                      DTOutput("tbl_explorer")
                  )
                )
        ),
        
        # TAB 7: DOWNLOADS
        tabItem("downloads",
                fluidRow(
                  box(width=6, status="primary", solidHeader=TRUE,
                      title="Data Downloads",
                      tags$p("Download the full cleaned dataset or a summary."), br(),
                      downloadButton("dl_full_csv",    "Full Dataset (CSV)",    class="btn-dl"), br(), br(),
                      downloadButton("dl_summary_csv", "Country Summary (CSV)", class="btn-dl"), br(), br(),
                      downloadButton("dl_ontime_csv",  "On-Time by Year (CSV)", class="btn-dl")
                  ),
                  box(width=6, status="success", solidHeader=TRUE,
                      title="Report Download",
                      tags$p("The EDA narrative report (HTML)."),
                      tags$p(style="color:#888;font-size:.9em;",
                             "Knit first:", tags$br(),
                             tags$code("rmarkdown::render('reports/02_eda_data_quality_report.Rmd')")),
                      br(), uiOutput("report_download_ui")
                  )
                )
        )
        
      ) # end tabItems
    )   # end dashboardBody
  )     # end dashboardPage
) # end secure_app

# =============================================================================
# 4.  SERVER
# =============================================================================
server <- function(input, output, session) {
  
  secure_server(check_credentials = check_credentials(credentials))
  
  # KPI outputs
  output$kpi_ontime    <- renderUI(paste0(overall_ontime, "%"))
  output$kpi_total     <- renderUI(format(total_records, big.mark=","))
  output$kpi_countries <- renderUI(total_countries)
  output$kpi_ratio     <- renderUI(
    if (!is.na(air_truck_ratio)) paste0(air_truck_ratio, "x") else "N/A")
  output$s_air_cpk    <- renderUI(paste0("$", avg_air_cpk))
  output$s_truck_cpk  <- renderUI(paste0("$", avg_truck_cpk))
  output$s_premium    <- renderUI(
    if (!is.na(air_truck_ratio)) paste0(air_truck_ratio, "x more expensive") else "N/A")
  output$s_ontime     <- renderUI(paste0(overall_ontime, "%"))
  
  # TAB 1: Mode x Year stacked bar
  output$hc_mode_year <- renderHighchart({
    d <- df_full %>%
      filter(!is.na(year), !is.na(shipment_mode_clean)) %>%
      count(year, shipment_mode_clean) %>%
      pivot_wider(names_from=shipment_mode_clean, values_from=n, values_fill=0L)
    modes  <- setdiff(names(d), "year")
    colors <- unname(PAL[modes])
    if (any(is.na(colors))) colors[is.na(colors)] <- "#cccccc"
    hc <- highchart() %>%
      hc_chart(type="column") %>%
      hc_xAxis(categories=as.character(d$year)) %>%
      hc_plotOptions(column=list(stacking="normal")) %>%
      hc_colors(colors) %>% hc_tooltip(shared=TRUE) %>% hc_legend(enabled=TRUE)
    for (m in modes) hc <- hc_add_series(hc, name=m, data=as.list(d[[m]]))
    hc
  })
  
  # TAB 1: Mode share pie
  output$hc_mode_pie <- renderHighchart({
    d <- df_full %>%
      filter(!is.na(shipment_mode_clean)) %>%
      count(shipment_mode_clean) %>%
      rename(name=shipment_mode_clean, y=n)
    hchart(d, "pie", hcaes(name=name, y=y)) %>%
      hc_plotOptions(pie=list(
        dataLabels=list(enabled=TRUE, format="{point.name}: {point.percentage:.1f}%")))
  })
  
  # TAB 1: Annual on-time line
  output$hc_ontime_overview <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_xAxis(categories=as.character(df_ontime_yr$year)) %>%
      hc_yAxis(title=list(text="On-Time %"), max=100, min=0,
               plotLines=list(list(value=80, color="grey", dashStyle="Dash",
                                   width=1.5, label=list(text="80% target")))) %>%
      hc_add_series(name="On-Time %", data=as.list(df_ontime_yr$ontime_pct),
                    color=NAVY, lineWidth=2.5,
                    marker=list(radius=4, fillColor=GREEN)) %>%
      hc_tooltip(pointFormat="<b>{point.y}%</b>") %>% hc_legend(enabled=FALSE)
  })
  
  # TAB 2: Leaflet map
  output$map_leaflet <- renderLeaflet({
    metric <- input$map_metric
    lbl    <- switch(metric,
                     avg_freight="Avg Freight ($)", avg_cpk="$/kg", ontime_pct="On-Time %")
    
    # Keep ALL columns so ontime_pct is always available for the popup.
    # Add metric_val as a NEW column (copy) rather than renaming.
    d <- df_country %>%
      mutate(metric_val = .data[[metric]]) %>%
      filter(!is.na(metric_val))
    
    pal_fn <- colorNumeric("YlOrRd", d$metric_val, na.color="#e0e0e0")
    
    centroids <- data.frame(
      country=c("South Africa","Nigeria","Uganda","Zimbabwe","Kenya","Ethiopia",
                "Tanzania","Mozambique","Zambia","Ghana","Cameroon","Rwanda",
                "Malawi","Botswana","Cote d'Ivoire","Senegal","Haiti","Vietnam",
                "India","Pakistan","Lesotho","Swaziland","DRC","Namibia","Angola"),
      lat=c(-30,-9,1,-19,-1,9,-6,-18,-15,8,4,-2,-13,-22,7,14,19,16,20,30,-29,-27,-4,-22,-12),
      lng=c(25,8,32,30,37,40,35,35,28,-1,12,30,34,24,-5,-14,-73,108,77,70,28,32,23,18,18),
      stringsAsFactors=FALSE)
    
    d_map <- left_join(d, centroids, by="country") %>% filter(!is.na(lat), !is.na(lng))
    
    leaflet(d_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng=~jitter(lng, amount=0.8), lat=~jitter(lat, amount=0.8),
        radius=~rescale(metric_val, to=c(6,26)),
        fillColor=~pal_fn(metric_val), color="white", weight=1.2, fillOpacity=0.82,
        label=~paste0(country, " - ", lbl, ": ", round(metric_val, 1)),
        popup=~paste0(
          "<b>", country, "</b><br>",
          lbl, ": <b>", round(metric_val, 1), "</b><br>",
          "Shipments: ", shipments, "<br>",
          "On-Time Rate: ", ontime_pct, "%<br>",
          "Avg Freight: $", avg_freight, "<br>",
          "Avg Cost/kg: $", avg_cpk
        )
      ) %>%
      addLegend("bottomright", pal=pal_fn, values=~metric_val, title=lbl, opacity=0.9)
  })
  
  # TAB 2: Top-10 horizontal bar
  output$hc_top10 <- renderHighchart({
    metric <- input$map_metric
    lbl    <- switch(metric,
                     avg_freight="Avg Freight (USD)", avg_cpk="Avg Cost/kg (USD)",
                     ontime_pct="On-Time Rate (%)")
    d <- df_country %>%
      mutate(val = .data[[metric]]) %>%
      filter(!is.na(val)) %>%
      arrange(desc(val)) %>%
      slice_head(n=10)
    highchart() %>%
      hc_chart(type="bar") %>%
      hc_xAxis(categories=rev(d$country), title=list(text=NULL)) %>%
      hc_yAxis(title=list(text=lbl)) %>%
      hc_add_series(name=lbl, data=as.list(rev(d$val)), color=NAVY, showInLegend=FALSE,
                    dataLabels=list(enabled=TRUE, format="{point.y:.1f}",
                                    style=list(fontSize="10px", fontWeight="normal"))) %>%
      hc_tooltip(pointFormat=paste0("<b>{point.category}</b><br>",lbl,": <b>{point.y:.1f}</b>")) %>%
      hc_title(text=paste("Top 10 -",lbl),
               style=list(fontSize="13px", fontWeight="bold", color=NAVY))
  })
  
  # TAB 3: Monthly on-time line
  output$hc_ontime_monthly <- renderHighchart({
    d <- df_ontime_monthly
    highchart() %>%
      hc_chart(type="line") %>%
      hc_xAxis(categories=d$year_month,
               labels=list(rotation=-45, style=list(fontSize="10px"))) %>%
      hc_yAxis(title=list(text="On-Time %"), max=100, min=0,
               plotLines=list(list(value=80, color="grey", dashStyle="Dash", width=1.2,
                                   label=list(text="80%", style=list(color="grey"))))) %>%
      hc_add_series(
        name="On-Time %",
        data=lapply(seq_len(nrow(d)), function(i)
          list(y=d$ontime_pct[i], color=if(d$ontime_pct[i]<70) RED else GREEN)),
        lineWidth=2, marker=list(enabled=TRUE, radius=4)) %>%
      hc_tooltip(pointFormat="<b>{point.y}%</b>") %>% hc_legend(enabled=FALSE)
  })
  
  # TAB 3: Annual on-time area
  output$hc_ontime_annual <- renderHighchart({
    highchart() %>%
      hc_chart(type="area") %>%
      hc_xAxis(categories=as.character(df_ontime_yr$year)) %>%
      hc_yAxis(title=list(text="On-Time %"), max=100, min=0,
               plotLines=list(list(value=80, color="grey", dashStyle="Dash", width=1.2))) %>%
      hc_add_series(name="On-Time %", data=as.list(df_ontime_yr$ontime_pct),
                    color=NAVY, fillOpacity=0.12, lineWidth=2, marker=list(radius=3)) %>%
      hc_tooltip(pointFormat="<b>{point.y}%</b>") %>% hc_legend(enabled=FALSE)
  })
  
  # TAB 3: Delivery status donut
  output$hc_status_donut <- renderHighchart({
    d <- df_full %>% count(delivery_status) %>% rename(name=delivery_status, y=n)
    hchart(d, "pie", hcaes(name=name, y=y)) %>%
      hc_plotOptions(pie=list(innerSize="55%", colors=c(GREEN, RED, "#B0B0B0"),
                              dataLabels=list(enabled=TRUE,
                                              format="{point.name}: {point.percentage:.1f}%")))
  })
  
  # TAB 3: Days-variance stacked histogram — FIX 1e: was renderGirafe / geom_histogram_interactive
  output$hc_days_var <- renderHighchart({
    d      <- df_full %>%
      filter(!is.na(days_variance), delivery_status != "Unknown", abs(days_variance) < 180)
    breaks <- seq(-180, 180, by=6)
    ot  <- hist(d$days_variance[d$delivery_status == "On-Time"], breaks=breaks, plot=FALSE)
    lat <- hist(d$days_variance[d$delivery_status == "Late"],    breaks=breaks, plot=FALSE)
    highchart() %>%
      hc_chart(type="column", animation=FALSE) %>%
      hc_xAxis(
        categories=as.character(breaks[-length(breaks)]),
        title=list(text="Days Variance (Delivered - Scheduled)"),
        plotLines=list(list(
          value=which(breaks==0)-1, color="#333", width=2, dashStyle="Dash",
          label=list(text="0 days", rotation=0, style=list(color="#333"))))) %>%
      hc_yAxis(title=list(text="Number of Shipments")) %>%
      hc_plotOptions(column=list(stacking="normal", groupPadding=0,
                                 pointPadding=0, borderWidth=0.2)) %>%
      hc_add_series(name="On-Time", data=as.list(ot$counts),  color=GREEN) %>%
      hc_add_series(name="Late",    data=as.list(lat$counts), color=RED) %>%
      hc_tooltip(shared=TRUE,
                 pointFormat="<span style='color:{point.color}'>*</span> {series.name}: <b>{point.y}</b><br>") %>%
      hc_legend(enabled=TRUE)
  })
  
  # TAB 4: Director's Story time-series
  output$hc_story_ts <- renderHighchart({
    modes  <- c("Air","Truck","Ocean")
    colors <- c(BLUE, GREEN, AMBER)
    years  <- sort(unique(df_mode_trend$year))
    hc <- highchart() %>%
      hc_chart(type="line") %>%
      hc_xAxis(categories=as.character(years), title=list(text="Year")) %>%
      hc_yAxis(title=list(text="Avg Cost per KG (USD)"), labels=list(format="${value}")) %>%
      hc_tooltip(shared=TRUE,
                 pointFormat="<span style='color:{point.color}'>*</span> <b>{series.name}</b>: ${point.y:.2f}/kg<br>") %>%
      hc_legend(enabled=TRUE)
    for (i in seq_along(modes)) {
      vals <- df_mode_trend %>%
        filter(mode==modes[i]) %>% arrange(year) %>%
        right_join(data.frame(year=years), by="year") %>% pull(avg_cpk)
      hc <- hc_add_series(hc, name=modes[i], data=as.list(vals),
                          color=colors[i], lineWidth=2.5, marker=list(radius=4))
    }
    hc
  })
  
  # TAB 5: EDA figures
  output$eda_issue1 <- renderUI({
    fig_box(fig_uri("issue1_non_numeric_freight.png"),
            "Issue #1 - Non-numeric freight cost values (script 02)")
  })
  output$eda_issue2 <- renderUI({
    fig_box(fig_uri("issue2_freight_outliers.png"),
            "Issue #2 - Outliers in numeric freight costs (script 02)")
  })
  output$eda_issue3 <- renderUI({
    fig_box(fig_uri("issue3_shipment_mode_na.png"),
            "Issue #3 - N/A sentinel in shipment mode (script 02)")
  })
  output$eda_hyp <- renderUI({
    fig_box(fig_uri("hypothesis_air_vs_truck.png"),
            "Hypothesis test - Air vs Truck cost/kg (script 02)")
  })
  
  # TAB 6: Filtered table
  filtered_data <- reactive({
    d <- df_full
    if (input$exp_year        != "All") d <- filter(d, year==as.integer(input$exp_year))
    if (input$exp_country     != "All") d <- filter(d, country==input$exp_country)
    if (input$exp_mode        != "All") d <- filter(d, shipment_mode_clean==input$exp_mode)
    if (input$exp_status      != "All") d <- filter(d, delivery_status==input$exp_status)
    if (input$exp_product     != "All") d <- filter(d, product_group==input$exp_product)
    if (input$exp_freight_cat != "All") d <- filter(d, freight_category==input$exp_freight_cat)
    d %>% select(id, country, shipment_mode_clean, product_group, vendor,
                 scheduled_date, delivered_date, year, delivery_status, days_variance,
                 freight_cost_usd, cost_per_kg, weight_kg, freight_category, line_item_value)
  })
  
  output$tbl_explorer <- renderDT({
    datatable(filtered_data(), rownames=FALSE, extensions="Buttons",
              options=list(pageLength=15, scrollX=TRUE, dom="Bfrtip",
                           buttons=list(list(extend="csv",   filename="sfh_filtered"),
                                        list(extend="excel", filename="sfh_filtered")))) %>%
      formatCurrency(c("freight_cost_usd","cost_per_kg"), digits=2) %>%
      formatRound("weight_kg", digits=1) %>%
      formatStyle("delivery_status",
                  backgroundColor=styleEqual(c("On-Time","Late","Unknown"),
                                             c("#e8f8ee","#fde8e8","#f8f8f8")))
  })
  
  output$dl_filtered <- downloadHandler(
    filename=function() paste0("sfh_filtered_", Sys.Date(), ".csv"),
    content =function(file) write_csv(filtered_data(), file)
  )
  
  # TAB 7: Downloads
  output$dl_full_csv <- downloadHandler(
    filename=function() paste0("sfh_cleaned_shipments_", Sys.Date(), ".csv"),
    content =function(file) write_csv(df_full, file)
  )
  output$dl_summary_csv <- downloadHandler(
    filename=function() paste0("sfh_country_summary_", Sys.Date(), ".csv"),
    content =function(file) write_csv(df_country, file)
  )
  output$dl_ontime_csv <- downloadHandler(
    filename=function() paste0("sfh_ontime_annual_", Sys.Date(), ".csv"),
    content =function(file) write_csv(df_ontime_yr, file)
  )
  
  report_candidates <- c(
    "02_eda_data_quality_report.html",
    "reports/02_eda_data_quality_report.html",
    "../reports/02_eda_data_quality_report.html"
  )
  report_path <- Filter(file.exists, report_candidates)[1]
  
  output$report_download_ui <- renderUI({
    if (length(report_path) > 0 && !is.na(report_path)) {
      downloadButton("dl_report", "Download HTML Report", class="btn-dl")
    } else {
      tags$p(style="color:#E05C5C;",
             "Report not found. Knit first:", tags$br(),
             tags$code("rmarkdown::render('reports/02_eda_data_quality_report.Rmd')"))
    }
  })
  
  output$dl_report <- downloadHandler(
    filename=function() "sfh_eda_report.html",
    content =function(file) {
      if (length(report_path) > 0 && !is.na(report_path)) file.copy(report_path, file)
    }
  )
}

# =============================================================================
# 5.  LAUNCH
# =============================================================================
shinyApp(ui=ui, server=server)