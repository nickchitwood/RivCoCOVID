# Get COVID Data from Riverside County ArcGIS

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(EpiEstim)
library(incidence)
library(RCurl)
library(ggthemes)

# Import data
r <- GET("https://services1.arcgis.com/pWmBUdSlVpXStHU6/arcgis/rest/services/COVID19_Cases_DateReport/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=DateReported%20asc&outSR=102100&resultOffset=0&resultRecordCount=32000&resultType=standard&cacheHint=true")
r_json <- content(r)
cases <- fromJSON(
  r_json
)$features$attributes %>%
  mutate(
    DateReported = as_datetime(DateReported / 1000),
    Date = as_date(DateReported)
  )

# Construct the curve
incidence <- as.incidence(
  cases$ReportedNewCases,
  dates = cases$Date
)

# Plot incidence
plot(incidence)

# Estimate R
covid_parametric <- estimate_R(
  incidence,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 3.96,
    std_si = 4.75
  ))
)

R_df <- covid_parametric$R %>%
  mutate(Date = date("2020-02-20") + t_end)


most_recent_date <- cases %>%
  slice_tail(1) %>%
  pull(Date)
  
most_recent_r <- R_df %>%
  slice_tail(1) %>%
  pull(`Mean(R)`)

case_table <- cases %>% 
  select(Date, ReportedNewCases, ReportedTotalCases) %>%
  left_join(R_df %>% select(Date, `Mean(R)`)) %>%
  mutate(
    CasesPer100k = ReportedNewCases / 24.70546,
    RiskLevel = case_when(
      CasesPer100k > 7 ~ "Purple - Widespread (>7)",
      CasesPer100k >=4 ~ "Red - Substantial (4-7)",
      CasesPer100k >= 1 ~ "Orange - Moderate (1-3.9)",
      CasesPer100k < 1 ~ "Yellow - Minimal (<1)"
    )
  )

# Plot R
png("RivCACOVID/incid.png", width = 800, height = 400)
incid <- plot(covid_parametric, "incid") + 
  geom_hline(yintercept = 7 * 24.70546, linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = 4 * 24.70546, linetype = 'dashed', color = 'orange') +
  geom_hline(yintercept = 1 * 24.70546, linetype = 'dashed', color = 'yellow') +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") +
theme_few()
incid
dev.off()

png("RivCACOVID/R.png", width = 800, height = 400)
plot(covid_parametric, "R", legend = FALSE)  +
  theme_few() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0,3)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") +
dev.off()

# Write data to cache
save(case_table, file = 'RivCACOVID/case_table.Rdata')
save(covid_parametric, file = 'RivCACOVID/covid_parametric.Rdata')
save(most_recent_date, file = 'RivCACOVID/most_recent_date.Rdata')
save(most_recent_r, file = 'RivCACOVID/most_recent_r.Rdata')

