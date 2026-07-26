# TEACH Dashboard — Setup Guide (Live REDCap Version)

## Files

- `redcap_connect.R` — connects to the REDCap API, pulls **all** records
  (every event, every repeating instrument), and reproduces the same
  cleaning/derivation logic as your Power Query M script (buckets, missing
  flags, cascade status, screening week/month). **Edit `REDCAP_TOKEN` here.**
- `redcap_inspect.R` — optional one-off diagnostic: confirms the API
  connection works and checks that key fields exist, before running the
  full app.
- `app.R` — the live dashboard. Currently has 3 of the 7 Power BI screens:
  **Home**, **Recruitment Cascade**, **Recruitment Cascade Table**. The
  rest (Trend over time, By Health Facility, Missing Consent, List) come
  next once we've confirmed their exact calculations.

## 1. Install required R packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "DT", "plotly",
  "lubridate", "stringr", "curl", "jsonlite"
))
```

## 2. Configure

Open `redcap_connect.R` and set:
```r
REDCAP_TOKEN <- "your_redcap_api_token"
```
`REDCAP_URL` is already set to `https://utirc.mak.ac.ug:8181/api/`, taken
from your Power Query script. **Double check this is the right host** —
it's different from the one you mentioned earlier
(`chdcresearch.mak.ac.ug:8181`), so worth confirming which is production
before sharing the link with colleagues.

As always: don't paste your token into chat, and don't hardcode it if this
project will ever go on GitHub — use a local `.Renviron` file instead:
```
REDCAP_TOKEN=your_redcap_api_token
```

## 3. (Optional) Verify the connection first

```r
setwd("path/to/teach_dashboard")
source("redcap_inspect.R")
```
This prints row counts, which events/instruments came back, and confirms
the key fields exist. Worth running once before the full app, given this
project has a more complex structure (5 events, several repeating forms)
than the Kobo one.

## 4. Run the dashboard

```r
setwd("path/to/teach_dashboard")
shiny::runApp("app.R")
```

## 5. What's different from the Power BI version (on purpose)

- **Month 2 / Month 5 SBQ cards now populate** (once there's data in those
  events) — your original M query only read the Baseline event, which is
  why those two cards were always blank. This version pulls those events
  separately and joins back by `study_id`.
- Facility and Screening Week filters drive every number on every tab, same
  as the Power BI slicers.

## 6. What I still need from you before building the remaining screens

- The DAX (or M) behind **Overall Retention %** on the Trend over time page
  — the current Power BI version has floating-point label glitches
  (`88.33333333332334`), so I want the actual formula rather than
  reverse-engineering it from a chart.
- Same for **Enrollment Rate %** on the By Health Facility page.
- Anything defining the **Missing Consent** and **List** pages, since I
  haven't seen those yet.
