# ==============================================================================
# TEACH Recruitment Dashboard (LIVE) -- initial screens
# ==============================================================================
# Screens built so far: Home, Recruitment Cascade, Recruitment Cascade Table,
# By Health Facility. Trend over time / Missing Consent / List come next,
# once their DAX/measure definitions are confirmed.
#
# BEFORE RUNNING: open redcap_connect.R and set REDCAP_TOKEN to your API
# token. REDCAP_URL is already set to https://utirc.mak.ac.ug:8181/api/
# (per your Power Query script) -- double check that's the right host before
# going live, since it differs from the one you gave me earlier.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(lubridate)

source("redcap_connect.R")

CASCADE_STEPS <- c(
  "Cascade Step 1 Screened",
  "Cascade Step 2 Eligible",
  "Cascade Step 3 Consented",
  "Cascade Step 4 Enrolled",
  "Cascade Step 5 SBQ Done"
)

# One row per participant -> named vector of cascade step counts
compute_cascade_counts <- function(df) {
  c(
    "Cascade Step 1 Screened"  = sum(df$screening_form_complete == "Complete", na.rm = TRUE),
    "Cascade Step 2 Eligible"  = sum(df$eligibility_status == "Yes", na.rm = TRUE),
    "Cascade Step 3 Consented" = sum(df$consent == "Yes", na.rm = TRUE),
    "Cascade Step 4 Enrolled"  = sum(df$enrollment_form_complete == "Complete", na.rm = TRUE),
    "Cascade Step 5 SBQ Done"  = sum(df$sbq_form_complete == "Complete", na.rm = TRUE)
  )
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "TEACH Dashboard (Live)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  sidebar = sidebar(
    width = 280,
    h5("Filters"),
    selectInput("f_facility", "Facility", choices = c("All"), selected = "All"),
    dateRangeInput("f_daterange", "Screening Date Range",
                    start = Sys.Date() - 365, end = Sys.Date()),
    hr(),
    textOutput("connection_status"),
    p(class = "text-muted small",
      paste0("Auto-refreshes every ", REFRESH_SECONDS, " seconds."))
  ),

  nav_panel(
    "Home",
    div(class = "mb-3", h4(textOutput("header_line"))),
    p(class = "text-muted small",
      "Green = forward progress (Completed/Eligible/Yes). Red = a gap or dropout (NOT Completed/Ineligible/No)."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("vb_screened"),
      uiOutput("vb_eligible"),
      uiOutput("vb_ineligible"),
      uiOutput("vb_consent_missing")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("vb_consent_yes"),
      uiOutput("vb_consent_no"),
      uiOutput("vb_enrolled"),
      uiOutput("vb_sbq_baseline")
    ),
    layout_columns(
      col_widths = c(3, 3, 3),
      uiOutput("vb_sbq_baseline_missing"),
      uiOutput("vb_sbq_m2"),
      uiOutput("vb_sbq_m5")
    )
  ),

  nav_panel(
    "Recruitment Cascade",
    card(
      card_header("Cascade Step 1 Screened, Step 2 Eligible, Step 3 Consented, Step 4 Enrolled, Step 5 SBQ Done"),
      plotlyOutput("cascade_funnel", height = "450px")
    )
  ),

  nav_panel(
    "Recruitment Cascade Table",
    card(
      card_header("Cascade counts by facility"),
      DTOutput("cascade_table")
    )
  ),

  nav_panel(
    "Enrolled by Sex",
    p(class = "text-muted small",
      "Enrolled = eligible + consented (Yes) + enrollment form Complete."),
    card(
      card_header(textOutput("sex_chart_title")),
      plotlyOutput("sex_by_facility_chart", height = "650px")
    )
  ),

  nav_panel(
    "Enrolled by HIV Status",
    p(class = "text-muted small",
      "Enrolled = eligible + consented (Yes) + enrollment form Complete."),
    card(
      card_header(textOutput("hiv_chart_title")),
      plotlyOutput("hiv_by_facility_chart", height = "650px")
    )
  ),

  nav_panel(
    "Missing Forms",
    p(class = "text-muted small",
      "Uses the Facility and Screening Date Range filters in the sidebar (based on each participant's screening date), plus the Form and Status filters below."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("mf_form", "Form", choices = c("All", STAGE1_FORMS), selected = "All"),
      selectInput("mf_status", "Status", choices = c("All", ALL_STATUSES), selected = "All"),
      value_box(title = "Total Missing Forms", value = textOutput("mf_total"), showcase = icon("list-check")),
      value_box(title = "Overdue", value = textOutput("mf_overdue_total"), showcase = icon("triangle-exclamation"), theme = "danger")
    ),
    card(
      card_header("Missing form line list"),
      DTOutput("missing_form_table")
    )
  ),

  nav_panel(
    "Missing TEACH Outcomes",
    p(class = "text-muted small",
      "Uses the Facility and Screening Date Range filters in the sidebar, plus the Form, Status, and TB Meds filters below. Lab / TB Treatment / ART Refill all apply only to eligible, consented, enrolled participants (ART Refill additionally requires HIV positive + on ART), and are all measured from tbrx_start_date."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("mo_form", "Form", choices = c("All", STAGE2_FORMS), selected = "All"),
      selectInput("mo_status", "Status", choices = c("All", STAGE2_STATUSES), selected = "All"),
      selectInput("mo_tbmeds", "Issue of TB Meds (TB Treatment only)", choices = c("All", TB_MEDS_CHOICES), selected = "All"),
      value_box(title = "Total Missing", value = textOutput("mo_total"), showcase = icon("list-check"))
    ),
    card(
      card_header("Missing TEACH Outcomes line list"),
      DTOutput("missing_outcomes_table")
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Poll REDCap on a timer ----
  live_data <- reactivePoll(
    intervalMillis = REFRESH_SECONDS * 1000,
    session = session,
    checkFunc = function() Sys.time(),
    valueFunc = function() load_live_data()
  )

  output$connection_status <- renderText({
    d <- live_data()
    paste0("Last synced: ", format(Sys.time(), "%H:%M:%S"),
           " -- ", nrow(d$baseline), " baseline record(s) loaded")
  })

  # Populate filter choices once real data arrives
  observeEvent(live_data(), {
    df <- live_data()$baseline
    if (nrow(df) == 0) return()
    facilities <- sort(unique(na.omit(df$facility)))
    updateSelectInput(session, "f_facility", choices = c("All", facilities), selected = "All")

    valid_dates <- na.omit(df$screening_date)
    if (length(valid_dates) > 0) {
      updateDateRangeInput(session, "f_daterange",
                            start = min(valid_dates), end = max(valid_dates),
                            min = min(valid_dates), max = max(valid_dates))
    }
  }, once = TRUE)

  baseline_filtered <- reactive({
    df <- live_data()$baseline
    if (nrow(df) == 0) return(df)
    if (input$f_facility != "All") df <- df %>% filter(facility == input$f_facility)
    df <- df %>% filter(screening_date >= input$f_daterange[1], screening_date <= input$f_daterange[2])
    df
  })

  # ---- Home tab header ----
  output$header_line <- renderText({
    d <- live_data()
    paste0("TEACH -- Data as at ", format(Sys.Date(), "%d %b %Y"),
           " | N = ", d$n_total, " participants")
  })

  # ---- Home KPIs ----
  # Shared computation of every Home-page KPI, keyed for reuse below
  current_kpis <- reactive({
    df <- baseline_filtered()
    m2 <- live_data()$month2_sbq
    m5 <- live_data()$month5_sbq
    ids <- if (nrow(df) > 0) df$study_id else character(0)

    list(
      screened             = if (nrow(df) == 0) NA_integer_ else sum(df$screening_form_complete == "Complete", na.rm = TRUE),
      consent_missing      = if (nrow(df) == 0) NA_integer_ else sum(df$missing_consent_n, na.rm = TRUE),
      eligible             = if (nrow(df) == 0) NA_integer_ else sum(df$eligibility_status == "Yes", na.rm = TRUE),
      ineligible           = if (nrow(df) == 0) NA_integer_ else sum(df$eligibility_status == "No", na.rm = TRUE),
      consent_yes          = if (nrow(df) == 0) NA_integer_ else sum(df$consent == "Yes", na.rm = TRUE),
      consent_no           = if (nrow(df) == 0) NA_integer_ else sum(df$consent == "No", na.rm = TRUE),
      enrolled             = if (nrow(df) == 0) NA_integer_ else sum(df$enrollment_form_complete == "Complete", na.rm = TRUE),
      sbq_baseline         = if (nrow(df) == 0) NA_integer_ else sum(df$sbq_form_complete == "Complete", na.rm = TRUE),
      sbq_baseline_missing = if (nrow(df) == 0) NA_integer_ else sum(df$missing_sbq_n, na.rm = TRUE),
      sbq_m2               = if (nrow(m2) == 0) NA_integer_ else sum(m2$study_id %in% ids & m2$sbq_form_complete == "Complete", na.rm = TRUE),
      sbq_m5               = if (nrow(m5) == 0) NA_integer_ else sum(m5$study_id %in% ids & m5$sbq_form_complete == "Complete", na.rm = TRUE)
    )
  })

  # Static classification: does this card represent forward progress
  # (green/"success") or a dropout/gap in the cascade (red/"danger")?
  # This is fixed per card, not based on trend over time.
  kpi_color <- list(
    screened             = "success",
    consent_missing      = "danger",
    eligible             = "success",
    ineligible            = "danger",
    consent_yes           = "success",
    consent_no             = "danger",
    enrolled                = "success",
    sbq_baseline             = "success",
    sbq_baseline_missing      = "danger",
    sbq_m2                     = "success",
    sbq_m5                      = "success"
  )

  kpi_meta <- list(
    screened             = list(title = "Notified from the Register",   icon = "clipboard-check"),
    consent_missing      = list(title = "Waiting to Consent",           icon = "hourglass-half"),
    eligible              = list(title = "Eligible from Screening",      icon = "circle-check"),
    ineligible             = list(title = "Ineligible from Screening",    icon = "circle-xmark"),
    consent_yes           = list(title = "Consented Yes",                 icon = "signature"),
    consent_no            = list(title = "Consented No",                  icon = "ban"),
    enrolled               = list(title = "Enrollment Completed",          icon = "user-plus"),
    sbq_baseline           = list(title = "Baseline SBQ Completed",        icon = "list-check"),
    sbq_baseline_missing   = list(title = "Baseline SBQ NOT Completed",    icon = "hourglass-half"),
    sbq_m2                  = list(title = "SBQ Completed Month 2",         icon = "calendar-check"),
    sbq_m5                  = list(title = "SBQ Completed Month 5",         icon = "calendar-check")
  )

  # Builds one color-coded value_box for a given KPI key
  render_kpi_box <- function(key) {
    force(key)  # lock in this card's key now -- without this, all cards
                # would lazily resolve to whichever key the loop ends on
    renderUI({
      val <- current_kpis()[[key]]
      meta <- kpi_meta[[key]]
      value_box(
        title = meta$title,
        value = if (is.na(val)) "--" else format(val, big.mark = ","),
        showcase = icon(meta$icon),
        theme = kpi_color[[key]]
      )
    })
  }

  for (k in names(kpi_meta)) {
    output[[paste0("vb_", k)]] <- render_kpi_box(k)
  }

  # ---- Recruitment Cascade funnel ----
  output$cascade_funnel <- renderPlotly({
    df <- baseline_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "funnel") |> layout(title = "No data for current filters"))
    counts <- compute_cascade_counts(df)
    plot_ly(
      y = names(counts), x = as.numeric(counts),
      type = "funnel",
      textinfo = "value+percent initial"
    ) |>
      layout(yaxis = list(title = ""), xaxis = list(title = ""))
  })

  # ---- Recruitment Cascade Table ----
  output$cascade_table <- renderDT({
    df <- baseline_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data for current filters"), rownames = FALSE))
    }
    by_facility <- df %>%
      group_by(facility) %>%
      summarise(
        `Cascade Step 1 Screened`  = sum(screening_form_complete == "Complete", na.rm = TRUE),
        `Cascade Step 2 Eligible`  = sum(eligibility_status == "Yes", na.rm = TRUE),
        `Cascade Step 3 Consented` = sum(consent == "Yes", na.rm = TRUE),
        `Cascade Step 4 Enrolled`  = sum(enrollment_form_complete == "Complete", na.rm = TRUE),
        `Baseline SBQ Done`       = sum(sbq_form_complete == "Complete", na.rm = TRUE),
        .groups = "drop"
      )

    # Month 2 / Month 5 SBQ completion live in separate REDCap events, so we
    # join them back to the (filtered) participant list by study_id to get
    # their facility, then count per facility -- same pattern as the Home
    # page's SBQ Month 2/6 cards.
    ids_facility <- df %>% select(study_id, facility)

    m2_by_facility <- live_data()$month2_sbq %>%
      filter(sbq_form_complete == "Complete") %>%
      inner_join(ids_facility, by = "study_id") %>%
      count(facility, name = "Month 2 SBQ Done")

    m5_by_facility <- live_data()$month5_sbq %>%
      filter(sbq_form_complete == "Complete") %>%
      inner_join(ids_facility, by = "study_id") %>%
      count(facility, name = "Month 5 SBQ Done")

    by_facility <- by_facility %>%
      left_join(m2_by_facility, by = "facility") %>%
      left_join(m5_by_facility, by = "facility") %>%
      mutate(across(c(`Month 2 SBQ Done`, `Month 5 SBQ Done`), ~ replace(., is.na(.), 0))) %>%
      arrange(desc(`Cascade Step 1 Screened`))

    totals <- by_facility %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(facility = "Total")

    out <- bind_rows(by_facility, totals) %>%
      rename(Facility = facility)

    datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # ---- By Health Facility tab ----
  # Enrolled = eligible + consented Yes + enrollment form Complete
  enrolled_filtered <- reactive({
    df <- baseline_filtered()
    if (nrow(df) == 0) return(df)
    df %>% filter(
      eligibility_status == "Yes",
      consent == "Yes",
      enrollment_form_complete == "Complete"
    )
  })

  # The full set of facilities that should appear on the axis -- either all
  # facilities in the system, or just the one selected in the sidebar filter.
  # Using this (rather than only facilities present in the enrolled subset)
  # is what makes facilities with zero enrolled participants still show up
  # as a zero-height bar instead of disappearing from the chart entirely.
  facility_universe <- reactive({
    all_f <- sort(unique(na.omit(live_data()$baseline$facility)))
    if (input$f_facility != "All") intersect(all_f, input$f_facility) else all_f
  })

  output$sex_chart_title <- renderText({
    total_enrolled <- nrow(enrolled_filtered())
    paste0("Enrolled by Sex, per Health Facility  |  Total Enrolled: ",
           format(total_enrolled, big.mark = ","))
  })

  output$hiv_chart_title <- renderText({
    df <- enrolled_filtered()
    total_positive <- if (nrow(df) == 0) 0 else sum(df$hiv_status == "Positive", na.rm = TRUE)
    paste0("Enrolled by HIV Status, per Health Facility  |  Total HIV Positive: ",
           format(total_positive, big.mark = ","))
  })

  output$sex_by_facility_chart <- renderPlotly({
    facilities <- facility_universe()
    if (length(facilities) == 0) return(plotly_empty(type = "bar") |> layout(title = "No facilities for current filters"))
    df <- enrolled_filtered()

    plot_df <- if (nrow(df) == 0) {
      data.frame(facility = character(0), sex = character(0), n = integer(0))
    } else {
      df %>% mutate(sex = ifelse(is.na(sex) | sex == "", "Unknown", sex)) %>% count(facility, sex)
    }
    sex_levels <- union(c("Male", "Female"), unique(plot_df$sex))
    plot_df <- plot_df %>%
      complete(facility = facilities, sex = sex_levels, fill = list(n = 0)) %>%
      mutate(facility = factor(facility, levels = facilities))

    plot_ly(plot_df, y = ~facility, x = ~n, color = ~sex, type = "bar", orientation = "h") |>
      layout(barmode = "stack",
             yaxis = list(title = "", categoryorder = "array", categoryarray = rev(facilities)),
             xaxis = list(title = "Enrolled participants"),
             margin = list(l = 160))
  })

  output$hiv_by_facility_chart <- renderPlotly({
    facilities <- facility_universe()
    if (length(facilities) == 0) return(plotly_empty(type = "bar") |> layout(title = "No facilities for current filters"))
    df <- enrolled_filtered()

    plot_df <- if (nrow(df) == 0) {
      data.frame(facility = character(0), hiv_status = character(0), n = integer(0))
    } else {
      df %>% mutate(hiv_status = ifelse(is.na(hiv_status) | hiv_status == "", "Unknown", hiv_status)) %>%
        count(facility, hiv_status)
    }
    hiv_levels <- union(c("Positive", "Negative", "Not available"), unique(plot_df$hiv_status))
    plot_df <- plot_df %>%
      complete(facility = facilities, hiv_status = hiv_levels, fill = list(n = 0)) %>%
      mutate(facility = factor(facility, levels = facilities))

    plot_ly(plot_df, y = ~facility, x = ~n, color = ~hiv_status, type = "bar", orientation = "h") |>
      layout(barmode = "stack",
             yaxis = list(title = "", categoryorder = "array", categoryarray = rev(facilities)),
             xaxis = list(title = "Enrolled participants"),
             margin = list(l = 160))
  })

  # ---- Missing Forms tab ----
  # Scoped to the same participants as the sidebar's Facility + Screening
  # Date Range filters (via baseline_filtered()'s study_ids), even though
  # each missing-form row's own "days since" reference date differs by form
  # type (screening/consent/enrollment date) -- the sidebar filters describe
  # WHO is in scope, not which date column to filter each row on.
  missing_forms_scoped <- reactive({
    mf <- live_data()$missing_forms
    ids <- baseline_filtered()$study_id
    if (nrow(mf) == 0 || length(ids) == 0) return(mf[0, ])
    mf %>% filter(study_id %in% ids, form %in% STAGE1_FORMS)
  })

  missing_forms_filtered <- reactive({
    df <- missing_forms_scoped()
    if (nrow(df) == 0) return(df)
    if (input$mf_form != "All") df <- df %>% filter(form == input$mf_form)
    if (input$mf_status != "All") df <- df %>% filter(status == input$mf_status)
    df
  })

  output$mf_total <- renderText({
    format(nrow(missing_forms_filtered()), big.mark = ",")
  })

  output$mf_overdue_total <- renderText({
    df <- missing_forms_filtered()
    if (nrow(df) == 0) return("0")
    format(sum(df$status == "Overdue", na.rm = TRUE), big.mark = ",")
  })

  output$missing_form_table <- renderDT({
    df <- missing_forms_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No missing forms match the current filters"), rownames = FALSE))
    }

    out <- df %>%
      arrange(form, desc(days_since)) %>%
      transmute(
        `Study ID` = study_id,
        Facility = facility,
        Form = form,
        `Reference Date` = format(reference_date, "%d %b %Y"),
        `Days Since` = round(days_since),
        Status = status
      )

    datatable(out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(names(STATUS_BG), unname(STATUS_BG)),
        color = styleEqual(names(STATUS_FG), unname(STATUS_FG)),
        fontWeight = "bold"
      )
  })

  # ---- Missing TEACH Outcomes tab ----
  missing_outcomes_scoped <- reactive({
    mf <- live_data()$missing_forms
    ids <- baseline_filtered()$study_id
    if (nrow(mf) == 0 || length(ids) == 0) return(mf[0, ])
    mf %>% filter(study_id %in% ids, form %in% STAGE2_FORMS)
  })

  missing_outcomes_filtered <- reactive({
    df <- missing_outcomes_scoped()
    if (nrow(df) == 0) return(df)
    if (input$mo_form != "All") df <- df %>% filter(form == input$mo_form)
    if (input$mo_status != "All") df <- df %>% filter(status == input$mo_status)
    if (input$mo_tbmeds != "All") df <- df %>% filter(issue_of_tb_meds == input$mo_tbmeds)
    df
  })

  output$mo_total <- renderText({
    format(nrow(missing_outcomes_filtered()), big.mark = ",")
  })

  output$missing_outcomes_table <- renderDT({
    df <- missing_outcomes_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No missing TEACH Outcomes forms match the current filters"), rownames = FALSE))
    }

    out <- df %>%
      arrange(form, desc(days_since)) %>%
      transmute(
        `Study ID` = study_id,
        Facility = facility,
        Form = form,
        `Reference Date (tbrx_start_date)` = format(reference_date, "%d %b %Y"),
        `Days Since` = round(days_since),
        Status = status,
        `Issue of TB Meds (Missing)` = ifelse(is.na(issue_of_tb_meds), "--", issue_of_tb_meds)
      )

    datatable(out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(names(STATUS_BG), unname(STATUS_BG)),
        color = styleEqual(names(STATUS_FG), unname(STATUS_FG)),
        fontWeight = "bold"
      )
  })
}

shinyApp(ui, server)
