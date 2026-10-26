# ==============================================================================
# TEACH Recruitment Dashboard (LIVE)
# ==============================================================================
# BEFORE RUNNING: open redcap_connect.R and check REDCAP_URL / REDCAP_TOKEN.
#
# CHANGE LOG (Oct 2026):
#   - Missing Forms + Missing TEACH Outcomes tabs are now filtered by a
#     Treatment Start Date (tbrx_start_date) range instead of screening date.
#   - TEACH Outcomes forms use scheduled due dates (see redcap_connect.R).
#   - "Total Missing" counts exclude "Not yet Due" rows.
#   - NEW Close-out tab (Died / Transferred out / Lost to follow-up /
#     Not consented >30 days). Sidebar "Hide closed-out participants" removes
#     them from both Missing tabs.
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

compute_cascade_counts <- function(df) {
  c(
    "Cascade Step 1 Screened"  = sum(df$screening_form_complete == "Complete", na.rm = TRUE),
    "Cascade Step 2 Eligible"  = sum(df$eligibility_status == "Yes", na.rm = TRUE),
    "Cascade Step 3 Consented" = sum(df$consent == "Yes", na.rm = TRUE),
    "Cascade Step 4 Enrolled"  = sum(df$enrollment_form_complete == "Complete", na.rm = TRUE),
    "Cascade Step 5 SBQ Done"  = sum(df$sbq_form_complete == "Complete", na.rm = TRUE)
  )
}

# Count of rows that are actually missing (i.e. not "Not yet Due")
count_missing <- function(df) {
  if (nrow(df) == 0) return(0L)
  sum(!(df$status %in% NOT_MISSING_STATUSES))
}
count_not_yet_due <- function(df) {
  if (nrow(df) == 0) return(0L)
  sum(df$status %in% NOT_MISSING_STATUSES)
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
    p(class = "text-muted small mt-n2",
      "Applies to Home, Cascade and Enrolled tabs."),
    dateRangeInput("f_tbrx_range", "Treatment Start Date Range (tbrx_start_date)",
                    start = Sys.Date() - 730, end = Sys.Date()),
    checkboxInput("f_tbrx_include_na", "Include participants with no treatment start date", value = TRUE),
    checkboxInput("f_hide_closed", "Hide closed-out participants (see Close-out tab)", value = TRUE),
    p(class = "text-muted small mt-n2",
      "Applies to Missing Forms and Missing TEACH Outcomes tabs."),
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
      "Uses the Facility and Treatment Start Date Range filters in the sidebar, plus the Form and Status filters below. ",
      "Total Missing excludes forms that are Not yet Due."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("mf_form", "Form", choices = c("All", STAGE1_FORMS), selected = "All"),
      selectInput("mf_status", "Status", choices = c("All", ALL_STATUSES), selected = "All"),
      value_box(title = "Total Missing Forms", value = textOutput("mf_total"),
                p(class = "small mb-0", textOutput("mf_notyet", inline = TRUE)),
                showcase = icon("list-check")),
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
      "Uses the Facility and Treatment Start Date Range filters in the sidebar, plus the filters below. ",
      "Each form has a due date counted from tbrx_start_date (TB refill: start of each period, e.g. 5-6 weeks is due at 5 weeks; ",
      "Lab: Month 2 and Month 5; ART: monthly). Before the due date a form is Not yet Due; after it, ",
      "Due (0-14 days past due), Almost Overdue (15-30) and Overdue (>30). Total Missing excludes Not yet Due."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("mo_form", "Form", choices = c("All", STAGE2_FORMS), selected = "All"),
      selectInput("mo_status", "Status", choices = c("All", STAGE2_STATUSES), selected = "All"),
      selectInput("mo_tbmeds", "Issue of TB Meds (TB Treatment only)", choices = c("All", TB_MEDS_CHOICES), selected = "All"),
      value_box(title = "Total Missing", value = textOutput("mo_total"),
                p(class = "small mb-0", textOutput("mo_notyet", inline = TRUE)),
                showcase = icon("list-check"))
    ),
    card(
      card_header("Missing TEACH Outcomes line list"),
      DTOutput("missing_outcomes_table")
    )
  ),

  nav_panel(
    "Close-out",
    p(class = "text-muted small",
      "Participants who can no longer be followed up. First matching rule wins: ",
      "(1) Died -- recorded anywhere (Dead at Baseline / Month 2 / Month 5 SBQ, passed on before consent, date of death, TB treatment outcome Died); ",
      "(2) Transferred out / Lost to follow-up from participant status, latest visit first (Month 5, Month 2, Baseline); ",
      "(3) Transferred out on any TB refill visit; (4) Transferred out given as reason for not consenting; ",
      "(5) Not consented -- eligible with no consent 30+ days after screening. ",
      "Uses the Facility filter only. When 'Hide closed-out participants' is ticked in the sidebar, ",
      "these participants are removed from both Missing tabs."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("co_vb_died"),
      uiOutput("co_vb_transfer"),
      uiOutput("co_vb_ltfu"),
      uiOutput("co_vb_noconsent")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("co_outcome", "Outcome", choices = c("All", CLOSEOUT_OUTCOMES), selected = "All"),
      checkboxInput("co_only_open", "Only those still on a Missing list", value = FALSE),
      value_box(title = "Total closed out", value = textOutput("co_total"),
                p(class = "small mb-0", textOutput("co_open_note", inline = TRUE)),
                showcase = icon("user-xmark"), theme = "secondary"),
      div(class = "d-flex align-items-center",
          downloadButton("co_download", "Download close-out list (CSV)"))
    ),
    card(
      card_header("Close-out line list"),
      DTOutput("closeout_table")
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

    valid_tbrx <- na.omit(df$tbrx_start_date)
    if (length(valid_tbrx) > 0) {
      # end at today (or later) so newly enrolled participants aren't cut off
      updateDateRangeInput(session, "f_tbrx_range",
                            start = min(valid_tbrx),
                            end = max(max(valid_tbrx), Sys.Date()))
    }
  }, once = TRUE)

  baseline_filtered <- reactive({
    df <- live_data()$baseline
    if (nrow(df) == 0) return(df)
    if (input$f_facility != "All") df <- df %>% filter(facility == input$f_facility)
    df <- df %>% filter(screening_date >= input$f_daterange[1], screening_date <= input$f_daterange[2])
    df
  })

  # Participants in scope for the two Missing tabs: Facility filter +
  # Treatment Start Date Range (tbrx_start_date), NOT screening date.
  missing_scope_ids <- reactive({
    df <- live_data()$baseline
    if (nrow(df) == 0) return(character(0))
    if (input$f_facility != "All") df <- df %>% filter(facility == input$f_facility)
    rng <- input$f_tbrx_range
    in_range <- !is.na(df$tbrx_start_date) &
      df$tbrx_start_date >= rng[1] & df$tbrx_start_date <= rng[2]
    keep <- in_range | (isTRUE(input$f_tbrx_include_na) & is.na(df$tbrx_start_date))
    ids <- df$study_id[keep]
    if (isTRUE(input$f_hide_closed)) ids <- setdiff(ids, live_data()$closeout$study_id)
    ids
  })

  # ---- Home tab header ----
  output$header_line <- renderText({
    d <- live_data()
    paste0("TEACH -- Data as at ", format(Sys.Date(), "%d %b %Y"),
           " | N = ", d$n_total, " participants")
  })

  # ---- Home KPIs ----
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

  kpi_color <- list(
    screened             = "success",
    consent_missing      = "danger",
    eligible             = "success",
    ineligible           = "danger",
    consent_yes          = "success",
    consent_no           = "danger",
    enrolled             = "success",
    sbq_baseline         = "success",
    sbq_baseline_missing = "danger",
    sbq_m2               = "success",
    sbq_m5               = "success"
  )

  kpi_meta <- list(
    screened             = list(title = "Notified from the Register",   icon = "clipboard-check"),
    consent_missing      = list(title = "Waiting to Consent",           icon = "hourglass-half"),
    eligible             = list(title = "Eligible from Screening",      icon = "circle-check"),
    ineligible           = list(title = "Ineligible from Screening",    icon = "circle-xmark"),
    consent_yes          = list(title = "Consented Yes",                icon = "signature"),
    consent_no           = list(title = "Consented No",                 icon = "ban"),
    enrolled             = list(title = "Enrollment Completed",         icon = "user-plus"),
    sbq_baseline         = list(title = "Baseline SBQ Completed",       icon = "list-check"),
    sbq_baseline_missing = list(title = "Baseline SBQ NOT Completed",   icon = "hourglass-half"),
    sbq_m2               = list(title = "SBQ Completed Month 2",        icon = "calendar-check"),
    sbq_m5               = list(title = "SBQ Completed Month 5",        icon = "calendar-check")
  )

  render_kpi_box <- function(key) {
    force(key)
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
        `Baseline SBQ Done`        = sum(sbq_form_complete == "Complete", na.rm = TRUE),
        .groups = "drop"
      )

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

  # ---- By Health Facility tabs ----
  enrolled_filtered <- reactive({
    df <- baseline_filtered()
    if (nrow(df) == 0) return(df)
    df %>% filter(
      eligibility_status == "Yes",
      consent == "Yes",
      enrollment_form_complete == "Complete"
    )
  })

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
  missing_forms_scoped <- reactive({
    mf <- live_data()$missing_forms
    ids <- missing_scope_ids()
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
    format(count_missing(missing_forms_filtered()), big.mark = ",")
  })

  output$mf_notyet <- renderText({
    paste0("Not yet due (excluded): ", format(count_not_yet_due(missing_forms_filtered()), big.mark = ","))
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
        `TB Rx Start Date` = format(tbrx_start_date, "%d %b %Y"),
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
    ids <- missing_scope_ids()
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
    format(count_missing(missing_outcomes_filtered()), big.mark = ",")
  })

  output$mo_notyet <- renderText({
    paste0("Not yet due (excluded): ", format(count_not_yet_due(missing_outcomes_filtered()), big.mark = ","))
  })

  output$missing_outcomes_table <- renderDT({
    df <- missing_outcomes_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No missing TEACH Outcomes forms match the current filters"), rownames = FALSE))
    }

    out <- df %>%
      arrange(form, desc(days_past_due)) %>%
      transmute(
        `Study ID` = study_id,
        Facility = facility,
        Form = form,
        `Expected At` = expected_at,
        `TB Rx Start Date` = format(reference_date, "%d %b %Y"),
        `Due Date` = format(due_date, "%d %b %Y"),
        `Days Since TB Rx Start` = round(days_since),
        `Days Past Due` = round(days_past_due),
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

  # ---- Close-out tab ----
  # Open (actually missing, not "Not yet Due") forms per participant
  open_missing_by_id <- reactive({
    mf <- live_data()$missing_forms
    if (nrow(mf) == 0) return(tibble(study_id = character(0), open_n = integer(0), open_forms = character(0)))
    if (!"expected_at" %in% names(mf)) mf$expected_at <- NA_character_
    mf %>%
      filter(!is.na(status), !(status %in% NOT_MISSING_STATUSES)) %>%
      mutate(label = ifelse(!is.na(expected_at) & form %in% STAGE2_FORMS,
                            paste0(form, " (", expected_at, ")"), form)) %>%
      group_by(study_id) %>%
      summarise(open_n = n(), open_forms = paste(label, collapse = "; "), .groups = "drop")
  })

  closeout_facility <- reactive({
    co <- live_data()$closeout
    if (nrow(co) == 0) return(co %>% mutate(open_n = integer(0), open_forms = character(0)))
    if (input$f_facility != "All") co <- co %>% filter(facility == input$f_facility)
    co %>%
      left_join(open_missing_by_id(), by = "study_id") %>%
      mutate(open_n = coalesce(open_n, 0L))
  })

  closeout_filtered <- reactive({
    co <- closeout_facility()
    if (nrow(co) == 0) return(co)
    if (input$co_outcome != "All") co <- co %>% filter(outcome == input$co_outcome)
    if (isTRUE(input$co_only_open)) co <- co %>% filter(open_n > 0)
    co
  })

  co_box <- function(outcome_label, title, icon_name, theme) {
    renderUI({
      n <- sum(closeout_facility()$outcome == outcome_label, na.rm = TRUE)
      value_box(title = title, value = format(n, big.mark = ","),
                showcase = icon(icon_name), theme = theme)
    })
  }
  output$co_vb_died      <- co_box("Died", "Died", "cross", "dark")
  output$co_vb_transfer  <- co_box("Transferred out", "Transferred out", "right-from-bracket", "info")
  output$co_vb_ltfu      <- co_box("Lost to follow-up", "Lost to follow-up", "user-slash", "warning")
  output$co_vb_noconsent <- co_box("Not consented (>30 days)", "Not consented (>30 days)", "ban", "danger")

  output$co_total <- renderText({
    format(nrow(closeout_filtered()), big.mark = ",")
  })

  output$co_open_note <- renderText({
    co <- closeout_filtered()
    paste0("Still on a Missing list: ",
           format(if (nrow(co) == 0) 0 else sum(co$open_n > 0), big.mark = ","))
  })

  closeout_display <- reactive({
    co <- closeout_filtered()
    if (nrow(co) == 0) return(NULL)
    co %>%
      arrange(outcome, facility, study_id) %>%
      transmute(
        `Study ID` = study_id,
        Facility = facility,
        Outcome = outcome,
        `Recorded At` = recorded_at,
        `Outcome Date` = format(outcome_date, "%d %b %Y"),
        `Screening Date` = format(screening_date, "%d %b %Y"),
        `TB Rx Start Date` = format(tbrx_start_date, "%d %b %Y"),
        `Open Missing Forms` = open_n,
        `Forms to Close` = coalesce(open_forms, "")
      )
  })

  output$closeout_table <- renderDT({
    out <- closeout_display()
    if (is.null(out)) {
      return(datatable(data.frame(Message = "No closed-out participants match the current filters"), rownames = FALSE))
    }
    co_bg <- c("Died" = "#343a40", "Transferred out" = "#17a2b8",
               "Lost to follow-up" = "#ffc107", "Not consented (>30 days)" = "#dc3545")
    co_fg <- c("Died" = "white", "Transferred out" = "white",
               "Lost to follow-up" = "black", "Not consented (>30 days)" = "white")
    datatable(out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("Outcome",
                  backgroundColor = styleEqual(names(co_bg), unname(co_bg)),
                  color = styleEqual(names(co_fg), unname(co_fg)),
                  fontWeight = "bold")
  })

  output$co_download <- downloadHandler(
    filename = function() paste0("TEACH_closeout_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      out <- closeout_display()
      if (is.null(out)) out <- data.frame(Message = "No closed-out participants")
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
