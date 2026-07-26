# ==============================================================================
# TEACH Dashboard -- Live REDCap connection
# ==============================================================================
# Pulls ALL records (every event, every repeating instrument) from REDCap,
# then reproduces the same derived fields as the Power Query M script:
#   - calc_days_since_screening/consent/enrollment
#   - calc_consent_bucket / calc_enrollment_bucket / calc_sbq_bucket
#   - missing_consent_n / missing_enrollment_n / missing_sbq_n
#   - calc_status (the 7-stage recruitment cascade label)
#   - screening_week (Monday-anchored) / screening_month
#   - clinic -> facility rename
#
# SCOPE NOTE: like the original M query, the cascade/status/bucket logic is
# computed from BASELINE-event rows only (one row per participant for those
# milestones). Month 2 / Month 5 SBQ completion are pulled separately from (REDCap event names still say "Month_Six_followup" internally -- only the display label changed to Month 5)
# their own events and joined back by study_id -- the original query didn't
# do this (it only read Baseline), which is why those two Home cards were
# always blank. This version fixes that.
# ==============================================================================

library(curl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------
REDCAP_URL   <- Sys.getenv("REDCAP_URL",   unset = "https://utirc.mak.ac.ug:8181/api/")
REDCAP_TOKEN <- Sys.getenv("REDCAP_TOKEN", unset = "DB3A6783D453629A94F96AE06C799A45")

REFRESH_SECONDS <- 60  # how often the dashboard re-polls REDCap

# ------------------------------------------------------------------------------
# Fetch ALL records from REDCap (flat export, labeled values)
# ------------------------------------------------------------------------------
redcap_fetch_raw <- function() {
  body <- paste0(
    "token=", REDCAP_TOKEN,
    "&content=record",
    "&format=json",
    "&type=flat",
    "&rawOrLabel=label",
    "&rawOrLabelHeaders=raw",
    "&exportCheckboxLabel=true",
    "&exportSurveyFields=true",
    "&exportDataAccessGroups=false",
    "&returnFormat=json"
  )

  h <- new_handle()
  handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
  handle_setopt(h, postfields = body)

  resp <- tryCatch(curl_fetch_memory(REDCAP_URL, handle = h), error = function(e) {
    warning("REDCap API connection failed: ", e$message)
    NULL
  })
  if (is.null(resp)) return(NULL)

  if (resp$status_code >= 400) {
    warning("REDCap API request failed with status ", resp$status_code, ": ",
            rawToChar(resp$content))
    return(NULL)
  }

  txt <- rawToChar(resp$content)
  if (identical(str_trim(txt), "")) return(data.frame())

  fromJSON(txt, simplifyVector = TRUE, flatten = TRUE)
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
blank_to_na <- function(x) ifelse(is.na(x) | x == "", NA, x)

parse_redcap_date <- function(x) {
  suppressWarnings(as_date(blank_to_na(x)))
}

# ------------------------------------------------------------------------------
# Apply the M-query-equivalent cleaning/derivation logic to the Baseline subset
# ------------------------------------------------------------------------------
build_baseline_table <- function(raw_all) {
  if (nrow(raw_all) == 0) return(data.frame())

  baseline <- raw_all %>%
    filter(
      redcap_event_name == "1. Baseline",
      is.na(redcap_repeat_instrument) | redcap_repeat_instrument == ""
    ) %>%
    mutate(
      # defensive remap in case eligibility_status ever comes back as 1/0
      eligibility_status = case_when(
        eligibility_status %in% c("1", "Yes") ~ "Yes",
        eligibility_status %in% c("0", "No")  ~ "No",
        TRUE ~ blank_to_na(eligibility_status)
      ),
      across(c(consent, consent_form_complete, enrollment_form_complete,
               screening_form_complete, sbq_form_complete, clinic,
               hiv_status, started_art),
             blank_to_na),
      screening_date     = parse_redcap_date(screening_date),
      date_of_consent     = parse_redcap_date(date_of_consent),
      date_of_enrollment  = parse_redcap_date(date_of_enrollment),
      tbrx_start_date     = parse_redcap_date(tbrx_start_date)
    ) %>%
    mutate(
      calc_days_since_screening  = as.numeric(Sys.Date() - screening_date),
      calc_days_since_consent    = as.numeric(Sys.Date() - date_of_consent),
      calc_days_since_enrollment = as.numeric(Sys.Date() - date_of_enrollment),
      calc_days_since_tbrx       = as.numeric(Sys.Date() - tbrx_start_date)
    ) %>%
    mutate(
      calc_consent_bucket = case_when(
        eligibility_status == "Yes" & is.na(consent) &
          calc_days_since_screening <= 14 ~ "Consent Due Soon (<=14d)",
        eligibility_status == "Yes" & is.na(consent) &
          calc_days_since_screening <= 30 ~ "Consent Overdue (15-30d)",
        eligibility_status == "Yes" & is.na(consent) ~ "To Be Dropped (>30d)",
        consent == "Yes" ~ "Consented: Yes",
        consent == "No"  ~ "Consented: No",
        TRUE ~ "Other/NA"
      ),
      calc_enrollment_bucket = case_when(
        consent == "Yes" & (is.na(enrollment_form_complete) | enrollment_form_complete != "Complete") &
          calc_days_since_consent <= 14 ~ "Enrollment Due Soon (<=14d)",
        consent == "Yes" & (is.na(enrollment_form_complete) | enrollment_form_complete != "Complete") &
          calc_days_since_consent <= 30 ~ "Enrollment Overdue (15-30d)",
        consent == "Yes" & (is.na(enrollment_form_complete) | enrollment_form_complete != "Complete") ~
          "Enrollment >30d (To Be Dropped)",
        enrollment_form_complete == "Complete" ~ "Enrolled: Complete",
        consent == "No" ~ "Consent = No (Stop)",
        TRUE ~ "Consent Missing/NA"
      ),
      calc_sbq_bucket = case_when(
        enrollment_form_complete == "Complete" & (is.na(sbq_form_complete) | sbq_form_complete != "Complete") &
          calc_days_since_enrollment <= 14 ~ "SBQ Due Soon (<=14d)",
        enrollment_form_complete == "Complete" & (is.na(sbq_form_complete) | sbq_form_complete != "Complete") &
          calc_days_since_enrollment <= 30 ~ "SBQ Overdue (15-30d)",
        enrollment_form_complete == "Complete" & (is.na(sbq_form_complete) | sbq_form_complete != "Complete") ~
          "SBQ >30d (To Be Dropped)",
        sbq_form_complete == "Complete" ~ "SBQ: Complete",
        !is.na(enrollment_form_complete) & enrollment_form_complete != "Complete" ~ "Not Enrolled Yet",
        TRUE ~ "SBQ: Incomplete/NA"
      ),
      missing_consent_n = as.integer(eligibility_status == "Yes" & is.na(consent)),
      missing_enrollment_n = as.integer(
        consent == "Yes" & (is.na(enrollment_form_complete) | enrollment_form_complete != "Complete")
      ),
      missing_sbq_n = as.integer(
        enrollment_form_complete == "Complete" & (is.na(sbq_form_complete) | sbq_form_complete != "Complete")
      ),
      calc_status = case_when(
        sbq_form_complete == "Complete"             ~ "5 - SBQ Complete",
        enrollment_form_complete == "Complete"       ~ "4 - Enrolled (SBQ Pending)",
        consent == "Yes"                             ~ "3 - Consented (Enrollment Pending)",
        consent == "No"                              ~ "2b - Refused Consent",
        eligibility_status == "No"                   ~ "2a - Ineligible",
        screening_form_complete == "Complete"        ~ "1 - Screened (Consent Pending)",
        TRUE                                          ~ "0 - Screening Incomplete"
      ),
      screening_week = if_else(is.na(screening_date), as.Date(NA),
                                floor_date(screening_date, unit = "week", week_start = 1)),
      screening_month = if_else(is.na(screening_date), NA_character_,
                                 paste0(year(screening_date), " M", sprintf("%02d", month(screening_date))))
    ) %>%
    rename(facility = clinic)

  # replace NA flags with 0 (case_when above can leave NA if the date diff was NA)
  baseline$missing_consent_n[is.na(baseline$missing_consent_n)] <- 0L
  baseline$missing_enrollment_n[is.na(baseline$missing_enrollment_n)] <- 0L
  baseline$missing_sbq_n[is.na(baseline$missing_sbq_n)] <- 0L

  baseline
}

# ------------------------------------------------------------------------------
# Month 2 / Month 5 SBQ completion (separate events, joined back by study_id).
# Note: REDCap's actual event name for the second one is "3. Month_Six_followup"
# -- only the dashboard's display label was changed to "Month 5".
# ------------------------------------------------------------------------------
build_followup_sbq <- function(raw_all, event_name) {
  if (nrow(raw_all) == 0) return(data.frame(study_id = character(0), sbq_form_complete = character(0)))
  raw_all %>%
    filter(
      redcap_event_name == event_name,
      is.na(redcap_repeat_instrument) | redcap_repeat_instrument == ""
    ) %>%
    transmute(study_id, sbq_form_complete = blank_to_na(sbq_form_complete))
}

# ------------------------------------------------------------------------------
# TEACH Outcomes repeating instruments (event "4. Teach_outcomes"): Lab,
# TB Medication Refill, ART Medication Refill. Each participant can have
# zero, one, or many instances of these over time. We just need, per
# study_id, whether AT LEAST ONE complete instance exists.
# ------------------------------------------------------------------------------
build_repeat_form_status <- function(raw_all, instrument_name, complete_field) {
  if (nrow(raw_all) == 0) {
    return(data.frame(study_id = character(0), complete_flag = character(0)))
  }
  raw_all %>%
    filter(redcap_repeat_instrument == instrument_name) %>%
    transmute(study_id, complete_flag = blank_to_na(.data[[complete_field]]))
}

# TB Medication Refill also carries issue_of_tb_meds (which refill-period
# this particular submission covers) -- used later as an extra filter, keyed
# off each participant's MOST RECENT complete submission.
build_tb_refill_status <- function(raw_all) {
  if (nrow(raw_all) == 0) {
    return(data.frame(study_id = character(0), complete_flag = character(0),
                       issue_of_tb_meds = character(0)))
  }
  raw_all %>%
    filter(redcap_repeat_instrument == "Form 6: TB Medication Refill Form") %>%
    transmute(
      study_id,
      complete_flag = blank_to_na(tb_medication_refill_form_complete),
      issue_of_tb_meds = blank_to_na(issue_of_tb_meds),
      repeat_instance = suppressWarnings(as.numeric(redcap_repeat_instance))
    )
}

# Maps days-since-tbrx_start_date to which issue_of_tb_meds period should
# currently be in progress, per the confirmed refill schedule. Returns NA
# for days 0-6 (nothing due yet -- the first period window starts day 7).
map_days_to_tb_period <- function(days) {
  case_when(
    is.na(days)  ~ NA_character_,
    days < 7     ~ NA_character_,
    days <= 14   ~ "1-2 weeks",
    days <= 28   ~ "3-4 weeks",
    days <= 42   ~ "5-6 weeks",
    days <= 56   ~ "7-8 weeks",
    days <= 90   ~ "3 months",
    days <= 120  ~ "4 months",
    days <= 150  ~ "5 months",
    days <= 180  ~ "6 months",
    days <= 210  ~ "7 months",
    days <= 240  ~ "8 months",
    days <= 270  ~ "9 months",
    days <= 330  ~ "11 months",
    TRUE         ~ "12 months"
  )
}

# ------------------------------------------------------------------------------
# Missing Form line list
# ------------------------------------------------------------------------------
# Five form types, each with: who it applies to, what "missing" means, which
# date the day-count is measured from, and a day-banded status. See the
# classification thresholds the user specified:
#
#  1. Consent      -- eligible & consent blank.        Days since screening.
#  2. Enrollment    -- eligible & consent=Yes & enrollment not Complete. Days since consent.
#  3. Baseline SBQ  -- eligible & consent=Yes & baseline SBQ not Complete. Days since consent.
#  4. Month 2 SBQ   -- eligible & consent=Yes & enrollment Complete & baseline
#                      SBQ Complete & Month 2 SBQ not Complete. Days since enrollment.
#  5. Month 5 SBQ   -- same preconditions as Month 2, but for the Month 5 (REDCap
#                      "Month_Six_followup") event. Days since enrollment.
#
# Statuses 1-3 use a 3-tier scale (Due / Almost Overdue / Overdue).
# Statuses 4-5 use a 5-tier scale (Not yet Due / Due within 2 weeks /
# Due this week / Still Due / Overdue) since they track a much longer window.
# ------------------------------------------------------------------------------

classify_3tier <- function(days) {
  case_when(
    is.na(days)  ~ NA_character_,
    days <= 14   ~ "Due",
    days <= 30   ~ "Almost Overdue",
    TRUE         ~ "Overdue"
  )
}

classify_month2 <- function(days) {
  case_when(
    is.na(days)  ~ NA_character_,
    days <= 44   ~ "Not yet Due",
    days <= 59   ~ "Due within 2 weeks",
    days <= 67   ~ "Due this week",
    days <= 120  ~ "Still Due",
    TRUE         ~ "Overdue"
  )
}

classify_month5 <- function(days) {
  case_when(
    is.na(days)  ~ NA_character_,
    days <= 135  ~ "Not yet Due",
    days <= 149  ~ "Due within 2 weeks",
    days <= 157  ~ "Due this week",
    days <= 210  ~ "Still Due",
    TRUE         ~ "Overdue"
  )
}

# Maps each status label to a Bootstrap contextual color name (used for
# row-highlighting in the Shiny table): success=green, warning=yellow,
# danger=red, secondary=grey.
STATUS_COLOR_MAP <- c(
  "Due"                 = "success",
  "Almost Overdue"      = "warning",
  "Overdue"             = "danger",
  "Not yet Due"         = "secondary",
  "Due within 2 weeks"  = "success",
  "Due this week"       = "success",
  "Still Due"           = "warning"
)

# Solid hex colors + matching text color for the DT status badges (shared by
# both the Missing Forms and Missing TEACH Outcomes tables).
STATUS_BG <- c(
  "Due"                 = "#28a745",  # green
  "Due within 2 weeks"  = "#28a745",  # green
  "Due this week"       = "#28a745",  # green
  "Almost Overdue"      = "#ffc107",  # yellow
  "Still Due"           = "#ffc107",  # yellow
  "Overdue"             = "#dc3545",  # red
  "Not yet Due"         = "#6c757d"   # grey
)
STATUS_FG <- c(
  "Due"                 = "white",
  "Due within 2 weeks"  = "white",
  "Due this week"       = "white",
  "Almost Overdue"      = "black",
  "Still Due"           = "black",
  "Overdue"             = "white",
  "Not yet Due"         = "white"
)

# All possible statuses across every form type -- used to populate the
# Status filter dropdown so every option is always selectable, even if no
# rows currently match it.
ALL_STATUSES <- names(STATUS_COLOR_MAP)

# Stage 1 = the original "Missing Forms" screen (Consent through Month 5 SBQ)
# Stage 2 = the new "Missing TEACH Outcomes" screen (Lab / TB / ART Refill)
STAGE1_FORMS <- c("Consent", "Enrollment", "Baseline SBQ", "Month 2 SBQ", "Month 5 SBQ")
STAGE2_FORMS <- c("Lab", "TB Treatment", "ART Refill")
ALL_FORMS <- c(STAGE1_FORMS, STAGE2_FORMS)

# Stage 2 forms only ever use the simple 3-tier scale (no "Not yet Due" /
# "Due within 2 weeks" / "Due this week" / "Still Due" -- those are Month
# 2/5 SBQ-specific), so its Status filter only offers the 3 relevant options.
STAGE2_STATUSES <- c("Due", "Almost Overdue", "Overdue")

# Exact choice labels from the REDCap data dictionary for issue_of_tb_meds --
# used for the TB Treatment-only filter on the Missing TEACH Outcomes screen.
TB_MEDS_CHOICES <- c(
  "1-2 weeks", "3-4 weeks", "5-6 weeks", "7-8 weeks",
  "3 months", "4 months", "5 months", "6 months", "7 months",
  "8 months", "9 months", "11 months", "12 months"
)

build_missing_form_line_list <- function(baseline, month2_sbq, month5_sbq,
                                          lab_status, tb_refill_status, art_refill_status) {
  if (nrow(baseline) == 0) {
    return(data.frame(
      study_id = character(0), facility = character(0), form = character(0),
      reference_date = as.Date(character(0)), days_since = numeric(0),
      status = character(0), issue_of_tb_meds = character(0)
    ))
  }

  m2_complete_ids <- month2_sbq$study_id[!is.na(month2_sbq$sbq_form_complete) & month2_sbq$sbq_form_complete == "Complete"]
  m5_complete_ids <- month5_sbq$study_id[!is.na(month5_sbq$sbq_form_complete) & month5_sbq$sbq_form_complete == "Complete"]
  lab_complete_ids <- lab_status$study_id[!is.na(lab_status$complete_flag) & lab_status$complete_flag == "Complete"]
  art_complete_ids <- art_refill_status$study_id[!is.na(art_refill_status$complete_flag) & art_refill_status$complete_flag == "Complete"]

  # (study_id, issue_of_tb_meds) pairs where THAT SPECIFIC period's refill
  # has already been completed -- used to check whether the currently-due
  # period (per the schedule) has been submitted yet, not just whether any
  # refill exists at all.
  tb_complete_pairs <- tb_refill_status %>%
    filter(!is.na(complete_flag), complete_flag == "Complete", !is.na(issue_of_tb_meds)) %>%
    distinct(study_id, issue_of_tb_meds)

  consent_missing <- baseline %>%
    filter(eligibility_status == "Yes", is.na(consent)) %>%
    transmute(study_id, facility, form = "Consent",
              reference_date = screening_date,
              days_since = calc_days_since_screening,
              status = classify_3tier(days_since))

  enrollment_missing <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           is.na(enrollment_form_complete) | enrollment_form_complete != "Complete") %>%
    transmute(study_id, facility, form = "Enrollment",
              reference_date = date_of_consent,
              days_since = calc_days_since_consent,
              status = classify_3tier(days_since))

  baseline_sbq_missing <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           is.na(sbq_form_complete) | sbq_form_complete != "Complete") %>%
    transmute(study_id, facility, form = "Baseline SBQ",
              reference_date = date_of_consent,
              days_since = calc_days_since_consent,
              status = classify_3tier(days_since))

  month2_missing <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           enrollment_form_complete == "Complete",
           sbq_form_complete == "Complete",
           !(study_id %in% m2_complete_ids)) %>%
    transmute(study_id, facility, form = "Month 2 SBQ",
              reference_date = date_of_enrollment,
              days_since = calc_days_since_enrollment,
              status = classify_month2(days_since))

  month5_missing <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           enrollment_form_complete == "Complete",
           sbq_form_complete == "Complete",
           !(study_id %in% m5_complete_ids)) %>%
    transmute(study_id, facility, form = "Month 5 SBQ",
              reference_date = date_of_enrollment,
              days_since = calc_days_since_enrollment,
              status = classify_month5(days_since))

  # ---- TEACH Outcomes forms (all reference tbrx_start_date, simple 3-tier) ----
  # "Applies to Eligible participant who consented and were enrolled"
  outcomes_base_eligible <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           enrollment_form_complete == "Complete",
           !is.na(tbrx_start_date))

  lab_missing <- outcomes_base_eligible %>%
    filter(!(study_id %in% lab_complete_ids)) %>%
    transmute(study_id, facility, form = "Lab",
              reference_date = tbrx_start_date,
              days_since = calc_days_since_tbrx,
              status = classify_3tier(days_since))

  # TB Treatment: figure out which issue_of_tb_meds period should currently
  # be in progress (based on days since tbrx_start_date and the confirmed
  # schedule), then flag participants who don't have a COMPLETE refill
  # submission tagged with that specific period yet. The displayed
  # "issue_of_tb_meds" is therefore the period that's actually missing, not
  # just their last completed one.
  tb_missing <- outcomes_base_eligible %>%
    mutate(current_tb_period = map_days_to_tb_period(calc_days_since_tbrx)) %>%
    filter(!is.na(current_tb_period)) %>%
    anti_join(tb_complete_pairs, by = c("study_id" = "study_id", "current_tb_period" = "issue_of_tb_meds")) %>%
    transmute(study_id, facility, form = "TB Treatment",
              reference_date = tbrx_start_date,
              days_since = calc_days_since_tbrx,
              status = classify_3tier(days_since),
              issue_of_tb_meds = current_tb_period)

  # ART Refill: additionally requires HIV positive + on ART
  art_missing <- outcomes_base_eligible %>%
    filter(hiv_status == "Positive", started_art == "Yes",
           !(study_id %in% art_complete_ids)) %>%
    transmute(study_id, facility, form = "ART Refill",
              reference_date = tbrx_start_date,
              days_since = calc_days_since_tbrx,
              status = classify_3tier(days_since))

  bind_rows(consent_missing, enrollment_missing, baseline_sbq_missing,
            month2_missing, month5_missing,
            lab_missing, tb_missing, art_missing)
}

# ------------------------------------------------------------------------------
# Single entry point app.R calls
# ------------------------------------------------------------------------------
load_live_data <- function() {
  raw <- tryCatch(redcap_fetch_raw(), error = function(e) {
    warning("Failed to fetch from REDCap: ", e$message)
    NULL
  })
  if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) {
    return(list(
      baseline = data.frame(),
      month2_sbq = data.frame(),
      month5_sbq = data.frame(),
      missing_forms = data.frame(),
      n_total = 0
    ))
  }

  baseline_tbl <- build_baseline_table(raw)
  month2_tbl <- build_followup_sbq(raw, "2. Month_Two_followup")
  month5_tbl <- build_followup_sbq(raw, "3. Month_Six_followup")
  lab_tbl <- build_repeat_form_status(raw, "Form 5: Laboratory Tests Form", "laboratory_tests_form_complete")
  tb_refill_tbl <- build_tb_refill_status(raw)
  art_refill_tbl <- build_repeat_form_status(raw, "Form 7: ART Medication Refill Form", "art_form_complete")

  list(
    baseline      = baseline_tbl,
    month2_sbq    = month2_tbl,
    month5_sbq    = month5_tbl,
    missing_forms = build_missing_form_line_list(baseline_tbl, month2_tbl, month5_tbl,
                                                  lab_tbl, tb_refill_tbl, art_refill_tbl),
    n_total       = n_distinct(raw$study_id)
  )
}
