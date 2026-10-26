# ==============================================================================
# TEACH Dashboard -- Live REDCap connection
# ==============================================================================
# Pulls ALL records (every event, every repeating instrument) from REDCap,
# then reproduces the same derived fields as the Power Query M script:
#   - calc_days_since_screening/consent/enrollment/tbrx
#   - calc_consent_bucket / calc_enrollment_bucket / calc_sbq_bucket
#   - missing_consent_n / missing_enrollment_n / missing_sbq_n
#   - calc_status (the 7-stage recruitment cascade label)
#   - screening_week (Monday-anchored) / screening_month
#   - clinic -> facility rename
#
# SCOPE NOTE: like the original M query, the cascade/status/bucket logic is
# computed from BASELINE-event rows only (one row per participant for those
# milestones). Month 2 / Month 5 SBQ completion are pulled separately from
# their own events and joined back by study_id. (REDCap event names still say
# "Month_Six_followup" internally -- only the display label changed to Month 5.)
#
# CHANGE LOG (Oct 2026):
#   - TEACH Outcomes forms (TB Treatment, Lab, ART Refill) are now scheduled:
#     each expected form gets a DUE DATE counted from tbrx_start_date. Before
#     that date the form is "Not yet Due"; after it, Due / Almost Overdue /
#     Overdue are counted from the due date (0-14 / 15-30 / >30 days).
#   - Every line-list row now carries tbrx_start_date so the Missing Forms
#     screens can be filtered on treatment start date.
#   - NEW: build_closeout() derives a close-out outcome per participant
#     (Died / Transferred out / Lost to follow-up / Not consented >30 days).
#     Missing-form rows carry closeout_outcome so the app can hide them.
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

REFRESH_SECONDS <- 1800  # how often the dashboard re-polls REDCap

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
# zero, one, or many instances of these over time.
# ------------------------------------------------------------------------------
build_repeat_form_status <- function(raw_all, instrument_name, complete_field) {
  if (nrow(raw_all) == 0) {
    return(data.frame(study_id = character(0), complete_flag = character(0)))
  }
  raw_all %>%
    filter(redcap_repeat_instrument == instrument_name) %>%
    transmute(study_id, complete_flag = blank_to_na(.data[[complete_field]]))
}

# TB Medication Refill also carries issue_of_tb_meds (which refill period
# this particular submission covers) -- used to match each submission to
# its slot in the TB refill schedule.
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

# ------------------------------------------------------------------------------
# Status classification
# ------------------------------------------------------------------------------
# Stage 1 (Consent / Enrollment / Baseline SBQ) use a 3-tier scale on days
# since their reference date. Month 2 / Month 5 SBQ use a 5-tier scale.
# Stage 2 (TEACH Outcomes) use "Not yet Due" before the due date, then the
# same 3-tier scale counted from the due date.
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

# Maps each status label to a Bootstrap contextual color name.
STATUS_COLOR_MAP <- c(
  "Due"                 = "success",
  "Almost Overdue"      = "warning",
  "Overdue"             = "danger",
  "Not yet Due"         = "secondary",
  "Due within 2 weeks"  = "success",
  "Due this week"       = "success",
  "Still Due"           = "warning"
)

# Solid hex colors + matching text color for the DT status badges.
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

ALL_STATUSES <- names(STATUS_COLOR_MAP)

# Rows with this status are listed, but NOT counted as missing.
NOT_MISSING_STATUSES <- c("Not yet Due")

STAGE1_FORMS <- c("Consent", "Enrollment", "Baseline SBQ", "Month 2 SBQ", "Month 5 SBQ")
STAGE2_FORMS <- c("Lab", "TB Treatment", "ART Refill")
ALL_FORMS <- c(STAGE1_FORMS, STAGE2_FORMS)

STAGE2_STATUSES <- c("Not yet Due", "Due", "Almost Overdue", "Overdue")

# Exact choice labels from the REDCap data dictionary for issue_of_tb_meds.
TB_MEDS_CHOICES <- c(
  "1-2 weeks", "3-4 weeks", "5-6 weeks", "7-8 weeks",
  "3 months", "4 months", "5 months", "6 months", "7 months",
  "8 months", "9 months", "11 months", "12 months"
)

# ------------------------------------------------------------------------------
# TEACH Outcomes schedules -- when each form becomes due, counted from
# tbrx_start_date. A form is "Not yet Due" until its due date; after that
# the Due / Almost Overdue / Overdue clock starts from the due date.
#   unit = "days"   -> due_date = tbrx_start_date + n days
#   unit = "months" -> due_date = tbrx_start_date + n calendar months
# ------------------------------------------------------------------------------

# TB refill: each period is due at the START of its window
# (e.g. "5-6 weeks" is due once 5 weeks = 35 days have passed).
TB_SCHEDULE <- data.frame(
  period = TB_MEDS_CHOICES,
  unit   = c(rep("days", 4), rep("months", 9)),
  n      = c(7, 21, 35, 49, 3, 4, 5, 6, 7, 8, 9, 11, 12),
  stringsAsFactors = FALSE
)
TB_SCHEDULE$seq <- seq_len(nrow(TB_SCHEDULE))

# Lab: two timepoints, Month 2 and Month 5 of treatment.
LAB_SCHEDULE <- data.frame(
  period = c("Month 2", "Month 5"),
  unit   = "months",
  n      = c(2, 5),
  stringsAsFactors = FALSE
)
LAB_SCHEDULE$seq <- seq_len(nrow(LAB_SCHEDULE))

# ART refill: monthly, from Month 1 up to ART_MAX_MONTHS.
ART_MAX_MONTHS <- 12
ART_SCHEDULE <- data.frame(
  period = paste("Month", seq_len(ART_MAX_MONTHS)),
  unit   = "months",
  n      = seq_len(ART_MAX_MONTHS),
  stringsAsFactors = FALSE
)
ART_SCHEDULE$seq <- seq_len(nrow(ART_SCHEDULE))

# Lab and ART forms have no "which timepoint" field, so completed instances
# are matched to the schedule in order: the 1st complete instance covers the
# 1st slot (Lab Month 2 / ART Month 1), the 2nd covers the 2nd slot, etc.
count_to_completed_pairs <- function(status_tbl, schedule) {
  done <- status_tbl %>%
    filter(!is.na(complete_flag), complete_flag == "Complete") %>%
    count(study_id, name = "n_done")
  if (nrow(done) == 0) return(data.frame(study_id = character(0), period = character(0)))
  merge(done, schedule[, c("period", "seq")], by = NULL) %>%
    filter(seq <= n_done) %>%
    transmute(study_id, period)
}

# Expands each participant across a schedule, drops slots already completed,
# and classifies the rest. Returns every slot whose due date has passed, plus
# the NEXT upcoming slot (as "Not yet Due") so staff can see what's coming.
build_scheduled_missing <- function(base, schedule, completed_pairs, form_label) {
  if (nrow(base) == 0) return(NULL)
  today <- Sys.Date()

  merge(
    base %>% select(study_id, facility, tbrx_start_date, calc_days_since_tbrx),
    schedule, by = NULL
  ) %>%
    as_tibble() %>%
    anti_join(completed_pairs, by = c("study_id", "period")) %>%
    mutate(
      due_date = if_else(unit == "months",
                         tbrx_start_date %m+% months(as.integer(n)),
                         tbrx_start_date + days(as.integer(n))),
      days_past_due = as.numeric(today - due_date),
      status = if_else(days_past_due < 0, "Not yet Due", classify_3tier(days_past_due))
    ) %>%
    group_by(study_id) %>%
    mutate(next_seq = min(c(Inf, seq[days_past_due < 0]))) %>%
    ungroup() %>%
    filter(days_past_due >= 0 | seq == next_seq) %>%
    transmute(study_id, facility, form = form_label,
              reference_date = tbrx_start_date,
              days_since = calc_days_since_tbrx,
              status,
              expected_at = period,
              due_date,
              days_past_due)
}

# ------------------------------------------------------------------------------
# Missing Form line list
# ------------------------------------------------------------------------------
#  1. Consent      -- eligible & consent blank.        Days since screening.
#  2. Enrollment    -- eligible & consent=Yes & enrollment not Complete. Days since consent.
#  3. Baseline SBQ  -- eligible & consent=Yes & baseline SBQ not Complete. Days since consent.
#  4. Month 2 SBQ   -- enrolled & baseline SBQ Complete & Month 2 SBQ not Complete. Days since enrollment.
#  5. Month 5 SBQ   -- same, for the Month 5 ("Month_Six_followup") event.
#  6-8. Lab / TB Treatment / ART Refill -- scheduled from tbrx_start_date
#       (see the schedules above).
# ------------------------------------------------------------------------------
build_missing_form_line_list <- function(baseline, month2_sbq, month5_sbq,
                                          lab_status, tb_refill_status, art_refill_status) {
  if (nrow(baseline) == 0) {
    return(data.frame(
      study_id = character(0), facility = character(0), form = character(0),
      reference_date = as.Date(character(0)), days_since = numeric(0),
      status = character(0), issue_of_tb_meds = character(0),
      expected_at = character(0), due_date = as.Date(character(0)),
      days_past_due = numeric(0), tbrx_start_date = as.Date(character(0))
    ))
  }

  m2_complete_ids <- month2_sbq$study_id[!is.na(month2_sbq$sbq_form_complete) & month2_sbq$sbq_form_complete == "Complete"]
  m5_complete_ids <- month5_sbq$study_id[!is.na(month5_sbq$sbq_form_complete) & month5_sbq$sbq_form_complete == "Complete"]

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

  # ---- TEACH Outcomes forms (scheduled from tbrx_start_date) ----
  outcomes_base_eligible <- baseline %>%
    filter(eligibility_status == "Yes", consent == "Yes",
           enrollment_form_complete == "Complete",
           !is.na(tbrx_start_date))

  # TB: completed slots matched exactly by issue_of_tb_meds
  tb_complete_pairs <- tb_refill_status %>%
    filter(!is.na(complete_flag), complete_flag == "Complete", !is.na(issue_of_tb_meds)) %>%
    distinct(study_id, period = issue_of_tb_meds)

  tb_missing <- build_scheduled_missing(outcomes_base_eligible, TB_SCHEDULE,
                                        tb_complete_pairs, "TB Treatment")
  if (!is.null(tb_missing)) tb_missing$issue_of_tb_meds <- tb_missing$expected_at

  lab_missing <- build_scheduled_missing(outcomes_base_eligible, LAB_SCHEDULE,
                                         count_to_completed_pairs(lab_status, LAB_SCHEDULE),
                                         "Lab")

  # ART Refill: additionally requires HIV positive + on ART
  art_base <- outcomes_base_eligible %>%
    filter(hiv_status == "Positive", started_art == "Yes")
  art_missing <- build_scheduled_missing(art_base, ART_SCHEDULE,
                                         count_to_completed_pairs(art_refill_status, ART_SCHEDULE),
                                         "ART Refill")

  tbrx_lookup <- baseline %>%
    select(study_id, tbrx_start_date) %>%
    distinct(study_id, .keep_all = TRUE)

  bind_rows(consent_missing, enrollment_missing, baseline_sbq_missing,
            month2_missing, month5_missing,
            lab_missing, tb_missing, art_missing) %>%
    left_join(tbrx_lookup, by = "study_id")
}

# ------------------------------------------------------------------------------
# CLOSE-OUT OUTCOMES
# ------------------------------------------------------------------------------
# Participants who should leave the Missing lists because they can no longer
# be followed up. One row per participant, first matching rule wins:
#
#  STEP 1  Died -- death recorded ANYWHERE:
#            participant_status == "Dead" at Baseline, Month 2 or Month 5 SBQ;
#            reason_refusal = "Passed on before consent" (or date_of_death_consent
#            filled); a dod at any SBQ visit; or a TB refill treatment_outcome
#            of Died.
#  STEP 3  Transferred out / Lost to follow-up from participant_status,
#            latest visit first: Month 5, then Month 2, then Baseline.
#  STEP 4  Transferred out -- transfer_out = "Yes" on ANY TB refill visit
#            (all refill periods, 1-2 weeks through 12 months).
#  STEP 5  Transferred out -- reason_refusal says "Transferred out".
#  STEP 6  Not consented -- eligible, consent blank, >30 days since screening.
# ------------------------------------------------------------------------------

CLOSEOUT_OUTCOMES <- c("Died", "Transferred out", "Lost to follow-up",
                       "Not consented (>30 days)")

# Returns a column (blank -> NA) or an all-NA vector if REDCap didn't export it
col_or_na <- function(df, col) {
  if (col %in% names(df)) blank_to_na(as.character(df[[col]])) else rep(NA_character_, nrow(df))
}

first_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) as.Date(NA) else min(x)
}

is_dead   <- function(x) !is.na(x) & str_detect(x, regex("^\\s*dead", ignore_case = TRUE))
is_ltfu   <- function(x) !is.na(x) & str_detect(x, regex("lost to follow", ignore_case = TRUE))
is_tout   <- function(x) !is.na(x) & str_detect(x, regex("transferred out", ignore_case = TRUE))

# participant_status + dates from one SBQ event, suffixed _bl / _m2 / _m5
build_event_participant_status <- function(raw_all, event_name, suffix) {
  ev <- raw_all %>%
    filter(redcap_event_name == event_name,
           is.na(redcap_repeat_instrument) | redcap_repeat_instrument == "")
  out <- tibble(
    study_id      = ev$study_id,
    status        = col_or_na(ev, "participant_status"),
    dod           = parse_redcap_date(col_or_na(ev, "dod")),
    date_transfer = parse_redcap_date(col_or_na(ev, "date_transfer")),
    date_last     = parse_redcap_date(col_or_na(ev, "date_last_visit"))
  ) %>%
    distinct(study_id, .keep_all = TRUE)
  names(out)[-1] <- paste0(names(out)[-1], "_", suffix)
  out
}

empty_closeout <- function() {
  tibble(study_id = character(0), facility = character(0), outcome = character(0),
         recorded_at = character(0), outcome_date = as.Date(character(0)),
         screening_date = as.Date(character(0)), tbrx_start_date = as.Date(character(0)))
}

build_closeout <- function(raw_all, baseline) {
  if (nrow(raw_all) == 0 || nrow(baseline) == 0) return(empty_closeout())

  bl <- tibble(
    study_id              = baseline$study_id,
    facility              = baseline$facility,
    screening_date        = baseline$screening_date,
    tbrx_start_date       = baseline$tbrx_start_date,
    eligibility_status    = baseline$eligibility_status,
    consent               = baseline$consent,
    days_since_screening  = baseline$calc_days_since_screening,
    reason_refusal        = col_or_na(baseline, "reason_refusal"),
    date_death_consent    = parse_redcap_date(col_or_na(baseline, "date_of_death_consent"))
  )

  st_bl <- build_event_participant_status(raw_all, "1. Baseline", "bl")
  st_m2 <- build_event_participant_status(raw_all, "2. Month_Two_followup", "m2")
  st_m5 <- build_event_participant_status(raw_all, "3. Month_Six_followup", "m5")

  # TB refill visits: transfer_out and treatment_outcome, any period
  tb <- raw_all %>% filter(redcap_repeat_instrument == "Form 6: TB Medication Refill Form")
  tb <- tibble(
    study_id          = tb$study_id,
    period            = col_or_na(tb, "issue_of_tb_meds"),
    transfer_out      = col_or_na(tb, "transfer_out"),
    date_transfer_out = parse_redcap_date(col_or_na(tb, "date_of_transfer_out")),
    treatment_outcome = col_or_na(tb, "treatment_outcome"),
    date_outcome      = parse_redcap_date(col_or_na(tb, "date_treatment_outcome"))
  )
  tb_transfer <- tb %>%
    filter(!is.na(transfer_out), transfer_out == "Yes") %>%
    group_by(study_id) %>%
    summarise(tb_transfer = TRUE,
              tb_transfer_date = first_date(date_transfer_out),
              tb_transfer_visit = paste(unique(na.omit(period)), collapse = ", "),
              .groups = "drop")
  tb_died <- tb %>%
    filter(!is.na(treatment_outcome),
           str_detect(treatment_outcome, regex("died|dead", ignore_case = TRUE))) %>%
    group_by(study_id) %>%
    summarise(tb_died = TRUE, tb_died_date = first_date(date_outcome), .groups = "drop")

  d <- bl %>%
    left_join(st_bl, by = "study_id") %>%
    left_join(st_m2, by = "study_id") %>%
    left_join(st_m5, by = "study_id") %>%
    left_join(tb_transfer, by = "study_id") %>%
    left_join(tb_died, by = "study_id") %>%
    mutate(
      tb_transfer = coalesce(tb_transfer, FALSE),
      tb_died     = coalesce(tb_died, FALSE),

      # Step 1 pieces
      died_m5      = is_dead(status_m5) | !is.na(dod_m5),
      died_m2      = is_dead(status_m2) | !is.na(dod_m2),
      died_bl      = is_dead(status_bl) | !is.na(dod_bl),
      died_consent = (!is.na(reason_refusal) &
                        str_detect(reason_refusal, regex("passed on|died|dead", ignore_case = TRUE))) |
                     !is.na(date_death_consent),

      # Step 6
      consent_overdue = !is.na(eligibility_status) & eligibility_status == "Yes" &
                        is.na(consent) & !is.na(days_since_screening) & days_since_screening > 30,

      outcome = case_when(
        died_m5 | died_m2 | died_bl | died_consent | tb_died ~ "Died",
        is_tout(status_m5) ~ "Transferred out",
        is_ltfu(status_m5) ~ "Lost to follow-up",
        is_tout(status_m2) ~ "Transferred out",
        is_ltfu(status_m2) ~ "Lost to follow-up",
        is_tout(status_bl) ~ "Transferred out",
        is_ltfu(status_bl) ~ "Lost to follow-up",
        tb_transfer        ~ "Transferred out",
        is_tout(reason_refusal) ~ "Transferred out",
        consent_overdue    ~ "Not consented (>30 days)",
        TRUE ~ NA_character_
      ),
      recorded_at = case_when(
        died_m5      ~ "Month 5 SBQ",
        died_m2      ~ "Month 2 SBQ",
        died_bl      ~ "Baseline SBQ",
        died_consent ~ "Consent form",
        tb_died      ~ "TB refill (treatment outcome)",
        is_tout(status_m5) | is_ltfu(status_m5) ~ "Month 5 SBQ",
        is_tout(status_m2) | is_ltfu(status_m2) ~ "Month 2 SBQ",
        is_tout(status_bl) | is_ltfu(status_bl) ~ "Baseline SBQ",
        tb_transfer        ~ paste0("TB refill (", tb_transfer_visit, ")"),
        is_tout(reason_refusal) ~ "Consent form",
        consent_overdue    ~ "Screening (no consent in 30 days)",
        TRUE ~ NA_character_
      ),
      outcome_date = case_when(
        died_m5      ~ dod_m5,
        died_m2      ~ dod_m2,
        died_bl      ~ dod_bl,
        died_consent ~ date_death_consent,
        tb_died      ~ tb_died_date,
        is_tout(status_m5) ~ date_transfer_m5,
        is_ltfu(status_m5) ~ date_last_m5,
        is_tout(status_m2) ~ date_transfer_m2,
        is_ltfu(status_m2) ~ date_last_m2,
        is_tout(status_bl) ~ date_transfer_bl,
        is_ltfu(status_bl) ~ date_last_bl,
        tb_transfer        ~ tb_transfer_date,
        consent_overdue    ~ screening_date + days(30),
        TRUE ~ as.Date(NA)
      )
    ) %>%
    filter(!is.na(outcome))

  if (nrow(d) == 0) return(empty_closeout())
  d %>% select(study_id, facility, outcome, recorded_at, outcome_date,
               screening_date, tbrx_start_date)
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
      closeout = empty_closeout(),
      n_total = 0
    ))
  }

  baseline_tbl <- build_baseline_table(raw)
  month2_tbl <- build_followup_sbq(raw, "2. Month_Two_followup")
  month5_tbl <- build_followup_sbq(raw, "3. Month_Six_followup")
  lab_tbl <- build_repeat_form_status(raw, "Form 5: Laboratory Tests Form", "laboratory_tests_form_complete")
  tb_refill_tbl <- build_tb_refill_status(raw)
  art_refill_tbl <- build_repeat_form_status(raw, "Form 7: ART Medication Refill Form", "art_form_complete")

  closeout_tbl <- build_closeout(raw, baseline_tbl)

  # Tag each missing-form row with the participant's close-out outcome (if any)
  missing_tbl <- build_missing_form_line_list(baseline_tbl, month2_tbl, month5_tbl,
                                              lab_tbl, tb_refill_tbl, art_refill_tbl) %>%
    left_join(closeout_tbl %>% select(study_id, closeout_outcome = outcome), by = "study_id")

  list(
    baseline      = baseline_tbl,
    month2_sbq    = month2_tbl,
    month5_sbq    = month5_tbl,
    missing_forms = missing_tbl,
    closeout      = closeout_tbl,
    n_total       = n_distinct(raw$study_id)
  )
}
