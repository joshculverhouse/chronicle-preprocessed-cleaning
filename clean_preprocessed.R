# ============================================================================
# Script: clean_preprocessed.R
# Purpose:
#   Cleaning pipeline for preprocessed Chronicle-compatible Android event-log
#   data. The input may come from the companion preprocessing repository or
#   from another workflow that produces the required columns listed below.
#
# Expected required input columns:
#   participant_id
#   interaction_type
#   application_label
#   app_package_name
#   event_timestamp
#   start_timestamp
#   stop_timestamp
#   timezone
#
# Optional columns are preserved upstream where possible, but this script only
# requires the columns above.
#
# Main features:
#   - Reads all CSVs from input_folder
#   - Parses timestamps using each file's primary timezone
#   - Collapses adjacent split app-usage segments
#   - Flags and logs long events
#   - Truncates configurable background/problematic apps
#   - Optionally drops or truncates events over the long-event threshold
#   - Flags first/last days, DST days, and parent-device long-gap boundary days
#   - Writes one cleaned CSV per participant plus audit logs
# ============================================================================


library(pacman)
p_load(tidyverse, lubridate, readr, stringr, purrr)

# ------------------------------ CONFIG ---------------------------------------

config <- list(
  
  # Core paths
  # Set these two paths before running.
  input_folder  = "path/to/preprocessed_csvs",
  output_folder = "path/to/cleaned_output",
  
  # Config file listing package names to truncate when they produce
  # implausibly long duration-bearing events. Users can edit this CSV
  # rather than editing the R script.
  bad_apps_file = "bad_apps.csv",
  
  # Device/person type:
  #   "parent" -> apply >12h gap boundary partial-day logic
  #   "child"  -> skip >12h gap boundary partial-day logic
  device_type = "parent",
  
  # Which interaction types should be treated as duration-bearing events
  # for long-event flagging/action?
  # Look in a preprocessed file to identify these labels
  duration_interaction_types = c("App Usage", "Session", "Glance"),
  
  # Which interaction types should be collapsed when adjacent segments are
  # separated by <= session_gap_secs?
  # Usually App Usage only, but can include others if needed later.
  collapse_interaction_types = c("App Usage"),
  
  # Bad apps
  # Loaded from bad_apps_file.
  # Keep this as NULL unless you intentionally want to override the CSV below.
  bad_apps = NULL,
  
  # Thresholds
  max_bad_app_secs = 10 * 60,      # 10 min
  long_3h_secs     = 3  * 60 * 60, # 3 h
  long_6h_secs     = 6  * 60 * 60, # 6 h
  long_gap_hours   = 12,           # 12 h
  session_gap_secs = 1,            # collapse threshold
  
  # Long-event action for rows exceeding long_6h_secs:
  #   "none"
  #   "truncate_to_bad_app"
  #   "truncate_to_6h"
  #   "drop"
  long_event_action = "truncate_to_bad_app",
  
  # Where to apply long_event_action:
  #   "all"            -> App Usage, Session, Glance, etc. if duration-bearing
  #   "app_usage_only" -> only interaction_type == "App Usage"
  apply_long_event_action_to = "all"
)

log_folder           <- file.path(config$output_folder, "logs")
additiona_log_folder <- file.path(log_folder, "additional_logs")

dir.create(config$output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(log_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(additiona_log_folder, recursive = TRUE, showWarnings = FALSE)

# Load bad-app package list from CSV unless config$bad_apps is manually set above.
# Expected columns: app_package_name, reason
if (is.null(config$bad_apps)) {
  if (!file.exists(config$bad_apps_file)) {
    stop("Could not find bad_apps_file: ", config$bad_apps_file, call. = FALSE)
  }
  
  bad_apps_tbl <- readr::read_csv(
    config$bad_apps_file,
    col_types = cols(
      app_package_name = col_character(),
      reason = col_character(),
      .default = col_character()
    )
  )
  
  if (!"app_package_name" %in% names(bad_apps_tbl)) {
    stop("bad_apps_file must contain a column named app_package_name.", call. = FALSE)
  }
  
  config$bad_apps <- bad_apps_tbl %>%
    filter(!is.na(app_package_name), app_package_name != "") %>%
    pull(app_package_name) %>%
    unique()
}

cat(sprintf("Loaded %d bad-app package name(s).\n", length(config$bad_apps)))

# Basic configuration checks
if (!tolower(config$device_type) %in% c("parent", "child")) {
  stop('config$device_type must be either "parent" or "child".', call. = FALSE)
}
if (!config$long_event_action %in% c("none", "truncate_to_bad_app", "truncate_to_6h", "drop")) {
  stop('config$long_event_action must be one of: "none", "truncate_to_bad_app", "truncate_to_6h", "drop".', call. = FALSE)
}
if (!config$apply_long_event_action_to %in% c("all", "app_usage_only")) {
  stop('config$apply_long_event_action_to must be either "all" or "app_usage_only".', call. = FALSE)
}

# ------------------------------ HELPERS --------------------------------------

bind_or_null <- function(lst) {
  if (length(lst)) bind_rows(lst) else tibble()
}

require_cols <- function(df, cols_needed) {
  missing <- setdiff(cols_needed, names(df))
  if (length(missing)) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

add_flag <- function(existing, newflag) {
  existing <- ifelse(is.na(existing) | existing == "", NA_character_, existing)
  out <- ifelse(is.na(existing), newflag, paste(existing, newflag, sep = ";"))
  map_chr(str_split(out, ";", simplify = FALSE), \(v) {
    if (length(v) == 0 || all(is.na(v))) return(NA_character_)
    v <- v[!is.na(v) & v != ""]
    if (!length(v)) return(NA_character_)
    paste(unique(v), collapse = ";")
  })
}

local_date <- function(x, tz) as.Date(x, tz = tz)

fmt_posix_cols <- function(df, fmt = "%Y-%m-%d %H:%M:%S") {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  df %>% mutate(across(where(~ inherits(.x, "POSIXt")), ~ format(.x, fmt)))
}

parse_event_timestamp <- function(x, tz) {
  # event_timestamp example:
  #   2024-01-19 10:38:44.745000-05:00
  suppressWarnings({
    out <- ymd_hms(x, tz = "UTC", quiet = TRUE)
  })
  
  # For offset-aware strings, ymd_hms parses the instant.
  # Convert to the file's primary timezone for local comparisons/order.
  suppressWarnings(with_tz(out, tz))
}

parse_start_stop_timestamp <- function(x, tz) {
  # start/stop examples:
  #   1/19/2024 10:38
  #   1/19/2024 10:38:44
  #   1/19/2024 10:38:44.745
  suppressWarnings({
    out <- mdy_hms(x, tz = tz, quiet = TRUE)
  })
  
  miss <- is.na(out)
  if (any(miss)) {
    out2 <- suppressWarnings(mdy_hm(x[miss], tz = tz, quiet = TRUE))
    out[miss] <- out2
  }
  
  out
}

apply_long_action <- function(start_posix, stop_posix, action, max_bad_app_secs, long_6h_secs) {
  if (action == "truncate_to_bad_app") {
    pmin(stop_posix, start_posix + seconds(max_bad_app_secs))
  } else if (action == "truncate_to_6h") {
    pmin(stop_posix, start_posix + seconds(long_6h_secs))
  } else {
    stop_posix
  }
}

# --------------------------- LOG ACCUMULATORS --------------------------------

acc_bad_trunc_rows   <- list()
acc_bad_trunc_counts <- list()
acc_long_events      <- list()
acc_gaps             <- list()
acc_partial_days     <- list()
acc_partial_dates    <- list()

# ---------------------------- FILE DISCOVERY ---------------------------------

files_list <- list.files(
  path = config$input_folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive =  TRUE
)

total_files <- length(files_list)

cat(sprintf("Found %d CSV file(s) in:\n  %s\n", length(files_list), config$input_folder))
if (!length(files_list)) stop("No CSV files found in input_folder.", call. = FALSE)

# ------------------------------- MAIN LOOP -----------------------------------

for (i in seq_along(files_list)) {
  
  input_file <- files_list[i]
  
  base_path <- basename(input_file)
  cat("[", i, "/", total_files, "] ", base_path, "\n", sep = "")
  
  df_raw <- readr::read_csv(
    input_file,
    col_types = cols(
      .default          = col_guess(),
      participant_id    = col_character(),
      interaction_type  = col_character(),
      application_label = col_character(),
      app_package_name  = col_character(),
      event_timestamp   = col_character(),
      start_timestamp   = col_character(),
      stop_timestamp    = col_character(),
      timezone          = col_character()
    )
  )
  
  cat(sprintf("Rows read: %d\n", nrow(df_raw)))
  if (!nrow(df_raw)) next
  
  require_cols(df_raw, c(
    "participant_id", "interaction_type", "application_label",
    "app_package_name", "event_timestamp", "start_timestamp",
    "stop_timestamp", "timezone"
  ))
  
  # Choose primary timezone by most frequent actual timezone string
  tz_counts <- df_raw %>%
    filter(!is.na(timezone), timezone != "") %>%
    count(timezone, sort = TRUE, name = "n")
  
  if (!nrow(tz_counts)) {
    warning("No valid timezone values found; skipping file: ", base_path)
    next
  }
  
  tz <- tz_counts$timezone[1]
  
  if (nrow(tz_counts) > 1) {
    cat("Primary timezone: ", tz, " (multiple found; filtering to most frequent)\n", sep = "")
  } else {
    cat("Primary timezone: ", tz, "\n", sep = "")
  }
  
  df <- df_raw %>%
    distinct() %>%
    filter(timezone == tz) %>%
    mutate(
      event_posix = parse_event_timestamp(event_timestamp, tz),
      start_posix_raw = parse_start_stop_timestamp(start_timestamp, tz),
      stop_posix_raw  = parse_start_stop_timestamp(stop_timestamp, tz),
      
      # Keep non-duration events:
      # if start missing, use event instant
      # if stop missing, leave as NA for now (we'll create internal stop for
      # gap logic later, but keep these as non-duration rows conceptually)
      start_posix = coalesce(start_posix_raw, event_posix),
      stop_posix  = stop_posix_raw,
      
      date_local = local_date(coalesce(start_posix, event_posix), tz)
    ) %>%
    arrange(participant_id, coalesce(start_posix, event_posix))
  
  if (!nrow(df)) next
  
  # --------------------------------------------------------------------------
  # 1) Split into collapse-target rows and passthrough rows
  # --------------------------------------------------------------------------
  
  df_to_collapse <- df %>%
    filter(interaction_type %in% config$collapse_interaction_types) %>%
    mutate(
      # Internal stop for collapse logic: if no stop, use start
      stop_for_logic = coalesce(stop_posix, start_posix)
    ) %>%
    arrange(participant_id, interaction_type, app_package_name, start_posix) %>%
    group_by(participant_id, interaction_type, app_package_name) %>%
    mutate(
      gap_prev = as.numeric(difftime(start_posix, lag(stop_for_logic), units = "secs")),
      new_block = is.na(gap_prev) | gap_prev > config$session_gap_secs,
      block_id = cumsum(replace_na(new_block, TRUE))
    ) %>%
    group_by(participant_id, interaction_type, app_package_name, block_id) %>%
    summarise(
      application_label = first(application_label),
      event_timestamp   = first(event_timestamp),
      start_timestamp   = first(start_timestamp),
      stop_timestamp    = last(stop_timestamp),
      timezone          = first(timezone),
      event_posix       = first(event_posix),
      start_posix       = first(start_posix),
      stop_posix        = last(stop_for_logic),
      date_local        = first(date_local),
      n_segments        = n(),
      .groups = "drop"
    )
  
  df_passthrough <- df %>%
    filter(!interaction_type %in% config$collapse_interaction_types) %>%
    mutate(
      stop_posix = stop_posix,
      n_segments = 1L
    ) %>%
    select(
      participant_id, interaction_type, app_package_name, application_label,
      event_timestamp, start_timestamp, stop_timestamp, timezone,
      event_posix, start_posix, stop_posix, date_local, n_segments
    )
  
  df2 <- bind_rows(df_to_collapse, df_passthrough) %>%
    arrange(participant_id, coalesce(start_posix, event_posix))
  
  # --------------------------------------------------------------------------
  # 2) Duration calculation and event flags
  # --------------------------------------------------------------------------
  
  df3 <- df2 %>%
    mutate(
      is_duration_type = interaction_type %in% config$duration_interaction_types,
      is_truncatable_type = interaction_type %in% config$duration_interaction_types &
        interaction_type != "nonuse", # don't truncate nonuse events
      
      stop_for_duration = if_else(is_duration_type & is.na(stop_posix), start_posix, stop_posix),
      duration_secs = as.numeric(difftime(stop_for_duration, start_posix, units = "secs")),
      duration_secs = if_else(is_duration_type, duration_secs, as.numeric(NA)),
      event_flags = NA_character_
    ) %>%
    mutate(
      event_flags = case_when(
        is_truncatable_type & !is.na(duration_secs) & duration_secs >= config$long_6h_secs ~ add_flag(event_flags, "long_6h"),
        is_truncatable_type & !is.na(duration_secs) & duration_secs >= config$long_3h_secs ~ add_flag(event_flags, "long_3h"),
        TRUE ~ event_flags
      )
    )
  
  # --------------------------------------------------------------------------
  # 3) Bad app truncation (> max_bad_app_secs)
  # --------------------------------------------------------------------------
  
  df4 <- df3 %>%
    mutate(
      is_bad_app = is_truncatable_type & app_package_name %in% config$bad_apps,
      is_bad_trunc = is_bad_app & !is.na(duration_secs) & duration_secs > config$max_bad_app_secs,
      
      original_stop_for_log = stop_for_duration,
      original_duration_for_log = duration_secs,
      
      stop_after_bad = if_else(
        is_bad_trunc,
        start_posix + seconds(config$max_bad_app_secs),
        stop_for_duration
      ),
      
      duration_after_bad = as.numeric(difftime(stop_after_bad, start_posix, units = "secs")),
      
      truncated_secs = if_else(
        is_bad_trunc,
        pmax(original_duration_for_log - duration_after_bad, 0),
        as.numeric(NA)
      ),
      
      event_flags = if_else(is_bad_trunc, add_flag(event_flags, "bad_app_truncated"), event_flags)
    )
  
  bad_trunc_rows <- df4 %>%
    filter(is_bad_trunc) %>%
    transmute(
      participant_id,
      interaction_type,
      app_package_name,
      application_label,
      start = start_posix,
      stop_original = original_stop_for_log,
      duration_secs_original = round(original_duration_for_log, 1),
      stop_truncated = stop_after_bad,
      duration_secs_truncated = round(duration_after_bad, 1),
      seconds_trimmed = round(truncated_secs, 1),
      source_file = base_path
    )
  
  if (nrow(bad_trunc_rows) > 0) {
    acc_bad_trunc_rows[[length(acc_bad_trunc_rows) + 1]] <- bad_trunc_rows
    
    bad_trunc_counts <- bad_trunc_rows %>%
      group_by(participant_id, interaction_type, app_package_name, application_label) %>%
      summarise(
        n_truncated_events = n(),
        total_seconds_trimmed = sum(seconds_trimmed, na.rm = TRUE),
        .groups = "drop"
      )
    
    acc_bad_trunc_counts[[length(acc_bad_trunc_counts) + 1]] <- bad_trunc_counts
  }
  
  # --------------------------------------------------------------------------
  # 4) Long-event action (> long_6h_secs), after bad-app handling
  # --------------------------------------------------------------------------
  
  df5 <- df4 %>%
    mutate(
      stop_for_action_input = stop_after_bad,
      duration_for_action_input = as.numeric(difftime(stop_for_action_input, start_posix, units = "secs")),
      
      eligible_for_long_action =
        is_truncatable_type &
        !is.na(duration_for_action_input) &
        duration_for_action_input > config$long_6h_secs &
        !is_bad_trunc &  # bad app rule already handled above
        case_when(
          config$apply_long_event_action_to == "all" ~ TRUE,
          config$apply_long_event_action_to == "app_usage_only" ~ interaction_type == "App Usage",
          TRUE ~ FALSE
        )
    )
  
  if (config$long_event_action == "drop") {
    df6 <- df5 %>%
      filter(!eligible_for_long_action) %>%
      mutate(
        stop_final = stop_for_action_input,
        duration_secs = as.numeric(difftime(stop_final, start_posix, units = "secs"))
      )
  } else if (config$long_event_action %in% c("truncate_to_bad_app", "truncate_to_6h")) {
    
    df6 <- df5 %>%
      mutate(
        stop_final = if_else(
          eligible_for_long_action,
          apply_long_action(
            start_posix = start_posix,
            stop_posix = stop_for_action_input,
            action = config$long_event_action,
            max_bad_app_secs = config$max_bad_app_secs,
            long_6h_secs = config$long_6h_secs
          ),
          stop_for_action_input
        ),
        duration_secs = as.numeric(difftime(stop_final, start_posix, units = "secs")),
        
        extra_trunc_secs = if_else(
          eligible_for_long_action,
          pmax(duration_for_action_input - duration_secs, 0),
          as.numeric(NA)
        ),
        
        truncated_secs = case_when(
          !is.na(truncated_secs) & !is.na(extra_trunc_secs) ~ truncated_secs + extra_trunc_secs,
          is.na(truncated_secs) & !is.na(extra_trunc_secs) ~ extra_trunc_secs,
          TRUE ~ truncated_secs
        ),
        
        event_flags = if_else(
          eligible_for_long_action,
          add_flag(event_flags, paste0("long_event_", config$long_event_action)),
          event_flags
        )
      )
    
  } else {
    df6 <- df5 %>%
      mutate(
        stop_final = stop_for_action_input,
        duration_secs = as.numeric(difftime(stop_final, start_posix, units = "secs"))
      )
  }
  
  # For non-duration rows, keep duration as NA
  df6 <- df6 %>%
    mutate(
      stop_final = if_else(is_duration_type, stop_final, stop_posix),
      duration_secs = if_else(is_duration_type, duration_secs, as.numeric(NA))
    )
  
  
  # Log all long events (including ones later truncated or dropped)
  long_rows <- df5 %>%
    filter(
      is_truncatable_type,
      !is.na(duration_for_action_input),
      duration_for_action_input >= config$long_3h_secs
    ) %>%
    transmute(
      participant_id,
      interaction_type,
      app_package_name,
      application_label,
      start = start_posix,
      stop_original = stop_for_action_input,
      duration_secs_original = round(duration_for_action_input, 1),
      event_flags,
      eligible_for_long_action,
      long_action_applied = case_when(
        duration_for_action_input > config$long_6h_secs & eligible_for_long_action ~ config$long_event_action,
        TRUE ~ "none"
      ),
      was_dropped = case_when(
        long_action_applied == "drop" ~ "yes",
        TRUE ~ "no"
      ),
      source_file = basename(input_file)
    ) %>%
    left_join(
      df6 %>%
        transmute(
          participant_id,
          interaction_type,
          app_package_name,
          start = start_posix,
          stop_final_logged = stop_final,
          duration_secs_final = round(duration_secs, 1),
          truncated_secs_logged = round(truncated_secs, 1)
        ),
      by = c("participant_id", "interaction_type", "app_package_name", "start")
    ) %>%
    mutate(
      was_dropped = case_when(
        is.na(stop_final_logged) & long_action_applied == "drop" ~ "yes",
        TRUE ~ "no"
      )
    ) %>%
    rename(
      stop_final = stop_final_logged,
      duration_secs_final = duration_secs_final,
      truncated_secs = truncated_secs_logged
    )
  
  if (nrow(long_rows) > 0) {
    acc_long_events[[length(acc_long_events) + 1]] <- long_rows
  }
  
  # --------------------------------------------------------------------------
  # 5) Gap detection (parents only)
  # --------------------------------------------------------------------------
  
  # For internal gap logic, non-duration rows use stop = start
  df_gap_base <- df6 %>%
    mutate(
      start_for_gap = coalesce(start_posix, event_posix),
      stop_for_gap  = coalesce(stop_final, start_for_gap)
    )
  
  if (tolower(config$device_type) == "parent") {
    
    gap_info <- df_gap_base %>%
      group_by(participant_id) %>%
      arrange(start_for_gap, .by_group = TRUE) %>%
      mutate(
        next_start = lead(start_for_gap),
        prev_stop  = stop_for_gap,
        gap_hours  = as.numeric(difftime(next_start, prev_stop, units = "hours"))
      ) %>%
      ungroup() %>%
      filter(!is.na(gap_hours), gap_hours > config$long_gap_hours)
    
    gap_days <- gap_info %>%
      mutate(
        end_day  = local_date(prev_stop, tz),
        next_day = local_date(next_start, tz)
      ) %>%
      select(participant_id, end_day, next_day) %>%
      pivot_longer(c(end_day, next_day), names_to = "which", values_to = "date_local") %>%
      distinct() %>%
      mutate(is_gap_boundary = TRUE) %>%
      select(participant_id, date_local, is_gap_boundary)
    
    if (nrow(gap_info) > 0) {
      gaps_log <- gap_info %>%
        mutate(
          first_partial_date = local_date(prev_stop, tz),
          last_partial_date  = local_date(next_start, tz),
          days_removed_for_this_gap = as.integer(last_partial_date - first_partial_date) + 1L
        ) %>%
        transmute(
          participant_id,
          stop_prev_local  = prev_stop,
          start_next_local = next_start,
          gap_hours        = round(gap_hours, 2),
          first_partial_date,
          last_partial_date,
          days_removed_for_this_gap,
          source_file = base_path
        )
      
      acc_gaps[[length(acc_gaps) + 1]] <- gaps_log
    }
    
    df7 <- df6 %>%
      left_join(gap_days, by = c("participant_id", "date_local")) %>%
      mutate(is_gap_boundary = replace_na(is_gap_boundary, FALSE))
    
  } else {
    df7 <- df6 %>%
      mutate(is_gap_boundary = FALSE)
  }
  
  # --------------------------------------------------------------------------
  # 6) Partial day flags: first/last + DST + gaps (parent only)
  # --------------------------------------------------------------------------
  
  df_days <- df7 %>%
    distinct(participant_id, date_local)
  
  dst_by_day <- df_days %>%
    mutate(
      day_start = as.POSIXct(date_local, tz = tz),
      day_end   = day_start + days(1) - seconds(1),
      is_dst_transition = dst(day_start) != dst(day_end)
    ) %>%
    select(participant_id, date_local, is_dst_transition)
  
  df8 <- df7 %>%
    group_by(participant_id) %>%
    mutate(
      first_day = min(date_local, na.rm = TRUE),
      last_day  = max(date_local, na.rm = TRUE),
      is_first_day = date_local == first_day,
      is_last_day  = date_local == last_day
    ) %>%
    ungroup() %>%
    left_join(dst_by_day, by = c("participant_id", "date_local")) %>%
    mutate(
      is_dst_transition = replace_na(is_dst_transition, FALSE),
      day_flags = NA_character_,
      day_flags = if_else(
        is_gap_boundary | is_first_day | is_last_day,
        add_flag(day_flags, "partial_day"),
        day_flags
      ),
      day_flags = if_else(
        is_dst_transition,
        add_flag(day_flags, "DST_day"),
        day_flags
      )
    ) %>%
    select(-first_day, -last_day, -is_first_day, -is_last_day, -is_dst_transition)
  
  flagged_days_this <- df8 %>%
    filter(!is.na(day_flags) & day_flags != "") %>%
    distinct(participant_id, date_local)
  
  if (nrow(flagged_days_this) > 0) {
    partial_counts <- flagged_days_this %>%
      group_by(participant_id) %>%
      summarise(
        n_unique_partial_days_flagged = n_distinct(date_local),
        .groups = "drop"
      ) %>%
      mutate(source_file = base_path)
    
    acc_partial_days[[length(acc_partial_days) + 1]] <- partial_counts
    acc_partial_dates[[length(acc_partial_dates) + 1]] <- flagged_days_this %>%
      mutate(source_file = base_path)
  }
  
  # --------------------------------------------------------------------------
  # 7) Write cleaned output
  # --------------------------------------------------------------------------
  
  to_time <- function(x) ifelse(is.na(x), NA, format(x, "%Y-%m-%d %H:%M:%S"))
  
  out <- df8 %>%
    mutate(
      start = to_time(start_posix),
      stop  = to_time(stop_final)
    ) %>%
    transmute(
      participant_id,
      interaction_type,
      app_package_name,
      application_label,
      timezone,
      start,
      stop,
      duration_secs = round(duration_secs, 1),
      day_flags,
      event_flags,
      truncated_secs = round(truncated_secs, 1)
    ) %>%
    distinct() %>%
    arrange(participant_id, start)
  
  pid <- out$participant_id[1]
  outfile <- file.path(config$output_folder, paste0(pid, "_cleaned.csv"))
  readr::write_csv(out, outfile)
  
}

# ------------------------------- GLOBAL LOGS ---------------------------------

cat("\nWriting global summary logs...\n")

bad_trunc_rows_out <- bind_or_null(acc_bad_trunc_rows) %>% distinct()

bad_trunc_counts_out <- bind_or_null(acc_bad_trunc_counts) %>%
  {
    if (nrow(.) > 0) {
      group_by(., participant_id, interaction_type, app_package_name, application_label) %>%
        summarise(
          n_truncated_events = sum(n_truncated_events),
          total_seconds_trimmed = sum(total_seconds_trimmed, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      tibble(
        participant_id = character(),
        interaction_type = character(),
        app_package_name = character(),
        application_label = character(),
        n_truncated_events = integer(),
        total_seconds_trimmed = double()
      )
    }
  }

long_events_out <- bind_or_null(acc_long_events) %>% distinct()
gaps_out        <- bind_or_null(acc_gaps) %>% distinct()

partial_days_out <- bind_or_null(acc_partial_days) %>%
  {
    if (nrow(.) > 0) {
      group_by(., participant_id) %>%
        summarise(
          n_unique_partial_days_flagged = max(n_unique_partial_days_flagged),
          .groups = "drop"
        )
    } else {
      tibble(
        participant_id = character(),
        n_unique_partial_days_flagged = integer()
      )
    }
  }

partial_dates_out <- bind_or_null(acc_partial_dates) %>%
  select(participant_id, date_local, source_file) %>%
  distinct() %>%
  arrange(participant_id, date_local)

if (nrow(long_events_out)      > 0) readr::write_csv(fmt_posix_cols(long_events_out),      file.path(log_folder, "log_long_events_3h_plus.csv"))

if (nrow(bad_trunc_rows_out)   > 0) readr::write_csv(fmt_posix_cols(bad_trunc_rows_out),   file.path(additiona_log_folder, "log_bad_apps_truncated_rows.csv"))
if (nrow(bad_trunc_counts_out) > 0) readr::write_csv(bad_trunc_counts_out,                  file.path(additiona_log_folder, "log_bad_apps_truncated_counts.csv"))
if (nrow(gaps_out)             > 0) readr::write_csv(fmt_posix_cols(gaps_out),             file.path(additiona_log_folder, "log_gaps_over_12h.csv"))
if (nrow(partial_days_out)     > 0) readr::write_csv(partial_days_out,                      file.path(additiona_log_folder, "log_partial_days_flagged_per_participant.csv"))
if (nrow(partial_dates_out)    > 0) readr::write_csv(partial_dates_out,                     file.path(additiona_log_folder, "log_partial_days_flagged_dates.csv"))

cat("Global logs written. Cleaning complete.\n")
