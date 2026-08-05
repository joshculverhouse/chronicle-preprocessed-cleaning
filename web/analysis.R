# Browser-side Chronicle preprocessed-cleaning core.
# This file is loaded by webR. JavaScript manages local folder access, writes
# one source file to the webR filesystem at a time, and writes output locally.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

browser_cleaning_result <- NULL

default_cleaning_config <- function() {
  list(
    device_type = "parent",
    duration_interaction_types = c("App Usage", "Session", "Glance"),
    collapse_interaction_types = c("App Usage"),
    max_bad_app_secs = 10 * 60,
    long_3h_secs = 3 * 60 * 60,
    long_6h_secs = 6 * 60 * 60,
    long_gap_hours = 12,
    session_gap_secs = 1,
    long_event_action = "truncate_to_bad_app",
    apply_long_event_action_to = "all"
  )
}

normalise_browser_config <- function(config_path) {
  supplied <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
  config <- utils::modifyList(default_cleaning_config(), supplied)
  config$device_type <- tolower(as.character(config$device_type[[1]]))
  config$duration_interaction_types <- as.character(unlist(config$duration_interaction_types, use.names = FALSE))
  config$collapse_interaction_types <- as.character(unlist(config$collapse_interaction_types, use.names = FALSE))
  config$max_bad_app_secs <- as.numeric(config$max_bad_app_secs[[1]])
  config$long_3h_secs <- as.numeric(config$long_3h_secs[[1]])
  config$long_6h_secs <- as.numeric(config$long_6h_secs[[1]])
  config$long_gap_hours <- as.numeric(config$long_gap_hours[[1]])
  config$session_gap_secs <- as.numeric(config$session_gap_secs[[1]])
  config$long_event_action <- as.character(config$long_event_action[[1]])
  config$apply_long_event_action_to <- as.character(config$apply_long_event_action_to[[1]])

  if (!config$device_type %in% c("parent", "child")) {
    stop("Device type must be either 'parent' or 'child'.", call. = FALSE)
  }
  if (!length(config$duration_interaction_types) || !length(config$collapse_interaction_types)) {
    stop("At least one duration-bearing and one collapse interaction type must be supplied.", call. = FALSE)
  }
  if (any(!is.finite(c(
    config$max_bad_app_secs, config$long_3h_secs, config$long_6h_secs,
    config$long_gap_hours, config$session_gap_secs
  ))) || any(c(
    config$max_bad_app_secs, config$long_3h_secs, config$long_6h_secs,
    config$long_gap_hours
  ) <= 0) || config$session_gap_secs < 0) {
    stop("All thresholds must be positive, except the session gap which may be zero.", call. = FALSE)
  }
  if (config$long_6h_secs < config$long_3h_secs) {
    stop("The long-event action threshold cannot be lower than the review threshold.", call. = FALSE)
  }
  if (!config$long_event_action %in% c("none", "truncate_to_bad_app", "truncate_to_6h", "drop")) {
    stop("Invalid long-event action.", call. = FALSE)
  }
  if (!config$apply_long_event_action_to %in% c("all", "app_usage_only")) {
    stop("Invalid long-event action scope.", call. = FALSE)
  }
  config
}

read_bad_apps <- function(path) {
  if (!file.exists(path)) stop("Could not read the selected bad-app CSV.", call. = FALSE)
  tbl <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) stop("Could not read the selected bad-app CSV: ", conditionMessage(e), call. = FALSE)
  )
  if (!"app_package_name" %in% names(tbl)) {
    stop("The bad-app CSV must contain a column named 'app_package_name'.", call. = FALSE)
  }
  tbl %>%
    transmute(app_package_name = as.character(app_package_name)) %>%
    filter(!is.na(app_package_name), nzchar(app_package_name)) %>%
    pull(app_package_name) %>%
    unique()
}

require_columns <- function(df, required) {
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

add_flag <- function(existing, new_flag) {
  existing <- ifelse(is.na(existing) | existing == "", NA_character_, existing)
  out <- ifelse(is.na(existing), new_flag, paste(existing, new_flag, sep = ";"))
  purrr::map_chr(str_split(out, ";", simplify = FALSE), function(values) {
    values <- values[!is.na(values) & values != ""]
    if (!length(values)) return(NA_character_)
    paste(unique(values), collapse = ";")
  })
}

local_date <- function(x, tz) as.Date(x, tz = tz)

format_posix_columns <- function(df, format_string = "%Y-%m-%d %H:%M:%S") {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  df %>% mutate(across(where(function(x) inherits(x, "POSIXt")), function(x) format(x, format_string)))
}

parse_event_timestamp <- function(x, tz) {
  out <- suppressWarnings(ymd_hms(x, tz = "UTC", quiet = TRUE))
  suppressWarnings(with_tz(out, tz))
}

parse_start_stop_timestamp <- function(x, tz) {
  out <- suppressWarnings(mdy_hms(x, tz = tz, quiet = TRUE))
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- suppressWarnings(mdy_hm(x[missing], tz = tz, quiet = TRUE))
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

write_log_if_rows <- function(data, path, format_posix = FALSE) {
  if (!nrow(data)) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (format_posix) data <- format_posix_columns(data)
  readr::write_csv(data, path)
  invisible(NULL)
}

clean_browser_file <- function(input_path, output_folder, bad_apps_path, config_path) {
  config <- normalise_browser_config(config_path)
  bad_apps <- read_bad_apps(bad_apps_path)
  source_file <- basename(input_path)

  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE, force = TRUE)
  dir.create(file.path(output_folder, "logs", "additional_logs"), recursive = TRUE, showWarnings = FALSE)

  df_raw <- tryCatch(
    readr::read_csv(
      input_path,
      col_types = cols(
        .default = col_guess(),
        participant_id = col_character(),
        interaction_type = col_character(),
        application_label = col_character(),
        app_package_name = col_character(),
        event_timestamp = col_character(),
        start_timestamp = col_character(),
        stop_timestamp = col_character(),
        timezone = col_character()
      ),
      progress = FALSE
    ),
    error = function(e) stop("Could not read '", source_file, "': ", conditionMessage(e), call. = FALSE)
  )
  require_columns(df_raw, c(
    "participant_id", "interaction_type", "application_label", "app_package_name",
    "event_timestamp", "start_timestamp", "stop_timestamp", "timezone"
  ))
  if (!nrow(df_raw)) stop("The input CSV is empty: '", source_file, "'.", call. = FALSE)

  participant_ids <- df_raw %>%
    transmute(participant_id = trimws(as.character(participant_id))) %>%
    filter(!is.na(participant_id), nzchar(participant_id)) %>%
    distinct() %>%
    pull(participant_id)
  if (length(participant_ids) != 1) {
    stop("Each input CSV must contain one non-blank participant_id. '", source_file,
      "' contains ", length(participant_ids), ".", call. = FALSE)
  }
  participant_id <- participant_ids[[1]]
  if (grepl("/", participant_id, fixed = TRUE) || grepl(intToUtf8(92), participant_id, fixed = TRUE)) {
    stop("participant_id contains a character that cannot be used in an output filename.", call. = FALSE)
  }

  timezone_counts <- df_raw %>%
    filter(!is.na(timezone), timezone != "") %>%
    count(timezone, sort = TRUE, name = "n")
  if (!nrow(timezone_counts)) {
    stop("No valid timezone values were found in '", source_file, "'.", call. = FALSE)
  }
  tz <- timezone_counts$timezone[[1]]

  df <- df_raw %>%
    distinct() %>%
    filter(timezone == tz) %>%
    mutate(
      event_posix = parse_event_timestamp(event_timestamp, tz),
      start_posix_raw = parse_start_stop_timestamp(start_timestamp, tz),
      stop_posix_raw = parse_start_stop_timestamp(stop_timestamp, tz),
      start_posix = coalesce(start_posix_raw, event_posix),
      stop_posix = stop_posix_raw,
      date_local = local_date(coalesce(start_posix, event_posix), tz)
    ) %>%
    arrange(participant_id, coalesce(start_posix, event_posix))
  if (!nrow(df)) {
    stop("No rows remained after selecting the primary timezone in '", source_file, "'.", call. = FALSE)
  }
  if (all(is.na(df$date_local))) {
    stop("No usable event/start timestamps were found in '", source_file, "'.", call. = FALSE)
  }

  df_to_collapse <- df %>%
    filter(interaction_type %in% config$collapse_interaction_types) %>%
    mutate(stop_for_logic = coalesce(stop_posix, start_posix)) %>%
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
      event_timestamp = first(event_timestamp),
      start_timestamp = first(start_timestamp),
      stop_timestamp = last(stop_timestamp),
      timezone = first(timezone),
      event_posix = first(event_posix),
      start_posix = first(start_posix),
      stop_posix = last(stop_for_logic),
      date_local = first(date_local),
      n_segments = n(),
      .groups = "drop"
    )

  df_passthrough <- df %>%
    filter(!interaction_type %in% config$collapse_interaction_types) %>%
    mutate(n_segments = 1L) %>%
    select(
      participant_id, interaction_type, app_package_name, application_label,
      event_timestamp, start_timestamp, stop_timestamp, timezone,
      event_posix, start_posix, stop_posix, date_local, n_segments
    )

  df2 <- bind_rows(df_to_collapse, df_passthrough) %>%
    arrange(participant_id, coalesce(start_posix, event_posix))

  df3 <- df2 %>%
    mutate(
      is_duration_type = interaction_type %in% config$duration_interaction_types,
      is_truncatable_type = interaction_type %in% config$duration_interaction_types & interaction_type != "nonuse",
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

  df4 <- df3 %>%
    mutate(
      is_bad_app = is_truncatable_type & app_package_name %in% bad_apps,
      is_bad_trunc = is_bad_app & !is.na(duration_secs) & duration_secs > config$max_bad_app_secs,
      original_stop_for_log = stop_for_duration,
      original_duration_for_log = duration_secs,
      stop_after_bad = if_else(is_bad_trunc, start_posix + seconds(config$max_bad_app_secs), stop_for_duration),
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
      participant_id, interaction_type, app_package_name, application_label,
      start = start_posix,
      stop_original = original_stop_for_log,
      duration_secs_original = round(original_duration_for_log, 1),
      stop_truncated = stop_after_bad,
      duration_secs_truncated = round(duration_after_bad, 1),
      seconds_trimmed = round(truncated_secs, 1),
      source_file
    )
  bad_trunc_counts <- bad_trunc_rows %>%
    group_by(participant_id, interaction_type, app_package_name, application_label) %>%
    summarise(
      n_truncated_events = n(),
      total_seconds_trimmed = sum(seconds_trimmed, na.rm = TRUE),
      .groups = "drop"
    )

  df5 <- df4 %>%
    mutate(
      stop_for_action_input = stop_after_bad,
      duration_for_action_input = as.numeric(difftime(stop_for_action_input, start_posix, units = "secs")),
      eligible_for_long_action =
        is_truncatable_type &
        !is.na(duration_for_action_input) &
        duration_for_action_input > config$long_6h_secs &
        !is_bad_trunc &
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
          apply_long_action(start_posix, stop_for_action_input, config$long_event_action,
            config$max_bad_app_secs, config$long_6h_secs),
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

  df6 <- df6 %>%
    mutate(
      stop_final = if_else(is_duration_type, stop_final, stop_posix),
      duration_secs = if_else(is_duration_type, duration_secs, as.numeric(NA))
    )

  long_rows <- df5 %>%
    filter(is_truncatable_type, !is.na(duration_for_action_input), duration_for_action_input >= config$long_3h_secs) %>%
    transmute(
      participant_id, interaction_type, app_package_name, application_label,
      start = start_posix,
      stop_original = stop_for_action_input,
      duration_secs_original = round(duration_for_action_input, 1),
      event_flags, eligible_for_long_action,
      long_action_applied = case_when(
        duration_for_action_input > config$long_6h_secs & eligible_for_long_action ~ config$long_event_action,
        TRUE ~ "none"
      ),
      was_dropped = case_when(long_action_applied == "drop" ~ "yes", TRUE ~ "no"),
      source_file
    ) %>%
    left_join(
      df6 %>% transmute(
        participant_id, interaction_type, app_package_name,
        start = start_posix,
        stop_final_logged = stop_final,
        duration_secs_final = round(duration_secs, 1),
        truncated_secs_logged = round(truncated_secs, 1)
      ),
      by = c("participant_id", "interaction_type", "app_package_name", "start")
    ) %>%
    mutate(was_dropped = case_when(
      is.na(stop_final_logged) & long_action_applied == "drop" ~ "yes",
      TRUE ~ "no"
    )) %>%
    rename(
      stop_final = stop_final_logged,
      truncated_secs = truncated_secs_logged
    )

  df_gap_base <- df6 %>%
    mutate(
      start_for_gap = coalesce(start_posix, event_posix),
      stop_for_gap = coalesce(stop_final, start_for_gap)
    )

  if (config$device_type == "parent") {
    gap_info <- df_gap_base %>%
      group_by(participant_id) %>%
      arrange(start_for_gap, .by_group = TRUE) %>%
      mutate(
        next_start = lead(start_for_gap),
        prev_stop = stop_for_gap,
        gap_hours = as.numeric(difftime(next_start, prev_stop, units = "hours"))
      ) %>%
      ungroup() %>%
      filter(!is.na(gap_hours), gap_hours > config$long_gap_hours)

    gap_days <- gap_info %>%
      mutate(
        end_day = local_date(prev_stop, tz),
        next_day = local_date(next_start, tz)
      ) %>%
      select(participant_id, end_day, next_day) %>%
      pivot_longer(c(end_day, next_day), names_to = "which", values_to = "date_local") %>%
      distinct() %>%
      mutate(is_gap_boundary = TRUE) %>%
      select(participant_id, date_local, is_gap_boundary)

    gaps_log <- gap_info %>%
      mutate(
        first_partial_date = local_date(prev_stop, tz),
        last_partial_date = local_date(next_start, tz),
        days_removed_for_this_gap = as.integer(last_partial_date - first_partial_date) + 1L
      ) %>%
      transmute(
        participant_id,
        stop_prev_local = prev_stop,
        start_next_local = next_start,
        gap_hours = round(gap_hours, 2),
        first_partial_date,
        last_partial_date,
        days_removed_for_this_gap,
        source_file
      )

    df7 <- df6 %>%
      left_join(gap_days, by = c("participant_id", "date_local")) %>%
      mutate(is_gap_boundary = replace_na(is_gap_boundary, FALSE))
  } else {
    gap_info <- tibble::tibble()
    gaps_log <- tibble::tibble()
    df7 <- df6 %>% mutate(is_gap_boundary = FALSE)
  }

  df_days <- df7 %>% distinct(participant_id, date_local)
  dst_by_day <- df_days %>%
    mutate(
      day_start = as.POSIXct(date_local, tz = tz),
      day_end = day_start + days(1) - seconds(1),
      is_dst_transition = dst(day_start) != dst(day_end)
    ) %>%
    select(participant_id, date_local, is_dst_transition)

  df8 <- df7 %>%
    group_by(participant_id) %>%
    mutate(
      first_day = min(date_local, na.rm = TRUE),
      last_day = max(date_local, na.rm = TRUE),
      is_first_day = date_local == first_day,
      is_last_day = date_local == last_day
    ) %>%
    ungroup() %>%
    left_join(dst_by_day, by = c("participant_id", "date_local")) %>%
    mutate(
      is_dst_transition = replace_na(is_dst_transition, FALSE),
      day_flags = NA_character_,
      day_flags = if_else(is_gap_boundary | is_first_day | is_last_day, add_flag(day_flags, "partial_day"), day_flags),
      day_flags = if_else(is_dst_transition, add_flag(day_flags, "DST_day"), day_flags)
    ) %>%
    select(-first_day, -last_day, -is_first_day, -is_last_day, -is_dst_transition)

  flagged_days <- df8 %>%
    filter(!is.na(day_flags), day_flags != "") %>%
    distinct(participant_id, date_local)
  partial_counts <- flagged_days %>%
    group_by(participant_id) %>%
    summarise(n_unique_partial_days_flagged = n_distinct(date_local), .groups = "drop") %>%
    mutate(source_file)
  partial_dates <- flagged_days %>% mutate(source_file)

  output <- df8 %>%
    mutate(
      start = ifelse(is.na(start_posix), NA, format(start_posix, "%Y-%m-%d %H:%M:%S")),
      stop = ifelse(is.na(stop_final), NA, format(stop_final, "%Y-%m-%d %H:%M:%S"))
    ) %>%
    transmute(
      participant_id, interaction_type, app_package_name, application_label,
      timezone, start, stop,
      duration_secs = round(duration_secs, 1),
      day_flags, event_flags,
      truncated_secs = round(truncated_secs, 1)
    ) %>%
    distinct() %>%
    arrange(participant_id, start)

  output_file <- paste0(participant_id, "_cleaned.csv")
  readr::write_csv(output, file.path(output_folder, output_file))
  logs_folder <- file.path(output_folder, "logs")
  additional_logs_folder <- file.path(logs_folder, "additional_logs")
  write_log_if_rows(long_rows, file.path(logs_folder, "log_long_events_3h_plus.csv"), format_posix = TRUE)
  write_log_if_rows(bad_trunc_rows, file.path(additional_logs_folder, "log_bad_apps_truncated_rows.csv"), format_posix = TRUE)
  write_log_if_rows(bad_trunc_counts, file.path(additional_logs_folder, "log_bad_apps_truncated_counts.csv"))
  write_log_if_rows(gaps_log, file.path(additional_logs_folder, "log_gaps_over_12h.csv"), format_posix = TRUE)
  write_log_if_rows(partial_counts, file.path(additional_logs_folder, "log_partial_days_flagged_per_participant.csv"))
  write_log_if_rows(partial_dates, file.path(additional_logs_folder, "log_partial_days_flagged_dates.csv"))

  browser_cleaning_result <<- tibble::tibble(
    output_file = output_file,
    rows_read = nrow(df_raw),
    rows_written = nrow(output),
    long_events = nrow(long_rows),
    bad_app_truncations = nrow(bad_trunc_rows),
    long_gaps = nrow(gap_info),
    flagged_days = nrow(flagged_days)
  )
  invisible(browser_cleaning_result)
}

browser_last_result <- function() {
  if (is.null(browser_cleaning_result)) stop("No file has been cleaned in this browser session.", call. = FALSE)
  browser_cleaning_result
}

reset_browser_workspace <- function() {
  unlink("/tmp/browser_output", recursive = TRUE, force = TRUE)
  unlink(c("/tmp/browser_input.csv", "/tmp/browser_bad_apps.csv", "/tmp/browser_config.json"), force = TRUE)
  browser_cleaning_result <<- NULL
  invisible(NULL)
}
