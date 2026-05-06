# Chronicle Preprocessed Cleaning

## Version
This repository corresponds to version **v1.0**.

---

## Purpose

This repository provides a cleaning pipeline for **preprocessed Chronicle-compatible Android event-log data**.

The script is designed to clean outputs from the companion preprocessing repository, but it can also be used with other preprocessing workflows if the input files contain the required columns listed below.

The cleaning pipeline standardises timestamps, reconstructs split app-usage segments, identifies implausibly long events, truncates configurable problematic/background app events, and flags days that may be incomplete or unreliable.

---

## Companion Repository

This repository is intended to work alongside a separate preprocessing repository:

> https://github.com/joshculverhouse/chronicle-android-preprocessing

However, this cleaning script is deliberately written as a reusable tool. It does **not** require that the data were created by the companion preprocessing script, as long as the input files follow the expected structure.

---

## Expected Input

Each input CSV should contain the following columns:

| Column | Description |
|--------|-------------|
| `participant_id` | Participant or device identifier |
| `interaction_type` | Event type, e.g. `App Usage`, `Session`, `Glance` |
| `application_label` | Human-readable app label, where available |
| `app_package_name` | Android package name |
| `event_timestamp` | Original event timestamp |
| `start_timestamp` | Event/session/app-use start timestamp |
| `stop_timestamp` | Event/session/app-use stop timestamp |
| `timezone` | Timezone associated with the event |

Optional upstream columns may be present, but are not required.

---

## To Run

1. Open `clean_preprocessed.R`

2. Set the core paths at the top of the script:

```r
config <- list(
  input_folder  = "path/to/preprocessed_csvs",
  output_folder = "path/to/cleaned_output",
  bad_apps_file = "config/bad_apps.csv",
  device_type   = "parent"
)
```

3. Review the optional parameters if needed.

4. Run the script.

The script will write cleaned participant-level CSV files to the output folder and write audit logs to an automatically created `logs/` subfolder.

---

## Key Parameters

| Parameter | Default | Description |
|----------|---------|-------------|
| `device_type` | `"parent"` | Use `"parent"` to apply >12h gap boundary day flagging; use `"child"` to skip this rule. |
| `duration_interaction_types` | `c("App Usage", "Session", "Glance")` | Interaction types treated as duration-bearing rows |
| `collapse_interaction_types` | `c("App Usage")` | Interaction types where adjacent split segments are collapsed. |
| `max_bad_app_secs` | 10 minutes | Maximum duration retained for apps listed in `bad_apps.csv` |
| `long_3h_secs` | 3 hours | Threshold for logging long events for review |
| `long_6h_secs` | 6 hours | Threshold for applying long-event actions |
| `long_gap_hours` | 12 hours | Threshold for flagging long data gaps on parent devices |
| `session_gap_secs` | 1 second | Maximum gap allowed when collapsing adjacent app-usage segments |
| `long_event_action` | `"truncate_to_bad_app"` | Action applied to eligible events >6h |
| `apply_long_event_action_to` | `"all"` | Apply long-event action to all duration-bearing rows or only app usage |

---

## Bad Apps Configuration

Problematic or background/system apps are stored in:

```text
bad_apps.csv
```

The CSV should contain:

```csv
app_package_name
com.android.launcher
com.google.android.deskclock
...
```

Apps in this list are **not removed automatically**. Instead, when they produce duration-bearing events longer than `max_bad_app_secs`, those events are truncated and logged.

This makes the cleaning process more transparent than silently excluding packages during preprocessing.

Users should review and modify `bad_apps.csv` for their own study context.

The overall assumption is that only certain apps would feasible run for >3 hours. For example, video players.

---

## Cleaning Steps

### 1. Load Data

The script reads all CSV files in the input folder. Each file is processed independently, usually corresponding to one participant or device.

Required columns are checked before processing.

---

### 2. Timezone Handling

For each file, the script identifies the most frequent timezone and treats it as the file’s primary timezone.

If multiple timezones are present, the script filters to the primary timezone.

---

### 3. Collapse Split App Usage

Some preprocessing workflows may split a continuous app-use episode into multiple adjacent/contiguous events/rows. These are considered to actually reflect continuous usage (e.g. multiple app instances actually reflects interactions within a single app usage).

To avoid inflating event counts, adjacent events are merged into single sessions.

By default, the script collapses adjacent `App Usage` rows when they:

- Belong to the same participant
- Have the same app package name
- Are separated by no more than `session_gap_secs`

---

### 4. Flag Long Events

Duration-bearing rows are flagged if they exceed:

- 3 hours (`long_3h`)
- 6 hours (`long_6h`)

Long events are logged for review.

---

### 5. Truncate Bad Apps

Apps listed in `bad_apps.csv` are treated as potentially implausible/background apps.

If one of these apps has a duration greater than `max_bad_app_secs`, the event is truncated to that threshold and flagged as:

```text
bad_app_truncated
```

All truncations are logged.

---

### 6. Apply Long-Event Action

For events still exceeding the 6-hour threshold, the script applies the configured `long_event_action`.

Available options are:

| Option | Behavior |
|--------|----------|
| `"none"` | Retain the event as-is but log it |
| `"truncate_to_bad_app"` | Truncate to `max_bad_app_secs` |
| `"truncate_to_6h"` | Truncate to 6 hours |
| `"drop"` | Remove eligible events |

---

### 7. Detect Long Gaps

For `device_type = "parent"`, the script identifies gaps greater than `long_gap_hours` between any recorded events (not just apps usage or pickup events).

The days at the start and end of each long gap are flagged as partial days.

For `device_type = "child"`, this long-gap boundary rule is skipped.

The assumption is that for adult/parent phones, there is unlikely to be >12h gap between events, as notifications and background events occur even when the device is idle. However, young child tablet data may feasibly have >12h gaps in any interaction type than adult phone use because these devices 1) have much lighter usage 2) generate fewer notification and background events. 

---

### 8. Flag Partial and DST Days

Days are flagged when they are:

- First or last observed day for a participant (assumed as partial)
- Boundary days around long data gaps (assumed as partial)
- Daylight Saving Time transition days

These flags are stored in `day_flags`.

Flagged days are retained in the output so users can decide how to handle them in downstream analyses.

---

## Output

The script writes one cleaned CSV per participant.

Each cleaned file contains:

| Column | Description |
|--------|-------------|
| `participant_id` | Participant/device identifier |
| `interaction_type` | Event type |
| `app_package_name` | Android package name |
| `application_label` | Human-readable app label |
| `timezone` | File-level primary timezone |
| `start` | Cleaned local start timestamp |
| `stop` | Cleaned local stop timestamp |
| `duration_secs` | Cleaned duration in seconds |
| `day_flags` | Day-level quality flags |
| `event_flags` | Event-level cleaning flags |
| `truncated_secs` | Number of seconds removed by truncation |

---

## Logs

Logs are written to:

```text
output_folder/logs/
```

Potential log files include:

| Log file | Description |
|----------|-------------|
| `log_bad_apps_truncated_rows.csv` | Row-level log of bad-app truncations |
| `log_bad_apps_truncated_counts.csv` | Summary of bad-app truncations by participant and app |
| `log_long_events_3h_plus.csv` | Events lasting at least 3 hours |
| `log_gaps_over_12h.csv` | Long gaps between events |
| `log_partial_days_flagged_per_participant.csv` | Number of flagged days per participant |
| `log_partial_days_flagged_dates.csv` | Dates flagged as partial/unreliable |

---

## Recommended Review Step

Before analysis, review:

```text
log_long_events_3h_plus.csv
```

Long events may reflect:

- Legitimate long usage
- Background processes
- Launcher/screen saver behavior
- App-specific logging artifacts

If additional implausible apps are identified, add them to `bad_apps.csv` and re-run the script.

---

## Notes

- The script is intended for researchers with some R experience.
- Most users only need to set the input/output paths and review `bad_apps.csv`.
- Advanced users can modify thresholds and long-event behavior in the config section.
- Cleaning decisions are logged for transparency and reproducibility.

---
