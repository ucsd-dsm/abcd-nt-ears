library(dplyr)
library(lubridate)
library(ggplot2)

#   ____________________________________________________________________________
#   extract data                                                            ####

# unzip raw data files into temporary directory
temp_dir <- tempfile()
purrr::walk(
  fs::dir_ls("data/abcd_earsraw01/"),
  ~ zip::unzip(.x, exdir = temp_dir)
)

# extract raw data into data frames
keyboard_raw <- purrr::map_dfr(
  fs::dir_ls(temp_dir, glob = "*_KeyInputTimestamps_session_*"),
  ~ readr::read_csv(.x)
)

screentime_raw <- purrr::map_dfr(
  fs::dir_ls(temp_dir, glob = "*_APPUSAGE_usageLog_*"),
  ~ readr::read_csv(.x)
)


#   ____________________________________________________________________________
#   transform data                                                          ####

#   ............................................................................
#   functions                                                               ####

#' Distribute seconds between hours
#'
#' Given a total number of seconds and the seconds in the first hour of
#'   observations that span at least one hour mark, distributes the remaining
#'   seconds among the different hours/rows.
#'
#' @param secs double. Total number of seconds.
#' @param secs_1st double. Seconds in the first hour.
#'
#' @return double vector. Vector of seconds distributed over hours.
distribute_secs_hourly <- function(secs, secs_1st) {
  n_interm  <- floor((secs - secs_1st) / 3600)
  secs_last <- secs - secs_1st - n_interm * 3600

  c(
    secs_1st,
    rep(60, n_interm),
    secs_last
  )
}

#' Split oberservation
#'
#' Given a start time, end time, and total number of seconds, returns a tibble
#'   with one row per hour and seconds distributed over the hours.
#'
#' @param time_start datetime. Start time of observation.
#' @param time_end datetime. End time of observation.
#' @param var_secs double. Seconds recorded for the observation
#'
#' @return tibble. A data frame with one row per hour.
split_obs <- function(time_start, time_end, var_secs) {
  time_end_1st <- ceiling_date({{ time_start }}, "hour")
  time_end_oth <- seq(time_end_1st, floor_date({{ time_end }}, "hour"), "hour")

  var_secs_1st <- interval(
    {{ time_start }},
    ceiling_date({{ time_start }}, "hour")
  ) / seconds(1)

  tibble(
    start = c({{ time_start }}, time_end_oth),
    end   = c(time_end_oth, {{ time_end }}),
    secs  = distribute_secs_hourly({{ var_secs }}, var_secs_1st)
  ) |>
    # remove extra row for cases where end time is exactly at the hour mark
    # also recompute app use end time as app use start time + seconds of use?
    filter(
      start != end
    )
}

#' Split multi-hour observations
#'
#' Given a data frame with observations, splits those observations that span at
#'   least one hour mark into as many rows as there are hours between start and
#'   end datetime and distributes seconds appropriately to those hours.
#'
#'   Optionally, adds other variable(s) to the resulting data frame by
#'   assigning their value only to the first row and setting the others to 0.
#'
#' @param data tibble. A data frame with one observation per row.
#' @param time_start unquoted expression. Datetime column recording the start
#'   of the observation.
#' @param time_end unquoted expression. Datetime column recording the end of
#'   the observation.
#' @param var_secs unquoted expression. Column recording the seconds of the
#'   observations.
#' @param vars_oth character (vector). Additional column(s) to include
#'   (Optional; default = `NULL`)
#'
#' @return tibble. Data frame with observations spanning the hour mark being
#'   split into one observation per hour.
split_multi_hour_obs <- function(data,
                                 time_start,
                                 time_end,
                                 var_secs,
                                 vars_oth = NULL) {
  # add helper columns
  data <- data |>
    mutate(
      start_hour_floor   = floor_date({{ time_start }}, unit = "hour"),
      start_hour_ceiling = ceiling_date({{ time_start }}, unit = "hour"),
      end_hour_floor     = floor_date({{ time_end }}, unit = "hour")
    )

  # observations that do not cross the hour mark -------------------------------

  data_out_no_cross <- data |>
    filter(
      start_hour_floor   == end_hour_floor |
      start_hour_ceiling == {{ time_end}}
    )

  # observations that cross the hour mark --------------------------------------

  data_cross <- data |>
    filter(
      start_hour_floor   != end_hour_floor &
      start_hour_ceiling != {{ time_end}}
    )

  data_out_cross <- data_cross |>
    mutate(
      data = purrr::pmap(
        list({{ time_start }}, {{ time_end }}, {{ var_secs }}),
        split_obs
      )
    ) |>
    select(
      -c(
        {{ time_start }},
        {{ time_end }},
        {{ var_secs }}
      ),
    ) |>
    tidyr::unnest(
      data
    ) |>
    rename(
      {{ time_start }} := start,
      {{ time_end }}   := end,
      {{ var_secs }}   := secs
    )

  # add value for selected other fields to the first row of observations that
  # cross the hour mark and are split into several rows; all other rows set to 0
  if (!is.null(vars_oth)) {
    data_oth <- data_cross |>
      select(
        id_app,
        id_participant,
        {{ time_start }},
        all_of(vars_oth)
      )

    data_out_cross <- data_out_cross |>
      select(
        -all_of(vars_oth)
      ) |>
      left_join(
        data_oth,
        by = join_by(id_participant, id_app, {{ time_start }})
      ) |>
      mutate(
        across(
          all_of(vars_oth),
          ~ tidyr::replace_na(.x, 0)
        )
      )
  }

  # combine --------------------------------------------------------------------

  bind_rows(
    data_out_no_cross,
    data_out_cross
  ) |>
    select(
      -matches("_floor$|_ceiling$")
    )
}


#   ............................................................................
#   keyboard: seconds of use / number of keystrokes                         ####

# KSANA recommended to remove the following app IDs
id_app_exclude <- c(
  "com.android.systemui",
  "com.lge.qmemoplus",
  "com.google.android.gms",
  "com.sec.android.app.launcher",
  "com.sec.android.mimage.photoretouching",
  "us.ozteam.bigfoot",
  "com.pixel.art.coloring.color.number",
  "com.robtopx.geometryjumplite",
  "com.google.android.inputmethod.latin",
  "com.lge.clock",
  "com.pt.bark",
  "com.tct.launcher",
  "com.lge.launcher3",
  "royaln.Removeunwantedcontent",
  "com.samsung.android.contacts",
  "com.google.android.packageinstaller",
  "com.wssyncmldm"
)

keyboard <- keyboard_raw |>
  # exclude app IDs
  filter(
    !id_app %in% id_app_exclude
  ) |>
  # remove extraneous variables
  select(
    -c(
      timezone,
      epoch_session_start,
      epoch_session_end
    )
  )

# split multi-hour observations -----------------------------------------------

# Note:
#
# Some observations will start in one hour and end in another. We need to
# separate these into different observations (two rows instead of one). Seconds
# of use can be allocated correctly to either of the two hours. In contrast,
# there is not a way to infer in what hour the keystrokes occurred for keyboard
# instances that start and end in different hours. We will, imperfectly, assume
# that keystrokes were recorded in the same hour that the keyboard use began.

keyboard_split <- split_multi_hour_obs(
  data       = keyboard,
  time_start = tm_session_start,
  time_end   = tm_session_end,
  var_secs   = amt_keyboard_session_sec,
  vars_oth   = "n_record_session"
)

# compute hourly keyboard use --------------------------------------------------

keyboard_hourly <- keyboard_split |>
  mutate(
    date = date(tm_session_start),
    hour = hour(tm_session_start)
  ) |>
  group_by(
    id_participant,
    date,
    hour
  ) |>
  summarize(
    keyboard_secs    = sum(amt_keyboard_session_sec),
    keyboard_strokes = sum(n_record_session),
    .groups = "drop"
  )

keyboard_hourly <- keyboard_hourly |>
  distinct(
    id_participant,
    date
  ) |>
  tidyr::crossing(
    hour = seq(0, 23, 1)
  ) |>
  left_join(
    keyboard_hourly,
    by = join_by(id_participant, date, hour)
  ) |>
  mutate(
    keyboard_secs    = tidyr::replace_na(keyboard_secs, 0),
    keyboard_strokes = tidyr::replace_na(keyboard_strokes, 0),
    wday = lubridate::wday(date, label = TRUE, abbr = TRUE),
    wknd = if_else(
      wday %in% c('Sat','Sun'),
      "Weekend",
      "Weekday"
    )
  )


#   ............................................................................
#   screen time                                                             ####

screentime <- screentime_raw |>
  mutate(
    # convert app use from milliseconds to seconds
    n_foreground_sec   = n_foreground_ms/1000,
    # recompute app use end time as app use start time + seconds of use
    tm_usagewindow_end = tm_usagewindow_start + n_foreground_sec
  ) |>
  # remove all observations > 20 hours (consistent w/ summary measures)
  filter(
    n_foreground_sec <= 20*60*60
  ) |>
  # exclude app IDs
  filter(
    !id_app %in% id_app_exclude
  ) |>
  # remove extraneous variables
  select(
    -c(
      timezone,
      epoch_usagewindow_start,
      epoch_usagewindow_end,
      n_foreground_ms
    )
  )

# split multi-hour observations -----------------------------------------------

# Note:
#
# Some observations will start in one hour and end in another. We need to
# separate these into different observations (two rows instead of one). Seconds
# of screen time use are allocated appropriately to either of the two hours.

screentime_split <- split_multi_hour_obs(
  data       = screentime,
  time_start = tm_usagewindow_start,
  time_end   = tm_usagewindow_end,
  var_secs   = n_foreground_sec
)

# compute hourly screentime use ------------------------------------------------

screentime_hourly <- screentime_split |>
  mutate(
    date = date(tm_usagewindow_start),
    hour = hour(tm_usagewindow_start)
  ) |>
  group_by(
    id_participant,
    date,
    hour
  ) |>
  summarize(
    screentime_secs = sum(n_foreground_sec),
    .groups = "drop"
  )

screentime_hourly <- screentime_hourly |>
  distinct(
    id_participant,
    date
  ) |>
  tidyr::crossing(
    hour = seq(0, 23, 1)
  ) |>
  left_join(
    screentime_hourly,
    by = join_by(id_participant, date, hour)
  ) |>
  mutate(
    screentime_secs = tidyr::replace_na(screentime_secs, 0),
    wday = lubridate::wday(date, label = TRUE, abbr = TRUE),
    wknd = if_else(
      wday %in% c('Sat','Sun'),
      "Weekend",
      "Weekday"
    )
  )


#   ____________________________________________________________________________
#   plot data                                                               ####

#   ............................................................................
#   plot function                                                           ####

#' Plot phone use by hour
#'
#' Creates a line graph visualizing hourly phone use over all hours of the day.
#'   Different outcomes (keyboard seconds, keyboard strokes, screentime) can be
#'   plotted. Additionally, different lines can be drawn for days during the
#'   weekend and week.
#'
#' @param data tibble. A data frame containing the hourly binned phone use data.
#' @param var character. Name of the variable / outcome measure to plot.
#' @param y_lab character. y-axis label.
#' @param y_lim numeric vector. minimum and maximum to display on the y-axis.
#' @param title character. Plot title.
#' @param subtitle character. Plot subtitle (Default: `NULL`).
#' @param group character. Variable to group by (Default: `NULL`).
#' @param scale numeric. Number to divide the outcome measure by (Default: 1)
#'
#' @return
plot_use_by_hour <- function(data,
                             var,
                             y_lab,
                             y_lim,
                             title,
                             subtitle = NULL,
                             group    = NULL,
                             scale    = 1) {
  plot_data <- data |>
    mutate(
      hour = lubridate::as_datetime(lubridate::hm(paste0(hour, ":00")))
    ) |>
    group_by(
      across(all_of(c("hour", group)))
    ) |>
    summarise(
      outcome = mean(.data[[var]]) / scale,
      .groups = "drop"
    )

  plot <- ggplot(
    data    = plot_data,
    mapping = aes(
      x = hour,
      y = outcome
    )
  )

  if (!is.null(group)) {
    plot <- plot +
      geom_line(
        mapping = aes(
          group = .data[[group]],
          color = .data[[group]]
        ),
        linewidth = 1.5
      )
  } else {
    plot <- plot +
      geom_line(
        linewidth = 1.5
      )
  }

  plot +
    scale_x_datetime(
      date_breaks = "1 hour",
      date_labels = "%H:%M",
      expand      = c(0.02, 0)
    ) +
    theme_classic() +
    theme(
      axis.text.x  = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
      axis.title   = element_text(size = 8, face = "bold"),
      legend.title = element_blank()
    ) +
    ylim(y_lim) +
    ylab(y_lab) +
    ggtitle(title, subtitle)
}


#   ............................................................................
#   generate plots                                                          ####

# keyboard seconds (ungrouped)
plot_use_by_hour(
  data  = keyboard_hourly,
  var   = "keyboard_secs",
  y_lim = c(0, 120),
  y_lab = "Seconds",
  title = "Mean Hourly Keyboard Usage"
)

# keyboard seconds (grouped by weekday)
plot_use_by_hour(
  data  = keyboard_hourly,
  var   = "keyboard_secs",
  group = "wknd",
  y_lim = c(0, 120),
  y_lab = "Seconds",
  title = "Mean Hourly Keyboard Usage"
)


# keyboard strokes (ungrouped)
plot_use_by_hour(
  data  = keyboard_hourly,
  var   = "keyboard_strokes",
  y_lim = c(0, 160),
  y_lab = "Keystroke count",
  title = "Mean Hourly Keystroke Count"
)

plot_use_by_hour(
  data   = keyboard_hourly,
  var    = "keyboard_strokes",
  group  = "wknd",
  y_lim = c(0, 160),
  y_lab = "Keystroke count",
  title = "Mean Hourly Keystroke Count"
)


# screentime seconds (ungrouped)
plot_use_by_hour(
  data     = screentime_hourly,
  var      = "screentime_secs",
  y_lim    = c(0, 25),
  y_lab    = "Minutes",
  scale    = 60,
  title    = "Mean Hourly Passively Measured Screentime",
  subtitle = "(Android Users Only)"
)

# screentime seconds (grouped by weekday)
plot_use_by_hour(
  data     = screentime_hourly,
  var      = "screentime_secs",
  group    = "wknd",
  y_lim    = c(0, 25),
  y_lab    = "Minutes",
  scale    = 60,
  title    = "Mean Hourly Passively Measured Screentime",
  subtitle = "(Android Users Only)"
)

