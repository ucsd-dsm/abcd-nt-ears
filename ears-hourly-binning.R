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

# rows with observations that do not cross hours
keyboard_unchanged <- keyboard |>
  filter(
    hour(tm_session_start) == hour(tm_session_end)
  ) |>
  mutate(
    amt_keyboard_session_sec = lubridate::as.duration(amt_keyboard_session_sec)
  )

# first hour of rows crossing hours
keyboard_h1 <- keyboard |>
  filter(
    hour(tm_session_start) != hour(tm_session_end)
  ) |>
  mutate(
    amt_keyboard_session_sec = amt_keyboard_session_sec -
      (tm_session_end - floor_date(tm_session_end, unit = "hour")),
    tm_session_end = ceiling_date(tm_session_start, unit = "hour")
  )

# second hour of rows crossing hours
keyboard_h2 <- keyboard |>
  filter(
    hour(tm_session_start) != hour(tm_session_end)
  ) |>
  mutate(
    amt_keyboard_session_sec = tm_session_end -
      floor_date(tm_session_end, unit = "hour"),
    tm_session_start = floor_date(tm_session_end, unit = "hour"),
    n_record_session = 0
  )

# combine
keyboard_transf <- bind_rows(
  keyboard_unchanged,
  keyboard_h1,
  keyboard_h2
)


# compute hourly keyboard use --------------------------------------------------

keyboard_hourly <- keyboard_transf |>
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

# rows with observations that do not cross hours
screentime_unchanged <- screentime |>
  filter(
    hour(tm_usagewindow_start) == hour(tm_usagewindow_end)
  ) |>
  mutate(
    n_foreground_sec = lubridate::as.duration(n_foreground_sec)
  )

# first hour of rows crossing hours
screentime_h1 <- screentime |>
  filter(
    hour(tm_usagewindow_start) != hour(tm_usagewindow_end)
  ) |>
  mutate(
    n_foreground_sec = n_foreground_sec -
      (tm_usagewindow_end - floor_date(tm_usagewindow_end, unit = "hour")),
    tm_usagewindow_end = ceiling_date(tm_usagewindow_start, unit = "hour")
  )

# second hour of rows crossing hours
screentime_h2 <- screentime |>
  filter(
    hour(tm_usagewindow_start) != hour(tm_usagewindow_end)
  ) |>
  mutate(
    n_foreground_sec = tm_usagewindow_end -
      floor_date(tm_usagewindow_end, unit = "hour"),
    tm_usagewindow_start = floor_date(tm_usagewindow_end, unit = "hour")
  )

# combine
screentime_transf <- bind_rows(
  screentime_unchanged,
  screentime_h1,
  screentime_h2
)


# compute hourly screentime use ------------------------------------------------

screentime_hourly <- screentime_transf |>
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

