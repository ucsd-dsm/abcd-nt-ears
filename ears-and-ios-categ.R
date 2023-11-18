#   ____________________________________________________________________________
#   Functions                                                               ####

#' Combine app categories
#'
#' Main function to combine Android and iOS app categories into common
#'   categories. Allows to select an aggregation type of interest and computes
#'   all combined categories as recommended by the ABCD NT workgroup. Returns
#'   either the complete data frame with the combined categories added, only
#'   the columns for the selected aggregation type with the combined categories
#'   added, or just the combined categories.
#'
#' @param data tbl. A data frame containing the EARS summary scores (table
#'   `nt_y_ears` in ABCD release 5.0)
#' @param outcome character. One of "min" (minutes; the default), "key"
#'   (keystrokes), or "ses" (sessions)
#' @param day character. One of "all" (all days, the default), "wkd" (weekdays),
#'   or "wke" (weekend)
#' @param score character. One of "sum" (sum, the default) or "avg" (average)
#' @param return character. One of "all" (retains all existing columns, appends
#'   the combined categories; the default), "selected" (retains only columns
#'   with the specified aggregation type, appends the combined categories), or
#'   "new" (retains only the combined categories)
#'
#' @return tbl. A data frame with the combined categories.
combine_app_categ <- function(data,
                              type    = c("kb", "st"),
                              outcome = c("min", "key", "ses"),
                              day     = c("all", "wkd", "wke"),
                              score   = c("sum", "avg"),
                              return  = c("all", "selected", "new")) {
  type    <- match.arg(type)
  outcome <- match.arg(outcome)
  day     <- match.arg(day)
  score   <- match.arg(score)
  return  <- match.arg(return)

  if (type == "st" & outcome %in% c("key", "ses")) {
    warning(
      "The only valid outcome for type 'st' is 'min'. ",
      "Other selections for type will be ignored!"
    )
    outcome <- "min"
  }

  # compute combined categories
  cols_regex <- create_regex(type, outcome, day, score)
  out        <- compute_all_comb_categ(data, cols_regex)

  # select columns to return
  if (return %in% c("selected", "new")) {
    if (return == "selected") {
      regex <- stringr::str_replace(
        cols_regex,
        "CATEGORY",
        ".*"
      )
    } else if (return == "new") {
      regex <- stringr::str_replace(
        cols_regex,
        "CATEGORY",
        "abcd_.*"
      )
    }

    out <- out |>
      dplyr::select(
        matches(regex)
      )

    if (day == "all") {
      out <- out |>
        dplyr::select(
          -matches("wke|wkd")
        )
    }
  }

  out
}

#' Create regex
#'
#' Helper function to select only columns that for a specified aggregation type.
#'   Uses the regular naming scheme of variables in table `nt_y_ears` to specify
#'   a regular expression template that is used to select columns.
#'
#'   Naming scheme:
#'
#'   - measure prefix: `nt_y_ears`
#'   - type of measurement
#'     - `kb`: keyboard
#'     - `st`: screen time measured by OS (currently only available for Android)
#'   - outcome:
#'     - `min`: minutes
#'     - `key`: keystrokes
#'     - `ses`: session
#'   - os (which store are categories from):
#'     - `and`: Android
#'     - `ios`: iOS
#'   - category
#'     - `art`
#'     - `auto`
#'     - … (abbreviations of all categories)
#'   - day
#'     - `wkd`: weekday
#'     - `wke`: weekend
#'   - score
#'     - `n`: number/count (only a few variables like number of apps used)
#'     - `sum`: sum
#'     - `avg`: average
#' @param outcome character. One of "min" (minutes; the default), "key"
#'   (keystrokes), or "ses" (sessions)
#' @param day character. One of "all" (all days, the default), "wkd" (weekdays),
#'   or "wke" (weekend)
#' @param score character. One of "sum" (sum, the default) or "avg" (average)
#'
#' @return character. A regular expression template
create_regex <- function(type    = c("kb", "st"),
                         outcome = c("min", "key", "ses"),
                         day     = c("all", "wkd", "wke"),
                         score   = c("sum", "avg")){
  type    <- match.arg(type)
  outcome <- match.arg(outcome)
  day     <- match.arg(day)
  score   <- match.arg(score)

  if (day == "all") {
    day  <- ""
  } else {
    day  <- paste0("_", day)
  }

  paste0(
    "nt_y_ears_",
    type,
    "_",
    outcome,
    "_CATEGORY",
    day,
    "_",
    score
  )
}

#' Compute combined category
#'
#' Helper function to compute a combined category from specified Android and iOS
#'   categories.
#'
#' @param data tbl. A data frame containing the EARS summary scores (table
#'   `nt_y_ears` in ABCD release 5.0)
#' @param categ character. The name of the combined category.
#' @param android character. The name of the Android category/ies to combine
#' @param ios character. The name of the iOS category/ies to combine
#' @param cols_regex character. Regex template to select columns for a specific
#'   aggregation type
#'
#' @return tbl. A data frame with the combined category added as a column
compute_comb_categ <- function(data,
                               categ,
                               android,
                               ios,
                               cols_regex){
  # name of new variable
  name <- stringr::str_replace(
    cols_regex,
    "CATEGORY",
    paste0("abcd_", categ)
  )

  # regular expression to select categories to combine
  regex <- stringr::str_replace(
    cols_regex,
    "CATEGORY",
    paste0("(", paste(c(android, ios), collapse = "|"), ")")
  )

  # create variable combining Android and iOS categories
  data |>
    dplyr::mutate(
      "{name}" := rowSums(
        dplyr::across(dplyr::matches(regex)),
        na.rm = TRUE
      )
    )
}

#' Compute all combined categories
#'
#' Helper function to compute all combined categories from specified Android and
#'   iOS as recommended by the ABCD NT workgroup.
#'
#' @param data tbl. A data frame containing the EARS summary scores (table
#'   `nt_y_ears` in ABCD release 5.0)
#' @param cols_regex character. Regex template to select columns for a specific
#'   aggregation type
#'
#' @return tbl. A data frame with the combined category added as a column
compute_all_comb_categ <- function(data, cols_regex) {
  data |>
    compute_comb_categ(
      categ      = "books",
      android    = "and_books",
      ios        = c("ios_reference", "ios_book"),
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "business",
      android    = "and_business",
      ios        = "ios_business",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "education",
      android    = "and_education",
      ios        = "ios_education",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "entertainment",
      android    = c("and_comics", "and_entertainment", "and_events"),
      ios        = "ios_entertainment",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "finance",
      android    = "and_finance",
      ios        = "ios_finance",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "food",
      android    = "and_food",
      ios        = "ios_food",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ   = "games",
      android = c(
        "and_gameaction",
        "and_gameadventure",
        "and_gamearcade",
        "and_gameboard",
        "and_gamecard",
        "and_gamecasino",
        "and_gamecasual",
        "and_gameeducation",
        "and_gamemusic",
        "and_gamepuzzle",
        "and_gameracing",
        "and_gameroleplay",
        "and_gamesimulation",
        "and_gamesports",
        "and_gamestrategy",
        "and_gametrivia",
        "and_gameword"
      ),
      ios = c(
        "ios_games",
        "ios_kids"
      ),
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "art",
      android    = "and_art",
      ios        = "ios_graphics",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "health",
      android    = "and_health",
      ios        = "ios_health",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "lifestyle",
      android    = c(
        "and_beauty",
        "and_house",
        "and_lifestyle",
        "and_parenting"
      ),
      ios        = "ios_lifestyle",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "music",
      android    = "and_music",
      ios        = "ios_music",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "navigation",
      android    = "and_maps",
      ios        = "ios_navigation",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "news",
      android    = "and_news",
      ios        = c("ios_magazines", "ios_news"),
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "medical",
      android    = "and_medical",
      ios        = "ios_medical",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "photography",
      android    = c("and_photography", "and_video"),
      ios        = "ios_photo",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "productivity",
      android    = "and_productivity",
      ios        = "ios_productivity",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "shopping",
      android    = c("and_auto", "and_shopping"),
      ios        = "ios_shopping",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "social",
      android    = c("and_communication", "and_dating", "and_social"),
      ios        = "ios_social",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "sports",
      android    = "and_sports",
      ios        = "ios_sports",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "travel",
      android    = "and_travel",
      ios        = "ios_travel",
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "tools_utilities",
      android    = c("and_tools", "and_libraries", "and_personalization"),
      ios        = c("ios_tools", "ios_utilities"),
      cols_regex = cols_regex
    ) |>
    compute_comb_categ(
      categ      = "weather",
      android    = "and_weather",
      ios        = "ios_weather",
      cols_regex = cols_regex
    )

}


#   ____________________________________________________________________________
#   Example use of functions                                                ####

# read in `nt_y_ears` table
# (csv file from release has to be copied into the `data/` directory)
data <- readr::read_csv("data/nt_y_ears.csv")

# Note:
#
# The following options can be chosen when using function `combine_app_categ()`:
#
# - type
#   - "kb": keyboard (default)
#   - "st": screen time measured by OS (currently only available for Android)
# - outcome:
#   - "min": minutes (default; for type 'kb', this is the only valid option)
#   - "key": keystrokes
#   - "ses": session
# - day
#   - "all": all days (default)
#   - "wkd": weekday
#   - "wke": weekend
# - score
#   - "sum": sum (default)
#   - "avg": average
# - return
#   - "all": retains all existing columns, appends the combined categories
#     (default)
#   - "selected": retains only the columns that where selected to be combined,
#     appends the combined categories
#   - "new": retains only the combined categories

# Example 1
# - selection: keyboard sum of minutes for all days (standard settings)
# - return: new columns appended to complete data frame
data_example_1 <- data |>
  combine_app_categ()

# Example 2
# - selection: keyboard sum of keystrokes for weekend days
# - return: new columns appended to selected columns
data_example_2 <- data |>
  combine_app_categ(
    outcome = "min",
    day     = "wke",
    return  = "selected"
  )

# Example 3
# - selection: keyboard average sessions for weekdays
# - return: only new columns
data_example_3 <- data |>
  combine_app_categ(
    outcome = "ses",
    score   = "avg",
    day     = "wkd",
    return  = "new"
  )

# Example 4
# - selection: screentime average minutes for weekend days
# - return: new columns appended to selected columns
data_example_4 <- data |>
  combine_app_categ(
    type    = "st",
    day     = "wke",
    return  = "selected"
  )

# Example 5
# combine several function calls, in this case minutes for both keyboard and
# screentime minutes for all days, weekdays, and weekend days
data_example_5 <- data |>
  combine_app_categ(
    day     = "all"
  ) |>
  combine_app_categ(
    day     = "wkd"
  ) |>
  combine_app_categ(
    day     = "wke"
  ) |>
  combine_app_categ(
    type    = "st",
    day     = "all"
  ) |>
  combine_app_categ(
    type    = "st",
    day     = "wkd"
  ) |>
  combine_app_categ(
    type    = "st",
    day     = "wke"
  )
