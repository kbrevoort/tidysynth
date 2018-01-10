#' Create Y Matrix
#'
#' This function creats a data.frame that contains the information on the time-
#' varying variables.
#' @param data A data.frame
#' @param i_var A character variable with the name of the unit variable (i)
#' @param t_var A character variable with the name of the time variable (t)
#' @param g_def A character variable with the boolean logic to identify observations
#' in the treatment group.
#' @param time_span An optional vector with up to three values:  The earliest value
#' of t_var allowed, the latest value that is part of the pre-treatment period,
#' and the latest time period allowd.
#' @param y A character variable with the name of any y variables to include
#' @importFrom dplyr select mutate
#' @export
create_y_matrix <- function(data, i_name, t_name, y) {
  # Make sure that all y are in data
  if (any(y %notin% names(data)))
    stop('Variable names included in y are not found on the data.frame supplied.')

  y_data <- select(!!! lapply(c(as.character(i_name),
                      as.character(t_name),
                      y),
                    as.name)) %>%
    gather(key = variable, value = value, -!!i_name, -!!t_name)

  # Remove any units that have 1 or more missing values of the y-variables
  bad_obs <- filter(y_data, is.na(value)) %>%
    select(!! i_name) %>%
    unique()

  num_bad <- dim(bad_obs)[1]
  if (num_bad > 0) {
    glue::glue('Removing {num_bad} units that have missing y variables') %>%
      warning()

    y_data <- anti_join(y_data, bad_obs)
  }

  y_data
}

#' Remove Observations (i) with Duplicate Time Values
#'
#' This function takes a data.frame and determines whether any of the individuals
#'
remove_duplicates <- function(data, i_name, t_name) {
  if (!is.name(i_name)) i_name <- as.name(i_name)
  if (!is.name(t_name)) t_name <- as.name(t_name)

  duplicate_obs <- group_by(data, !! i_name, !! t_name) %>%
    summarize(n = length(!! t_name)) %>%
    group_by(!! i_name) %>%
    summarize(n = max(n)) %>%
    filter(n > 1) %>%
    select(!! i_name) %>%
    unique()

  if (dim(duplicate_obs)[1] > 0) {
    glue::glue('Removing {dim(duplicate_obs)[1]} observation(s) with multiple observations for the same time period.') %>%
      warning()

    data <- anti_join(data, duplicate_obs)
  }

  data
}
