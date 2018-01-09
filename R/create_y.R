create_y_matrix <- function(data, i_var, t_var, g_def, time_span, y) {
  if (missing(data) | !is.data.frame(data))
    stop('Must supply a valid data.frame to create_y_matrix.')

  if (!all(c(i_var, t_var) %in% names(data)))
    stop('Individual or time variable not found in data.frame supplied to create_y_matix')

  if (missing(time_span)) {
    date_range <- c(min(data[[t_var]], na.rm = TRUE),
                   max(data[[t_var]], na.rm = TRUE))
  } else if (length(time_span) <= 2) {
    date_range <- c(time_span[[1]],
                   max(data[[t_var]], na.rm = TRUE))
  } else if (length(time_span) == 3) {
    date_range <- time_span[c(1, 3)]
  } else stop('Invalid time_span supplied to create_y_matrix.')

  # Create name versions of the input variables
  t_name <- as.name(t_var)
  i_name <- as.name(i_var)

  data <- remove_duplicates(data, i_name, t_name)

  my_data <- filter(data,
                    (!! t_name) >= date_range[[1]] &
                      (!! t_name) <= date_range[[2]])
  all_dates <- unique(my_data[[t_var]])
  num_dates <- length(all_dates)

  # Only keep observations with complete date information
  i_vals <- group_by(my_data, !! i_name) %>%
    summarize(n = length(!! t_name)) %>%
    filter(n == num_dates) %>%
    select(!! i_name)

  # Check if observations have been removed because of incomplete date inforation
  # and if so warn the user
  if (dim(my_data)[1] > dim(i_vals)[1]) {
    num_dropped <- setdiff(unique(my_data[i_var]), unique(i_vals[i_var])) %>%
      length()
    if (num_dropped > 0) {
      glue('Removed {num_dropped} observations for incomplete date information.') %>%
        warning()
    }
  }

  # Create group_def logical variable that is TRUE if observation is part of
  # treatment. (This is also where num_dropped observations are dropped.)
  is_treated <- eval(substitute(g_def), my_data, parent.frame())
  if (!is.logical(is_treated)) stop('g_def must supply a logical result.')

  mutate(my_data, group_def = is_treated) %>%
    merge(i_vals,
          by = i_var,
          all.y = TRUE) %>%
    select(!!! lapply(c(i_var, t_var, 'group_def', y), as.name)) %>%
    gather(key = y_var, value = y, -!!i_name, -!!t_name, -group_def)
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
