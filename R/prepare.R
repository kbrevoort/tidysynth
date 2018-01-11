#' Prepare Data
#'
#' This function processes the data before creating a synthetic panel.
#' @importFrom dplyr enquo
prepare_data <- function(data, i, t, treated_def, preperiod, y, ...) {
  if (missing(data) | !is.data.frame(data))
    stop('Must supply a valid data.frame to create_y_matrix.')

  i_name <- enquo(i) %>% quo_name() %>% as.name()
  t_name <- enquo(t) %>% quo_name() %>% as.name()
  if (!all(c(as.character(i_name), as.character(t_name)) %in% names(data)))
    stop('Individual or time variable not found in data.frame supplied to create_y_matix')

  # This step removes units (i) taht have multiple observations for the same date.
  # It also removes observations where the date information is missing.
  data <- remove_duplicates(data, i_name, t_name) %>%
    filter(!is.na(!! t_name))

  # Compile complete list of dates and remove units that don't have complete
  # date coverage.
  all_dates <- unique(data[[as.character(t_name)]])
  data <- remove_incomplete_dates(data, i_name, t_name, all_dates)

  # This creates a data.frame with two variables i and is_treated.
  # I will use this data.frame to map individual units into their treatment
  # state.
  treated_data <- define_treatment(data, i_name, enquo(treated_def))
  data <- select(treated_data, -is_treated) %>%
    merge(data,
          by = as.character(i_name),
          all = FALSE)

  y_matrix <- create_y_matrix(data, i_name, t_name, y)
  z_matrix <- create_z_matrix()

}

#' Remove Incomplete Dates
#'
#' This function removes observations from a data frame that do not have
#' observations for all of the available dates.
remove_incomplete_dates <- function(data, i_name, t_name, date_vec) {
  n_dates <- length(date_vec)

  ret_val <- group_by(data, !! i_name) %>%
    summarize(n = length(!! t_name)) %>%
    filter(n == n_dates) %>%
    merge(data,
          by = as.character(i_name),
          all.x = TRUE)

  # If none of the observations have complete dates, throw an error
  if (dim(ret_val)[1] == 0)
    stop('None of the observations in supplied data.frame have complete date coverage')

  ret_val
}

define_treatment <- function(data, i_name, treated_def) {
  treated_flag <- parse(text = quo_name(treated_def)) %>%
    eval(data, parent.frame())

  if (!is.logical(treated_flag))
    stop('treated_def must supply a logical result')

  treated_data <- mutate(data, is_treated = treated_flag) %>%
    select(!! i_name, is_treated)

  # Find units with inconsistent treatment/control designations
  inconsistent_data <- group_by(treated_data, !! i_name, is_treated) %>%
    summarize(n = length(is_treated)) %>%
    group_by(!! i_name) %>%
    summarize(n = length(n)) %>%
    filter(n > 1)

  inconsistent_count <- dim(inconsistent_data)[1]
  if (inconsistent_count > 0) {
    glue::glue('Removing {inconsistent_count} units for having inconsistent',
               'treatment designations. Consider revising your treatment',
               'definition.') %>%
      warning()

    # This section removes the inconsistent observations
    treated_data <- merge(treated_data, inconsistent_data,
                          by = as.character(i_name),
                          all.x = TRUE) %>%
      filter(n == 1) %>%
      select(-n)
  }

  treated_data
}
