#' Create Z Matrix
#'
#' This function creates a Z matrix for the synthetic panel.  It contains one
#' observation for each unit (i) and one column for each variable to be used as
#' a control.
#' @param data A data.frame
#' @param ... Variables to be included as controls
#' @importFrom dplyr quos "%>%"
#' @importFrom tidyr gather
#' @export
create_z_matrix <- function(data, i_name, t_name, ...) {
  my_vars <- dplyr::quos(...)

  lapply(seq(1, length(my_vars)), convert_quo_value, my_vars) %>%
    lapply(make_z_variable, data = data, i_name = i_name, t_name = t_name) %>%
    Reduce(function(x, y) merge(x, y, all = TRUE), .) %>%
    gather(key = variable, value = value, -!!i_name)

}

#' Convert Quo Value to Component parts
#'
#' Each quo value submitted to create_z_matrix has three component parts:  a
#' variable name, the command, and restictions on the time span.
#' @param x The value attribute of a quosure produced by the quos function.
#' @return A list with three attributes:  name (character), formula (quosure),
#' and filter (numeric or date vector)
#' @importFrom dplyr quo_name "%>%"
#' @importFrom stringr str_split str_trim
convert_quo_value <- function(x, quo_list) {
  this_quo <- quo_list[x]

  ret_val <- list()

  formula_and_time <- quo_name(this_quo[[1]]) %>%
    stringr::str_split('\\|', n = 2L) %>%
    unlist()

  # Set name equal to the given name or the formula used to contruct the
  # variable.
  my_name <- names(this_quo)
  if (my_name == '') {
    if (length(formula_and_time) == 2L) {
      my_name <- paste(formula_and_time, collapse = '_')
    } else {
      my_name <- formula_and_time
    }
  }
  ret_val$name <- stringr::str_trim(my_name) %>% as.name()

  ret_val$formula <- stringr::str_trim(formula_and_time[[1]])
  if (length(formula_and_time) == 2L) {
    ret_val$filter <- stringr::str_trim(formula_and_time[[2]]) %>%
      parse(text = .) %>%
      eval()
  } else {
    ret_val$filter <- TRUE
  }

  ret_val
}

#' Make Individual Z Variable
#'
#' This function takes the inpute produced by convert_quo_value for a single
#' control variable and returns a data.frame with its value.
#' @importFrom dplyr filter group_by summarize
#' @importFrom rlang parse_expr
make_z_variable <- function(quo_list, data, i_name, t_name) {
  if (is.numeric(quo_list$filter)) {
    data <- filter(data, (!! t_name)  %in% quo_list$filter)
  }

  group_by(data, !! i_name) %>%
    summarize(!! quo_list$name := (!! rlang::parse_expr(quo_list$formula)))
}

