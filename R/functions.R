#' Function to make some descriptive summary stats
#'
#' @param data Lipidomics dataset.
#'
#' @return A data frame/tibble.
#'
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(
      value,
      list(mean = mean, sd = sd)
    )) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      ~ round(.x, digits = 1)
    ))
}

#' Function ot plot distribution of metabolites
#'
#' @param data A tibble/ data frame.
#'
#' @return A plot object.
#'
plot_distributions <- function(data) {
  metabolite_distribution_plot <- ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
  metabolite_distribution_plot
}

#' Convert column value strings into snakecase
#'
#' @param data The data with string columns.
#' @param cols The columns that needs to be converted into snakecase.
#'
#' @return A data frame.
#'
column_values_to_snake_case <- function(data, cols) {
  data %>%
    dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}

#' Converting data from in long format to wide format.
#'
#' @param data A data frame that needs to be in wide format.
#'
#' @return A data frame in the wide format.
#'
metabolites_to_wider <- function(data) {
  data %>% tidyr::pivot_wider(
    names_from = metabolite,
    values_from = value,
    values_fn = mean,
    names_prefix = "metabolite_"
  )
}


#' A transformation recipe to pre-proces the data.
#'
#' @param data A data frame with the predictor, outcome and covariates.
#' @param metabolite_variable The specific predictor in the given model.
#'
#' @return A recipe with the parameters from the model.
#'
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }}, age, gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}
