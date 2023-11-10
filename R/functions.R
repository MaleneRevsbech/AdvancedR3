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


#' Add model and recipe specs to workflow
#'
#' @param model_specs The specific model parameters
#' @param recipe_specs The recipe parameters
#'
#' @return A workflow with the gioven model parameters and recipes.
#'
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The workflow of the fitted model including recipe and model
#'
#' @return a Data frame.
#'
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}


#' Convert the long data form data set into a list of wide form data frames.
#'
#' @param data The Lipidomics data set in its original form.
#'
#' @return Returns a list of data frames with each metabolite.
#'
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>% # denne her splitter alle metabolitterne i individuelle tibbles!
    purrr::map(metabolites_to_wider)
}


#' Generate the results of the model
#'
#' @param data The lipidomics dataset
#'
#' @return A data frame.
#'
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() %>%
      parsnip::set_engine("glm"),
    data %>%
      create_recipe_spec(
        tidyselect::starts_with("metabolite_")
      )
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}


#' A function to add good value names to the model estimates
#'
#' @param model_results Results from the logistic models
#' @param data The original Lipidomics data set
#'
#' @return A combiend data frame with model results AND good column names
#'
add_original_metabolite_names <- function(model_results, data) {
  data %>%
    dplyr::select(metabolite) %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snake_case(term) %>%
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
    dplyr::distinct(term, metabolite) %>%
    dplyr::right_join(model_results, by = "term")
}
