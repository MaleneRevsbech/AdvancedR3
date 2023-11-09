#' Function to make some descriptive summary stats
#'
#' @param data Lipidomics dataset.
#'
#' @return A data frame/tibble.
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(dplyr::across(value,
                                       list(mean = mean, sd = sd))) %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric),
                                    ~ round(.x, digits = 1)))
}

#' Function ot plot distribution of metabolites
#'
#' @param data A tibble/ data frame.
#'
#' @return A plot object.
plot_distributions <- function(data){
    metabolite_distribution_plot <- ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
    metabolite_distribution_plot
}

