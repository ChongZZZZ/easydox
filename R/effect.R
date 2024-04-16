#' Main Effect Plots
#'
#' This function gives the main effect plots for one or more treatments in an experiment.
#' If more than 1 treatment is given and ylim is not specified, plots will share the same y-axis range.
#' @param formula y~x1+x2(optional)+x3(optional)
#' @param dataset the dataset that contains the experiment information
#' @param label Label "Mean" or "Effect"
#' @param text_size text size of the x-axis
#' @param ylim a vector of the range of the y-axis
#' @return main effect plots
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point coord_cartesian element_text element_blank
#' @importFrom dplyr group_by summarise %>%
#' @importFrom gridExtra grid.arrange
#' @export
#' @examples
#' dox_main(LogStrength ~ Brand + Water, Towels2, label="Mean")
#' # If you want the label to be effect and have a larger size for the x-axis
#' dox_main(LogStrength ~ Brand + Water, Towels2, label="Effect", text_size = 14)
dox_main <- function(formula, dataset, label = "Mean", text_size = 12, ylim = NULL) {
  formula <- as.formula(formula)
  response <- all.vars(formula)[1]
  vars <- all.vars(formula)[-1]
  means <- list()
  plots <- list()

  u <- mean(dataset[[response]], na.rm = TRUE)

  # Initialize y_min and y_max with very large or small values to ensure they are set by the data
  y_min <- Inf
  y_max <- -Inf

  # Check and prepare each variable
  for (i in 1:length(vars)) {
    var <- vars[i]
    if (!is.na(var)) {
      if (is.numeric(dataset[[var]])) {
        stop(paste("Variable", var, "needs to be a factor. Currently numeric."))
      }

      # Calculate means and update global min/max
      # Correct way to handle spaces in variable names
      grouped_data <- dataset %>%
        dplyr::group_by(.data[[var]]) %>%
        dplyr::summarise(Mean_Response = mean(.data[[response]], na.rm = TRUE), .groups = 'drop')

      # Update global y_min and y_max
      y_min <- min(y_min, min(grouped_data$Mean_Response, na.rm = TRUE))
      y_max <- max(y_max, max(grouped_data$Mean_Response, na.rm = TRUE))

      means[[var]] <- grouped_data

      # Prepare plots with adjusted axis ranges
      extended_y_min <- y_min - (y_max - y_min) * 0.1  # Extend by 10%
      extended_y_max <- y_max + (y_max - y_min) * 0.1  # Extend by 10%

      plots[[var]] <- ggplot(grouped_data, aes_string(x = var, y = "Mean_Response")) +
        geom_line(aes(group = 1)) +
        geom_point() +
        theme(axis.title = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(size = text_size)) +
        coord_cartesian(ylim = if (is.null(ylim)) c(extended_y_min, extended_y_max) else ylim) +
        geom_text(aes(label = ifelse((abs(Mean_Response - ifelse(label == "Mean", 0, u)) > 1e4) |
                                       (abs(Mean_Response - ifelse(label == "Mean", 0, u)) < 0.01),
                                     sprintf('%.3e', Mean_Response - ifelse(label == "Mean", 0, u)),
                                     round(Mean_Response - ifelse(label == "Mean", 0, u), 4))),
                  vjust = -1)
    }
  }

  # Layout multiple plots if applicable
  plot_layout <- do.call(gridExtra::grid.arrange, c(plots, nrow = 1))
  return(plot_layout)
}





#' Interaction Effect Plot
#'
#' This function gives the interaction effect plots for two treatments in an experiment.
#' @param formula y~x1+x2
#' @param dataset the dataset that contains the experiment information
#' @param label Label "Mean" or "Effect"
#' @param text_size text size of the x-axis
#' @return interaction effect plot
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point element_text labs facet_grid vars
#' @importFrom dplyr group_by summarise %>%
#' @export
#' @examples
#' dox_inter(LogStrength ~ Brand + Water, Towels2, label="Mean", text_size = 14)
#' # If you want the label to be effect and have a larger size for the x-axis
#' dox_inter(LogStrength ~ Brand + Water, Towels2, label="Effect", text_size = 14)
dox_inter = function(formula, dataset, label="Mean", text_size = 12){
  formula=as.formula(formula)
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  x2 = all.vars(formula)[3]

  if(is.numeric(dataset[[x1]])){
    error_message = paste("Variable \"", x1, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if(is.numeric(dataset[[x2]])){
    error_message = paste("Variable \"", x2, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }


  df <- dataset %>%
    group_by(.data[[x1]], .data[[x2]]) %>%
    summarise(Mean_Response = mean(.data[[response]]),.groups = 'drop')

  y_min = min(df$Mean_Response)
  y_max = max(df$Mean_Response)
  if (label=="Mean" | label =="Effect"){
    y_max=y_max+(y_max-y_min)*0.15
  }


  # calculate means
  x1_mean <- dataset %>%
    group_by(.data[[x1]]) %>%
    summarise(x1_mean = mean(.data[[response]]))

  x2_mean <- dataset %>%
    group_by(.data[[x2]]) %>%
    summarise(x2_mean = mean(.data[[response]]))

  inter_mean <- dataset %>%
    group_by(.data[[x1]], .data[[x2]]) %>%
    summarise(inter_mean = mean(.data[[response]]))

  u <- mean(dataset[[response]])

  # merge table
  inter_mean = merge(inter_mean, x1_mean, by = x1, all.x = TRUE)
  inter_mean = merge(inter_mean, x2_mean, by = x2, all.x = TRUE)
  inter_mean$effect = inter_mean$inter_mean-inter_mean$x1_mean-inter_mean$x2_mean+u
  interaction_effect = inter_mean$effect

  p1 = ggplot(df, aes(.data[[x1]], Mean_Response, color = .data[[x2]])) +
    theme(axis.title=element_text(size=14,face="bold"),
          axis.text.x = element_text(size = text_size))+
    geom_line(aes(group = .data[[x2]])) +
    geom_point() +
    coord_cartesian(ylim = c(y_min, y_max))


  if(label=="Mean"){
    p1 <- p1+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.3e', Mean_Response), round(Mean_Response,4))),vjust = -1)
  }

  else if(label=="Effect"){
    p1 <- p1+geom_text(aes(label=ifelse(((abs(interaction_effect) > 1e4) | (abs(interaction_effect) < 0.01)), sprintf('%.3e', interaction_effect), round(interaction_effect,4))),vjust = -1)
  }
  p1
}
