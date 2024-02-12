#' An ANOVA summary table with total DF & SS
#'
#' This function gives an ANOVA summary table with total degrees of freedom and sum of squares
#' @param formula formula used in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @return a html table
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable kable_styling
#' @importFrom dplyr group_by summarise %>% n
#' @export
#' @examples
#' dox_aov(LogStrength ~ Brand + Water, Towels2)
dox_aov=function(formula, dataset){
  formula=as.formula(formula)
  anova_model=aov(formula, dataset)
  if(length(summary(anova_model)) > 1){
    stop("This function only works for ANOVA with one summary table. Designs like split-plot do not work.")
  }

  # give warnings if the experiment is not balanced
  counts_table <- dataset %>%
    group_by(across(all.vars(formula)[-1])) %>%
    summarise(n = n())

  if(!all(counts_table$n[1] == counts_table$n)){
    warning("Your experiment is not balanced and the result can be misleading. The aov() function used here conducts Type I ANOVA, which only works for balanced design. We recommend using Anova() in the 'car' package to conduct Type II/III ANOVA.")
    print(counts_table)
  }


  table <- summary(anova_model)[[1]]
  lastest_rownames = c(rownames(table),"Total")


  # Add extra row for sum of DF and SS
  extra_row <- c(sum(table$Df), sum(table[["Sum Sq"]]),NA,NA,NA)
  anova_results <- rbind(table, extra_row)
  rownames(anova_results) <- lastest_rownames
  # options(knitr.kable.NA = '')
  # Create ANOVA summary table with kable
  # knitr::opts_chunk$set(
  #   out.width = "50%",
  #   out.height = "400px"
  # )
  kable(format(anova_results, digits = 4), align = 'r',
        caption = "ANOVA Summary", escape = F, format.args = list(big.mark = ","))  %>% kable_styling()

  # output_list <- list(resid = anova_model$residuals)
  #
  # # Return the list
  # return(output_list)
}



#' Confidence intervals of pairwise comparisons
#'
#' This function plots the confidence intervals of pairwise comparisons using Fisher least significant difference (LSD),
#' Bonferroni significant difference (BSD), and Tukey honest significant difference (HSD) methods.
#' @param dataset dataset of experimental results
#' @param formula target~treatment
#' @param alpha alpha level (default 0.05)
#' @param method LSD, BSD, or HSD (default is ALL)
#' @return confidence interval plots
#' @importFrom dplyr %>% pull filter bind_rows
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang enquo quo_name parse_expr eval_tidy
#' @import ggplot2
#' @export
#' @examples
#' dox_pairs(LogStrength~Water, Towels2)
#' # If you want to adjust the alpha level
#' dox_pairs(LogStrength~Water, Towels2, alpha = 0.01)
#' # If you are only interested in LSD
#' dox_pairs(LogStrength~Water, Towels2, method = "LSD")
dox_pairs <- function(formula,dataset, alpha = 0.05, method = "All") {
  formula=as.formula(formula)
  # Get the string version
  target_str = all.vars(formula)[1]
  treatment_str = all.vars(formula)[2]

  alpha_str = deparse(substitute(alpha))
  legend_str = paste("p-value < ", alpha_str)

  # Compute ANOVA to obtain MSE
  # response <- parse_expr(quo_name(enquo(target)))
  # x <- parse_expr(quo_name(enquo(treatment)))
  # anova_res=eval_tidy(expr(aov(!!response ~ !!x, data = dataset)))

  # str aov
  formula_str <- paste(target_str, "~", treatment_str)
  formula_obj <- as.formula(formula_str)
  anova_res <- aov(formula_obj, data = dataset)

  mse <- summary(anova_res)[[1]]["Mean Sq"][[1]][2]

  # Get the levels of the treatment variable
  treatment_levels <- unique(dataset[[treatment_str]])
  treatment_levels=as.character(treatment_levels)


  # Obtain all pairs
  pairs <- as.data.frame(t(combn(treatment_levels, 2)))
  colnames(pairs) = c("treatment1","treatment2")

  results <- list()
  for (i in 1:nrow(pairs)) {
    treatment1 <- pairs[i, "treatment1"]
    treatment2 <- pairs[i, "treatment2"]

    # Extract two treatment groups
    data1 <- dataset[dataset[[treatment_str]] == treatment1,target_str]
    data2 <- dataset[dataset[[treatment_str]] == treatment2,target_str]

    # Compute the sample sizes and means for the two treatment groups
    n1 <- length(data1)
    n2 <- length(data2)
    mean1 <- mean(data1)
    mean2 <- mean(data2)

    # Compute the margin of error and confidence interval
    # LSD
    LSD_me <- qt(alpha/2, nrow(dataset) - length(treatment_levels), lower.tail = FALSE) * sqrt(mse * (1/n1 + 1/n2))
    LSD_ci <- c(mean1 - mean2 - LSD_me, mean1 - mean2 + LSD_me)

    # BSD
    BSD_me <- qt(alpha/2/nrow(pairs), nrow(dataset) - length(treatment_levels), lower.tail = FALSE) * sqrt(mse * (1/n1 + 1/n2))
    BSD_ci <- c(mean1 - mean2 - BSD_me, mean1 - mean2 + BSD_me)

    # Tukey HSD
    HSD_me <- qtukey(alpha,length(treatment_levels),nrow(dataset)-length(treatment_levels), lower.tail = FALSE)*sqrt(mse*(1/n1+1/n2))/sqrt(2)
    HSD_ci <- c(mean1 - mean2 - HSD_me, mean1 - mean2 + HSD_me)

    # Store the results
    results[[i]] <- data.frame(
      treatment1 = treatment1,
      treatment2 = treatment2,
      diff = mean1 - mean2,
      LSD_ci_low = LSD_ci[1],
      LSD_ci_high = LSD_ci[2],
      BSD_ci_low = BSD_ci[1],
      BSD_ci_high = BSD_ci[2],
      HSD_ci_low = HSD_ci[1],
      HSD_ci_high = HSD_ci[2]
    )
  }

  # Combine the results into a data frame
  results <- bind_rows(results)
  x_min = min(min(results$LSD_ci_low),min(results$BSD_ci_low),min(results$HSD_ci_low))
  x_max = max(max(results$LSD_ci_high),max(results$BSD_ci_high),max(results$HSD_ci_low))

  # Add an indicator for statistical significance
  results$LSD_no_zero <- ifelse(results$LSD_ci_low > 0 | results$LSD_ci_high < 0, "yes", "no")
  results$BSD_no_zero <- ifelse(results$BSD_ci_low > 0 | results$BSD_ci_high < 0, "yes", "no")
  results$HSD_no_zero <- ifelse(results$HSD_ci_low > 0 | results$HSD_ci_high < 0, "yes", "no")

  # Plot the confidence intervals
  LSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = LSD_no_zero)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = LSD_ci_low, xmax = LSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "LSD", color = legend_str) +
    xlim(x_min, x_max)


  BSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = BSD_no_zero)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = BSD_ci_low, xmax = BSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "BSD", color = legend_str) +
    xlim(x_min, x_max)

  HSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = HSD_no_zero)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = HSD_ci_low, xmax = HSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "Tukey HSD", color = legend_str) +
    xlim(x_min, x_max)

  results_display = results %>%
    mutate_if(is.numeric, round, digits = 2)

  results_LSD=results_display[,c("treatment1","treatment2","diff","LSD_ci_low","LSD_ci_high","LSD_no_zero")]
  results_BSD=results_display[,c("treatment1","treatment2","diff","BSD_ci_low","BSD_ci_high","BSD_no_zero")]
  results_HSD=results_display[,c("treatment1","treatment2","diff","HSD_ci_low","HSD_ci_high","HSD_no_zero")]

  if (method == "LSD")
  {
    print(results_LSD)
    LSD_plot
  }
  else if (method == "BSD")
  {
    print(results_BSD)
    BSD_plot
  }
  else if (method == "HSD"){
    print(results_HSD)
    HSD_plot
  }
  else{
    grid.arrange(LSD_plot, BSD_plot, HSD_plot, ncol=1)
    print(results_LSD)
    print(results_BSD)
    print(results_HSD)
  }
}

#' An ANOVA table
#'
#' This function gives an ANOVA summary table with total degrees of freedom and sum of squares
#' @param formula formula used in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @return a print table
#' @export
#' @examples
#' dox_anova(LogStrength ~ Brand, Towels2)

dox_anova=function(formula, dataset){
  anova <- aov(formula, dataset)
  # Summary of the ANOVA
  summary_anova <- summary(anova)

  # Extract the main table from the summary
  anova_table <- summary_anova[[1]]

  # Calculate sums for relevant columns
  total_df <- sum(anova_table$Df)
  total_sum_sq <- sum(anova_table$"Sum Sq")

  # You might not have a meaningful sum for Mean Sq, F value, and Pr(>F) for the total row
  # But you can calculate the total Mean Sq if needed
  # total_mean_sq <- total_sum_sq / total_df

  # Create a new row for totals
  total_row <- c(Df = total_df, `Sum Sq` = total_sum_sq, `Mean Sq` = NA, `F value` = NA, `Pr(>F)` = NA)

  # Append this row to the original ANOVA table
  anova_table_with_total <- rbind(anova_table, Total = total_row)

  # Print the modified table
  print(anova_table_with_total)
}
