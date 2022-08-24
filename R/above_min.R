#' Calculate area under the curve of a CI that is above a certain treatment effect
#'
#' @param TE treatment effect
#' @param LCL lower confidence interval limit
#' @param min_diff minimum important difference
#' @param only_outcome whether only the outcome should be calculated. FALSE returns an additional plot
#' @param lower_better whether lower values are better
#' @param RR whether Risk-Ratio is used
#'
#' @return
#' @export
#'
#' @examples
#' above_min(1.1, 0.1, 0.6, RR = FALSE)
above_min <-  function(TE, LCL, min_diff, only_outcome = FALSE, only_graphdata = FALSE, lower_better = TRUE, RR = TRUE){
  # if RR is used get log values
  if(RR){
    min_diff <- log(min_diff)
    TE <- log(TE)
    LCL <- log(LCL)
  }

  # create the Upper CI Limit
  diff_value <- TE - LCL
  UCL <- TE + diff_value


  SE <- metaHelper::SEp_from_CIp(LCL, UCL, t_dist = FALSE)

  x <- seq(qnorm(0.0001, TE, SE),
           qnorm(0.999, TE, SE),
           length.out = 2000)

  y <- dnorm(x, TE, SE)


  ci_u <- qnorm(0.975, TE, SE)
  ci_l <- qnorm(0.025, TE, SE)

  # get the percentage above the min_diff
  perc_orig <- pnorm(min_diff, TE, SE)

  if(lower_better){
    below_or_above <- "Below"
    percentages <- pnorm(x, TE, SE)
  } else {
    perc_orig <- 1 - perc_orig
    percentages <- 1- percentages
    below_or_above <- "Above"
  }
  perc <- round(100 * perc_orig, 1)


  # adjustment if we work with RR
  if(RR){
    effect_size <- "RR"
    x <- exp(x)
    ci_u <- exp(ci_u)
    ci_l <- exp(ci_l)
    min_diff <- exp(min_diff)
    LCL <- exp(LCL)
    UCL <- exp(UCL)
    TE <- exp(TE)
  } else{
    effect_size <- ""
  }

  # create plot annotation for effect size and CI
  data_annotation <- paste(
    "Effect: ", round(TE, 2), effect_size,
    "\nLower CI: ", round(LCL, 2), effect_size,
    "\nUpper CI: ", round(UCL, 2), effect_size
  )

  # create plot data frame
  dat <- data.frame(x = x, y = y)

  # if only the data for the graphs needs to be exported
  if(only_graphdata){
    return(list(perc_orig, x, y, ci_u, ci_l, min_diff, percentages))
  }

  # if only the percentage value needs to be exported
  if(only_outcome){
    return(perc_orig)
  } else{
    ###########################
    # Plotting logic
    dat %>%
      ggplot(aes(x = x, y = y)) +
      geom_line(alpha = 0.3, size = 1) +
      geom_area(data =dplyr::filter(dat, x >= LCL, x <= UCL), fill = "black", alpha = 0.2) -> p

    # change are direction if lower values are better
    if(lower_better){
      p <- p +
        geom_area(data =dplyr::filter(dat, x <= min_diff), fill = "#72b87f", alpha = 1)
    } else {
      p <- p +
        geom_area(data =dplyr::filter(dat, x >= min_diff), fill = "#72b87f", alpha = 1)
    }

    p +
      geom_vline(xintercept = c(ci_u, ci_l), color = "red", alpha = 0.5, linetype = 2) +
      geom_vline(xintercept = min_diff, color = "#1d8a3a", lwd = 1, alpha = 0.5) +
      geom_vline(xintercept = TE, color = "black", alpha = 0.2) +
      theme_light() +
      xlab(effect_size) +
      scale_x_continuous(breaks = c(min_diff, TE, ci_u, ci_l) %>% round(2)) +
      ylab("density") +
      # Percentage above threshold annotation
      ggtitle(paste(perc, "% ", below_or_above, " MCID Threshold", sep = "")) +
      # Data Annotation
      annotate("text", y = mean(y), x =  qnorm(0.0001, TE, SE), label = data_annotation,
               hjust = 0, vjust = -1, color = "black", size = 5.5) +
      # Min Difference Annotation
      annotate("text", y = mean(y), x =  qnorm(0.0001, TE, SE),
               label =   paste("\nMCID: ", min_diff %>% round(2), sep = ""),
               hjust = 0, vjust = 0, color = "#458251", size = 5.5) +
      theme(panel.grid = element_blank())
  }
}



#above_min(2, 0.8, 1.2, RR = TRUE, lower_better = FALSE)


# above_min(1.1, 0.1, 0.6, RR = FALSE)




