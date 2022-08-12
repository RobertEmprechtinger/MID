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
above_min <- function(TE, LCL, min_diff, only_outcome = FALSE, only_graphdata = FALSE, lower_better = TRUE, RR = TRUE){
  # get logs
  if(RR){
    min_diff <- log(min_diff)
    TE <- log(TE)
    LCL <- log(LCL)
  }

  log_dif <- TE - LCL
  UCL <- TE + log_dif


  SE <- SEp_from_CIp(LCL, UCL, t_dist = FALSE)


  pnorm(0, TE, SE)

  x <- seq(qnorm(0.0001, TE, SE),
           qnorm(0.999, TE, SE),
           length.out = 2000)

  y <- dnorm(x, TE, SE)


  ci_u <- qnorm(0.975, TE, SE)
  ci_l <- qnorm(0.025, TE, SE)

  if(lower_better){
    perc_orig <- pnorm(min_diff, TE, SE)
    below_or_above <- "Below"
  } else {
    perc_orig <- 1 - pnorm(min_diff, TE, SE)
    below_or_above <- "Above"
  }
  perc <- round(100 * perc_orig, 1)

  if(RR){
    data_annotation <- paste(
      "Effect: ", round(exp(TE), 2), " RR",
      "\nLower CI: ", round(exp(LCL), 2), " RR",
      "\nUpper CI: ", round(exp(UCL), 2), " RR",

      sep = ""
    )

    dat <- data.frame(x = exp(x), y = y)

    # if only the data for the graphs needs to be exported
    if(only_graphdata) return(list(perc_orig, x, y, ci_u, ci_l,
                                   min_diff, exp(min_diff)))

    # if only the percentage value needs to be exported
    if(only_outcome){
      return(perc_orig)
    } else{
    # export the plot
      dat %>%
        ggplot(aes(x = x, y = y)) +
        geom_line(alpha = 0.3, size = 1) +
        geom_area(data =dplyr::filter(dat, x >= exp(LCL), x <= exp(UCL)), fill = "black", alpha = 0.2) -> p

      # change are direction if lower values are better
      if(lower_better){
        p <- p +
          geom_area(data =dplyr::filter(dat, x <= exp(min_diff)), fill = "#72b87f", alpha = 1)
      } else {
        p <- p +
          geom_area(data =dplyr::filter(dat, x >= exp(min_diff)), fill = "#72b87f", alpha = 1)
      }

      p +
        geom_vline(xintercept = c(exp(ci_u), exp(ci_l)), color = "red", alpha = 0.5, linetype = 2) +
        geom_vline(xintercept = exp(min_diff), color = "#1d8a3a", lwd = 1, alpha = 0.5) +
        geom_vline(xintercept = exp(TE), color = "black", alpha = 0.2) +
        theme_light() +
        xlab("Risk Ratio") +
        scale_x_continuous(trans='log2',
                           breaks = c(exp(min_diff), exp(TE), exp(ci_u), exp(ci_l)) %>% round(2)) +
        ylab("density") +
        # Percentage above threshold annotation
        annotate("text", y = max(y) * 1.1, x = exp(TE), label = paste(perc, "% ", below_or_above, " MCID Threshold", sep = ""),
                 color = "black", size = 8) +
        # Data Annotation
        annotate("text", y = mean(y), x =  exp(qnorm(0.0001, TE, SE)), label =data_annotation,
                 hjust = 0, vjust = -1, color = "black", size = 5.5) +
        # Min Difference Annotation
        annotate("text", y = mean(y), x =  exp(qnorm(0.0001, TE, SE)),
                 label =   paste("\nMCID: ", exp(min_diff) %>% round(2), " RR", sep = ""),
                 hjust = 0, vjust = 0, color = "#458251", size = 5.5) +
        theme(panel.grid = element_blank())
    }
  } else{
    data_annotation <- paste(
      "Effect: ", round(TE, 2),
      "\nLower CI: ", round(LCL, 2),
      "\nUpper CI: ", round(UCL, 2),
      sep = ""
    )

    dat <- data.frame(x = x, y = y)

    # if only the data for the graphs needs to be exported
    if(only_graphdata) return(list(perc_orig, x, y, ci_u, ci_l,
                                   exp(min_diff)))

    if(only_outcome){
      return(perc_orig)
    } else{
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
        xlab("Risk Ratio") +
        scale_x_continuous(breaks = c(min_diff, TE, ci_u, ci_l) %>% round(2)) +
        ylab("density") +
        # Percentage above threshold annotation
        annotate("text", y = max(y) * 1.1, x = TE, label = paste(perc, "% ", below_or_above, " MCID Threshold", sep = ""),
                 color = "black", size = 8) +
        # Data Annotation
        annotate("text", y = mean(y), x =  qnorm(0.0001, TE, SE), label =data_annotation,
                 hjust = 0, vjust = -1, color = "black", size = 5.5) +
        # Min Difference Annotation
        annotate("text", y = mean(y), x =  qnorm(0.0001, TE, SE),
                 label =   paste("\nMCID: ", min_diff %>% round(2), sep = ""),
                 hjust = 0, vjust = 0, color = "#458251", size = 5.5) +
        theme(panel.grid = element_blank())
    }

  }
}

# above_min(1.1, 0.1, 0.6, RR = FALSE)


#TE <- 0.5
#LCL <- 0.4
#min_diff = 0.45
#only_outcome = FALSE
#only_graphdata = FALSE
#lower_better = TRUE
#RR = TRUE


