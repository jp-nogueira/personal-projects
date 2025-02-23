
#### Helper functions for the main analysis ####

#***************************************************************************************************
# Load packages ####
#***************************************************************************************************

installed <- require(tidyverse)
if (!installed) install.packages("tidyverse")
installed <- require(data.table)
if (!installed) install.packages("data.table")
installed <- require(gridExtra)
if (!installed) install.packages("gridExtra")

# regressions
installed <- require(lfe)
if (!installed) install.packages("lfe")

# rd packages
installed <- require(rdd)
if (!installed) install.packages("rdd")
installed <- require(rdrobust)
if (!installed) install.packages("rdrobust")
installed <- require(jtools)
if (!installed) install.packages("jtools")

# ipums package
installed <- require(ipumsr)
if (!installed) install.packages("ipumsr")


#***************************************************************************************************
# Helper functions ####
#***************************************************************************************************

is_true <- function(x) {
  !is.na(x) & x
}


#' Wraps the format function into a more easily manageble function, by setting a default format for integers, 0 decimal points, and non-integers, 3 decimal points.
#'
#' Note that the argument \code{digits} in the function \code{format} indicates the number of significant digits to include.  In \code{format2}, the argument \code{digits} refers to the number of places after the decimal to include (ie., for \code{digits=3}, \code{format(0.0044321) = 0.00443} whereas \code{format2(0.0044321) = 0.004}).
#'
#' @param x: numeric object to format
#' @param trunc_large: TRUE/FALSE, should large numbers be treated as integers?  Default is to round to 3 digits unless the number is an integer
#' @param ... other arguments to pass to format
#' @seealso \code{\link{format}}
#' @export
format2 <- function(x, trunc_large = FALSE, ...) {
  args <- list(...)
  
  if ( !class(x) %in% c("numeric", "integer", "double") ) {
    output = x
  } else {
    if ( !"scientific" %in% names(args) ) args <- append(args, list(scientific = FALSE))
    if ( !"big.mark" %in% names(args) ) args <- append(args, list(big.mark = ","))
    if ( !"justify" %in% names(args) ) args <- append(args, list(justify = "right"))
    
    if ( "digits" %in% names(args) ) d <- args[["digits"]]
    
    if ( is_true(all(as.integer(x) %in% x)) | (trunc_large & any(x > 1000)) ) { # integer/large numbers
      if ( !"digits" %in% names(args) ) {
        args <- append(args, list(nsmall = 0, x = round(x, 0)))
      } else {
        args <- append(args, list(x = round(x, d)))
      }
      
      output = do.call(format, args)
    } else { # non-integer numbers
      if ( !"digits" %in% names(args) ) {
        args <- append(args, list(digits = 3, nsmall = 3, x = round(x, 3)))
      } else {
        args <- append(args, list(x = round(x, d)))
      }
      
      output = do.call(format, args)
    }
  }
  
  return(str_replace_all(output, c("^-" = "$-$", "NA" = "")))
}

#***************************************************************************************************
# RD functions ####
#***************************************************************************************************


#' Function to generate RD plots of the raw or residualized outcomes
#' 
#' @param dt: data table, containing the data to plot
#' @param dep_var: string, dependent variable to plot (y-axis)
#' @param dep_var_lab: string, label for the dependent variable
#' @param clust_var: string, variable to cluster standard errors for confidence intervals
#' @param run_var: string, running variable (x-axis).  Default is "elig_voters"
#' @param spec: string, specification to residualize the dependent variable.  Default is NA, no residualizing
#' @param binwidth: integer, size of the bins.  Default is 7500
#' @param cut: integer, value of the threshold for the running variable.  Default is 200k
#' @param run_low: integer, min value of the running variable to plot.  Default is 150k
#' @param run_high: integer, max value of the running variable to plot.  Default is 250k
#' @param file: string, file name to save the plot to
#' @param h: integer, height of the plot in inches
#' @param w: integer, width of the plot in inches
#' @import ggplot2
#' @import jdtools
#' @import lfe
#' @export
plotRD <- function(
  dt, dep_var, dep_var_lab = "", clust_var, run_var = "elig_voters", spec = NA,
  binwidth = 7500, cut = 200000, run_low = 150000, run_high = 250000, 
  file = NA, w = 16, h = 9) {
  
  # get a list of the variables needed to run the specification, if provided
  spec_var <- switch(
    is.na(spec)+1,
    unique(setdiff(
      str_extract_all(spec, "[\\w_]+")[[1]], 
      c("0", "factor")
    )),
    c()
  )
  
  # subset the data to run between run_low and run_high
  dt1 <- dt[
    get(run_var) >= run_low & get(run_var) <= run_high, 
    copy(.SD), 
    .SDcols = unique(c(dep_var, run_var, clust_var, spec_var))
  ]
  dt1 <- dt1[complete.cases(dt1)]
  
  # calculate residualized dependent variable
  if ( !is.na(spec) ) {
    reg <- felm(
      as.formula(paste0(
        dep_var, " ~ ", 
        str_replace_all(
          spec, 
          "\\b(tworound:cut_dist|tworound\\*cut_dist|tworound|cut_dist)( \\+ |(?=\\|))", 
          ""
        )
      )), 
      data = dt1
    )
    dt1[, resid := reg$residuals]
  }
  
  # rename variables
  setnames(
    dt1, 
    c(switch(is.na(spec)+1, "resid", dep_var), run_var, clust_var), 
    c("depvar", "runvar", "clustvar")
  )
  
  # create bins and calculate mean outcome (either raw or residualized dep_var) by bin
  getBin <- function(x, values) {
    dist = sapply(x, function(z) min(which(abs(z - values) == min(abs(z - values)))))
    return(values[dist])
  }
  
  int.low = seq(run_low+binwidth*0.1, cut-binwidth*.5, by = binwidth)
  dt1[runvar < cut, bin := getBin(runvar, int.low)]
  
  int.high = seq(cut+binwidth*0.5, run_high-binwidth*0.1, by = binwidth)
  dt1[runvar >= cut, bin := getBin(runvar, int.high)]
  
  bybin <- dt1[, list(
    count = .N, outcome = mean(depvar)
  ), by = bin]
  
  # calculate cluster standard errors
  fit1 <- lm(depvar ~ runvar, data = dt1[runvar >= cut])
  t1 <- make_predictions(
    fit1, "runvar", interval = TRUE, robust = "HC0", cluster = "clustvar", 
    pred.values = seq(cut, run_high, length.out = 20)
  )
  
  fit2 <- lm(depvar ~ runvar, data = dt1[runvar < cut])
  t2 <- make_predictions(
    fit2, "runvar", interval = TRUE, robust = "HC0", cluster = "clustvar",
    pred.values = seq(run_low, cut, length.out = 20)
  )
  
  # generate the RD plot
  plot <- ggplot(data = bybin) +
    theme_bw(base_size = 11) +
    theme(plot.caption = element_text(hjust = 0.5, size = rel(1.3))) +
    xlab("Number of registered voters") +
    ylab(dep_var_lab) +
    scale_size_continuous(guide = "none") +
    scale_x_continuous(labels = scales::comma) +
    geom_point(aes(x = bin, y = outcome, size = count)) +
    geom_vline(aes(xintercept = cut), linetype = 2) +
    # right of the discontinuity
    geom_line(data = t1, aes(x = runvar, y = depvar), color = maroon, size = 1) +
    geom_line(data = t1, aes(x = runvar, y = ymin), linetype = 2) +
    geom_line(data = t1, aes(x = runvar, y = ymax), linetype = 2) +
    geom_ribbon(data = t1, aes(runvar, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.2) +
    # left of the discontinuity
    geom_line(data = t2, aes(x = runvar, y = depvar), color = maroon, size = 1) +
    geom_line(data = t2, aes(x = runvar, y = ymin), linetype = 2) +
    geom_line(data = t2, aes(x = runvar, y = ymax), linetype = 2) +
    geom_ribbon(data = t2, aes(runvar, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.2)
  
  
  if ( !is.na(file) ) ggsave(file = file, width = w, height = h, units = "in", plot = plot)
  
  plot
  return(plot)
}


#' Function to estimate the regression discontinuity and generate latex tables of the results.
#' 
#' @param dtl: list of data.tables, for estimating each regression
#' @param lhs: character vector, names of the outcome variables
#' @param spec: string, the RHS of the regression specification.  Default is no controls with standard errors clustered at municipality level
#' @param cut: string, name of the running variable.  Default is "cut_dist"
#' @param keep: character vector, names of variables to keep
#' @param covariate.labels: character vector, labels of the kept variables
#' @param ik: TRUE/FALSE, should ik bandwidth be estimated?  Default is false
#' @param mserd: TRUE/FALSE, should mserd bandwidth be estimated?  Default is false
#' @param bandwidths: integer vector, bandwidths to be stimated
#' @param disp_obs: TRUE/FALSE, should # obs be displayed in table?  Default is true
#' @param obs_text: string, label for # obs in table.  Default is "Observations"
#' @param disp_muni: TRUE/FALSE, should # of municipalities be displayed in table?  Default is true
#' @param muni_text: string, label for # muni in table.  Default is "Municipalities"
#' @param disp_mean: TRUE/FALSE, should single-round mean be displayed in table?  Default is true
#' @param mean_text: string, label for single-round mean in table.  Default is "Single-round mean"
#' @param disp_bw: TRUE/FALSE, should bandwidth be displayed in table?  Default is false
#' @param bw_text: string, label for bandwidth used in table.  Default is "Bandwidth size"
#' @param stars: TRUE/FALSE, should significant stars be displayed in table?  Default is false
#' @param lines: list of character vectors, additional lines to add to table
#' @import lfe
#' @export

estRD <- function(
  dtl, lhs, spec = "tworound + cut_dist + tworound*cut_dist|0|0|tse_code",
  keep = "tworound", covariate.labels = "TwoRound",
  cut = "cut_dist", ik = FALSE, mserd = FALSE, bandwidths,
  disp_obs = TRUE, obs_text = "Observations",
  disp_muni = TRUE, muni_text = "Municipalities",
  disp_mean = TRUE, mean_text = "Single-round mean",
  disp_bw = FALSE, bw_text = "Bandwidth size", 
  stars = FALSE, lines = list(), ...) {

  # repeat dtl, spec, cut if the lengths do not match the number of lhs variables
  dtl <- rep_len(dtl, length(lhs))
  spec <- rep_len(spec, length(lhs))
  cut <- rep_len(cut, length(lhs))
  
  # create variable for the output list, which will contain all the regression outputs and tables
  output = list()
  
  # calculate IK and MSERD bandwidths for each lhs variable
  # - IK and MSERD bandwidths are calculated using observations with less than 400k voters,
  #   otherwise the calculated bandwidths are sometimes greater than the support 
  # - "bw" is appended to output list, and contains the ik, mserd, and user-input bandwidths
  bws <- lapply(
    set_names(1:length(lhs), paste0(lhs, "_bws")), 
    function(x) {
      # get the cluster variable
      clust_var = str_split(spec[[x]], "\\|")[[1]][4]
      
      # calculate IK bandwidth
      bw_ik = switch(
        ik+1,
        NULL,
        rdbwselect_2014(
          dtl[[x]][[lhs[[x]]]], dtl[[x]][[cut[[x]]]], bwselect = "IK",
          subset = dtl[[x]][, elig_voters <= 400000])$bws[1]
      )
      
      # calculate MSERD bandwidth
      bw_mserd = switch(
        mserd+1,
        NULL,
        rdbwselect(
          dtl[[x]][[lhs[[x]]]], dtl[[x]][[cut[[x]]]], 
          cluster = dtl[[x]][[str_split(spec, "\\|")[[1]][4]]],
          subset = dtl[[x]][, elig_voters <= 400000])$bws[3]
      )
      
      return(
        c(set_names(bandwidths, paste0("bw_", 1:length(bandwidths))),
          bw_ik = bw_ik,
          bw_mserd = bw_mserd
        ))
    })
  output = append(output, bws)
  
  # create variables for table summary statistics
  obs = list()
  muni = list()
  ymean = list()

  # estimate the RD regression for each lhs variable
  for ( count in 1:length(lhs) ) {
    dt = dtl[[count]]
    lhsvar = lhs[[count]]
    bwlist = bws[[paste0(lhsvar, "_bws")]]
    specification = spec[[count]]
    cutvar = cut[[count]]
    
    # estimate the regression, looping through all bandwidths
    formula = paste0(lhsvar, " ~ ", specification)
    reg_ls <- lapply(
      setNames(bwlist, paste0(lhsvar, "_", names(bwlist))),
      function(x, f) {
        return(felm(as.formula(f), data = dt[get(paste0(cutvar, "_abs")) <= x]))
      }, f = formula)
    output = append(output, reg_ls)
    
    # calculate single-round dependent variable mean
    ymean[lhsvar] <- list(
      sapply(
        unname(bwlist),
        function(x) dt[tworound == FALSE][cut_dist_abs <= x, mean(get(lhsvar), na.rm = TRUE)],
        USE.NAMES = FALSE
      ))
    
    # get number of observations used to estimate each regression
    obs[lhsvar] <- list(
      sapply(
        unname(reg_ls),
        function(x) x$N,
        USE.NAMES = FALSE
      ))
    
    # get number of municipalities in each regression
    muni[lhsvar] <- list(
      sapply(
        unname(reg_ls),
        function(x) uniqueN(x$clustervar[[1]]),
        USE.NAMES = FALSE
      )
    )
  }

  # generate regression tables for each bandwidth
  idx = 0
  for ( count in c(1:length(bandwidths), switch(ik+1, NULL, "ik"), switch(mserd+1, NULL, "mserd")) ) {
    idx <- idx + 1
    
    # get the mean, obs, and bandwidth values
    ymn_tab <- sapply(ymean, function(x) x[idx])
    mun_tab <- sapply(muni, function(x) x[idx])
    obs_tab <- sapply(obs, function(x) x[idx])
    bws_tab <- sapply(bws, function(x) x[idx])
    
    # format mean, obs, bandwidth values, and other lines
    lines2 = lines
    if ( disp_mean )
      lines2 = append(lines2, list(c(mean_text, format2(ymn_tab))))
    if ( disp_bw ) 
      lines2 = append(lines2, list(c(bw_text, format2(bws_tab, trunc_large = TRUE))))
    if ( disp_obs ) 
      lines2 = append(lines2, list(c(obs_text, format2(obs_tab, trunc_large = TRUE))))
    if ( disp_muni )
      lines2 = append(lines2, list(c(muni_text, format2(mun_tab, trunc_large = TRUE))))
    
    # format these lines into a table latex format
    if ( length(lines2) > 0 ) lines2[[1]][1] <- paste0("\\\\[-1.8ex] ", lines2[[1]][1])
    
    lines2 <- sapply(lines2, function(x) paste0(paste(x, collapse = " & "), "\\\\\n"))
    
    # format the coefficients and standard errors
    se <- matrix(sapply(
      output[str_subset(names(output), paste0("bw_", count, "$"))],
      function(x) format2(x$cse[keep])
    ), nrow = length(keep))
    pval <- matrix(sapply(
      output[str_subset(names(output), paste0("bw_", count, "$"))],
      function(x) x$cpval[keep]
    ), nrow = length(keep))
    coeff <- matrix(sapply(
      output[str_subset(names(output), paste0("bw_", count, "$"))],
      function(x) x$beta[keep,]
    ), nrow = length(keep))
    
    # format the results into a table latex format
    results <- sapply(1:length(keep),
      function(idx2, covariate.labels, pval, coeff, se) {
        # format coefficients
        if (stars) {
          coeff_formatted <- mapply(
            function(x, y) {
              if ( x <= 0.10 & x > 0.05 ) { paste0(format2(y), "$^{*}$")
              } else if ( x <= 0.05 & x > 0.01 ) { paste0(format2(y), "$^{**}$")
              } else if ( x <= 0.01 ) { paste0(format2(y), "$^{***}$")
              } else { format2(y)
              }
            }, x = pval[idx2,], y = coeff[idx2,]
          )
        } else {
          coeff_formatted <- sapply(coeff[idx2,], format2)
        }
        coeff_formatted <- paste0(paste(
          c(covariate.labels[idx2], coeff_formatted),
          collapse = " & "
        ), "\\\\\n")
        
        # format standard errors
        se_formatted <- sapply(
          se[idx2,], function(x) paste0("(", str_trim(x), ")")
        )
        se_formatted <- paste0(paste(
          c("", se_formatted),
          collapse = " & "
        ), "\\\\\n")
        
        # combine the coefficients and standard errors into a vector and return
        return(c(coeff_formatted, se_formatted))
      }, covariate.labels = covariate.labels, pval = pval, coeff = coeff, se = se
    )
    results <- as.vector(results)
    
    # combine the results and lines into a table and append to output
    table <- capture.output(cat(
      results, lines2
    ))
    
    output[[paste0("output_table_bw_", count)]] <- table
  }
  return(output)
}


#' Function to plot RD estimates at various bandwidths, to test sensitivty to bandwidth size
#' 
#' @param output: output from estRD
#' @param lhs: string, the dependent variable to plot bandwidths for.  All bandwidths for lhs, except for omit_bw, will be plotted
#' @param omit_bw: character vector, name of bandwidths to omit plotting
#' @param coeffvar: string, coefficient to plot.  Default is "tworound", the RD estimate
#' @param color: TRUE/FALSE, should bw_1, bw_ik, and bw_mserd have different colors?  Default is FALSE
#' @param ...: other arguments to plotCoef
#' @export
plotBWs <- function(
  output, lhs, omit_bw = c(), coeffvar = "tworound", color = FALSE, ...) {
  
  # determine the bandwidths to plot for that lhs -- all bandwidths, except for omit_bw, available 
  # in output for that lhs will be plotted
  bws <- sort(output[[paste0(lhs, "_bws")]])
  bws <- bws[setdiff(names(bws), omit_bw)]
  
  # pass arguments onto plotCoef to plot the bandwidths
  plot <- plotCoef(
    output[paste0(lhs, "_", names(bws))], coeffvar,
    reg_lab = unlist(unname(bws)), reg_lab_numeric = TRUE, xlab = "Bandwidth size",
    group = 
      if ( color ) {
        case_when(
          bws == bws[["bw_1"]] ~ "Main",
          str_detect(names(bws), "ik$") ~ "IK", 
          str_detect(names(bws), "mserd$") ~ "MSERD",
          TRUE ~ "Standard")
      } else {
        "1"
      },
    group_colors = 
      if ( color ) {
        c("Standard" = "#000000", "Main" = "#FF0000", "IK" = "#0000FF", "MSERD" = "#FD00FF")
      } else {
        c("1" = "#000000")
      },
    annot_N = FALSE, ...
  )
  
  plot
  return(plot)
}


#' Function to plot RD estimates at placebo thresholds, to test smoothness of outcomes around the threshold
#' 
#' @param dt: data.table, for estimating regressions
#' @param lhs: string, dependent variable
#' @param spec: string, regression specification
#' @param bw: numeric, bandwidth to use for regressions
#' @param run_var: string, name of the running variable.  Default is "elig_voters"
#' @param cut: numeric, where the actual threshold is.  Default is 200,000
#' @param threshold_low: numeric, lowest placebo threshold to estimate.  Default is 170,000
#' @param threshold_high: numeric, highest placebo threshold to estimate.  Default is 230,000
#' @param threshold_by: numeric, function will estimate placebo thresholds from \code{seq(threshold_low, threshold_high, threshold_by)}.  Default is 10,000
#' @param file: string, file name to save the plot to
#' @param h: integer, height of the plot in inches
#' @param w: integer, width of the plot in inches
#' @import ggplot2
#' @import lfe
#' @export
placebos <- function(
  dt, lhs, spec, bw, run_var = "elig_voters", cut = 200000, 
  threshold_low = 170000, threshold_high = 230000, threshold_by = 10000, 
  file = NA, w = 16, h = 9) {
  
  # get a list of the variables needed to run the specification
  spec_var <- c(
    unique(setdiff(
      str_extract_all(spec, "[\\w_]+")[[1]], 
      c("0", "factor")
    )),
    lhs, run_var
  )
  dt_new <- dt[, copy(.SD), .SDcols = spec_var]
  
  # instantiate a list with the results from each placebo threshold regression
  results = list()
  cut_list = unique(c(seq(threshold_low, threshold_high, by = threshold_by), cut))
  
  # loop through each placebo threshold
  for ( i in sort(cut_list) ) {
    # create new running and treatment variables
    dt_new[, tworound := (get(run_var) >= i)*1]
    dt_new[, cut_dist := (get(run_var) - i)]
    dt_new[, cut_dist_abs := abs(get(run_var) - i)]
    
    # estimate the regression
    reg <- felm(as.formula(paste0(lhs, " ~ ", spec)), data = dt_new[cut_dist_abs <= bw])
    results = append(results, set_names(list(reg), paste0("threshold_", i)))
  }
  
  # plot the estimates
  plot <- plotCoef(
    results, coeffvar = "tworound",
    ylab = "Coefficient on TwoRound",
    reg_lab = c(
      format2(seq(threshold_low, cut - threshold_by, by = threshold_by)), 
      "200,000 (Actual)",
      format2(seq(cut + threshold_by, threshold_high, by = threshold_by))
    ),
    xlab = "Threshold",
    group = c(
      rep("placebos", length(seq(threshold_low, cut - threshold_by, by = threshold_by))),
      "actual",
      rep("placebos", length(seq(cut + threshold_by, threshold_high, by = threshold_by)))
    ), 
    group_colors = c(placebos = "#000000", actual = "#FF0000"),
    legend = FALSE,
    flip = TRUE
  )
  
  if ( !is.na(file) ) ggsave(file = file, width = w, height = h, units = "in", plot = plot)
  
  plot
  return(plot)
  
}


#' Function to plot coefficients and confidence intervals from a list of regressions.
#'
#' @param reg_ls: felm object list, containing the regressions to plot
#' @param coeffvar: character vector, indicating the coefficients to plot for each regression
#' @param ylab: string, caption for the y-axis (ie. the label for the coefficient values).  Default is "Coefficients on (coeffvar)"
#' @param reg_lab: character vector, containing labels for each regression.  Default is c("Regression 1", "Regression 2", ...)
#' @param reg_lab_numeric: TRUE/FALSE, should the regressions be treated as numeric?  Default is FALSE
#' @param xlab: string, caption for the x-axis (ie. the label for all regressions).  Default is an empty string
#' @param group: vector, indicating how the regressions should be grouped.  Default is the same group for all regressions
#' @param group_colors: named character vector, containing color values for each group, where the names are the group names.  Default is black ("#000000")
#' @param legend: TRUE/FALSE, should a legend for the colors be shown?  Default is TRUE if more than 1 group color is provided
#' @param annot_N: TRUE/FALSE, should the number of observations be annotated in the graph?  Default is FALSE
#' @param flip: TRUE/FALSE, should the axes be flipped?  Default is FALSE
#' @param file: string, file name to save the plot to
#' @param h: integer, height of the plot in inches
#' @param w: integer, width of the plot in inches
#' @import ggplot2
#' @export
plotCoef <- function(
  reg_ls, coeffvar,
  ylab = paste0("Coefficient on ", paste(unique(coeffvar), collapse = ", ")),
  reg_lab = paste("Regression ", seq(1, length(reg_ls))), reg_lab_numeric = FALSE, xlab = "",
  group = "1", group_colors = c("1" = "#000000"), 
  legend = ifelse(length(unique(group_colors)) > 1, TRUE, FALSE),
  annot_N = TRUE, flip = FALSE, 
  file = NA, w = 16, h = 9
) {
  
  # if regressions are not to be treated as numeric, set the regression ordering to the order
  # supplied in the arguments
  if ( !reg_lab_numeric ) {
    reg_lab <- factor(
      seq(1, length(reg_ls)), 
      labels = reg_lab,
      levels = seq(1, length(reg_ls)))
  }
  
  # retrieve regression statistics
  beta <- mapply(function(x, y) x$beta[y, ],
    x = reg_ls, y = coeffvar)
  
  cse <- mapply(function(x, y) x$clustervcv[y, y],
    x = reg_ls, y = coeffvar)
  
  N <- format2(sapply(reg_ls, function(x) x$N, USE.NAMES = FALSE))
  
  # create a data table for the results to be plotted
  results <- data.table(
    reg = paste("Regression", seq(1, length(reg_ls))),
    reg_lab = reg_lab,
    depvar = sapply(reg_ls, function(x) x$lhs, USE.NAMES = FALSE),
    group = factor(group, levels = unique(group)),
    beta = beta, cse = sqrt(cse),
    N = N,
    annot = paste0("N: ", N)
  )
  
  # calculate confidence intervals
  results[, `:=`(
    ci95_low = beta - 1.96 * cse,
    ci95_high = beta + 1.96 * cse,
    ci90_low = beta - 1.645 * cse,
    ci90_high = beta + 1.645 * cse
  )]
  
  # plot the results
  range <- max(results$ci95_high) - min(results$ci95_low)
  
  plot <- ggplot(data = results, aes(x = reg_lab, y = beta, color = group)) +
    theme_bw(base_size = 11) +
    theme(plot.caption = element_text(hjust = 0.5, size = rel(1.3))) +
    xlab(xlab) + ylab(ylab) +
    scale_y_continuous(limits = c(
      min(results$ci95_low, 0) - range * .1,
      max(results$ci95_high, 0) + range * .1
    )) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = ci95_low, ymax = ci95_high), size = 0.4) +
    geom_linerange(aes(ymin = ci90_low, ymax = ci90_high), size = 0.8) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    scale_color_manual(name = "", values = group_colors, guide = ifelse(legend, "legend", "none"))
  
  if (annot_N ) {
    plot <- plot + 
      geom_text(aes(label = results$annot, y = min(results$ci95_low, 0) - range * .1), 
        vjust = 0, size = 2.5)
  }
  
  if ( flip ) {
    plot <- plot + coord_flip()
  }
  
  if ( !is.na(file) ) ggsave(file = file, width = w, height = h, units = "in", plot = plot)
  
  return(plot)
}

