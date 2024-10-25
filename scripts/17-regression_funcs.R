# load libraries
library(lfe)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(fixest)
library(corrplot)
library(dplyr)
library(ggpubr)
library(patchwork)
library(broom)


#' Visualize correlation matrix for each type of bin (avg, min, max, diurnal)
#' 
#' @param df Dataframe containing all data 
#' @param title String title of plot 
get_correlation_matrix <- function(df, title) {
  # filter out cols with sd = 0 (cols w all same entries)
  df <- Filter(function(x) sd(x) != 0, df)
  names(df) <- get_bin_names(names(df))
  corrplot(cor(df), method="color", tl.cex=0.5, main=title, mar=c(2, 2, 2, 2)) # tl.cex makes text smaller 
}


#' Create df with num_degrees degree bins
#' 
#' @param num_degrees Int representing how large each bin interval is 
#' @param df_full Dataframe with one-degree temp bins 
#' @return Dataframe with newly binned bins. Does not change original dataframe
get_binned_df <- function(num_degrees, df_full) {
  # create copy of df_full, to modify
  df_bins <- df_full[, grepl("DAILY|DIURNAL", names(df_full))]
  df_not_bins <- df_full[, !colnames(df_full) %in% colnames(df_bins)]
  df_final <- data.frame(df_not_bins) 
  
  # get relevant idxs for one bin "type" (here, AVG) 
  num_idxs <- 2 * ((length(names(df_full[, grepl("AVG", names(df_full))]))) %/% num_degrees) - 2 
  root_idxs <- c(1, 1 + num_degrees)
  for (i in 2:num_idxs) {
    if (i %% 2 == 0) {
      root_idxs <- c(root_idxs, root_idxs[i] + 1)
    }
    else {
      root_idxs <- c(root_idxs, root_idxs[i] + (num_degrees - 1))
    }
  }
  root_idxs <- c(root_idxs, 42)
  
  # get idxs for remaining bin "types" (min, max, diurnal temp bins)
  idxs <- c(root_idxs, root_idxs + 42, root_idxs + 2 * 42, root_idxs + 3 * 42)
  
  # sum relevant rows, and join to df_final
  for (i in seq(1, length(idxs) - 1, by=2)) {
    idx <- idxs[i]
    idx_1 <- idxs[i + 1]
    
    # get str before second-to-last underscore
    prefix <- str_extract(names(df_bins)[idx], ".*(?=_[^_]*_[^_]*$)") 
    # get str between second-to-last and last underscore
    first_num <- str_extract(names(df_bins)[idx], "(?<=_)[^_]*(?=_[^_]*$)")
    # get str after last underscore
    second_num <-  str_extract(names(df_bins)[idx_1], "(?<=_)[^_]+$")
    
    # get prefix of colname 
    colname <- paste(prefix, first_num, second_num, sep="_")
    df_final[, colname] <- c(df_bins[, idx:idx_1] %>% rowSums)
  }
  
  return(df_final)
}


#' Get bins in interval notation to use for visualizations 
#' 
#' @param curr_names Current bin names 
#' @return Vector of new bin names
get_bin_names <- function(curr_names) {
  bin_names <- c()
  for (name in curr_names) {
    first_num <- str_extract(name, "(?<=_)[^_]*(?=_[^_]*$)")
    second_num <-  str_extract(name, "(?<=_)[^_]+$")
    
    if ((first_num == "Inf") == TRUE) {
      bin_names <- c(bin_names, paste0("<", second_num))
    }
    else if (second_num == "Inf") {
      # use greater than or equal symbol's unicode 
      bin_names <- c(bin_names, paste0("\u2265", first_num)) 
    }
    else {
      bin_names <- c(bin_names, paste0("[", first_num, ",", second_num, ")"))
    }
  }
  return(bin_names)
}


#' Get age groups in interval notation, to use for visualizations
#' 
#' @param curr_names Current age names 
#' @return Vector of new age names
get_age_names <- function(curr_names) {
  age_names <- c()
  for (i in 1:length(curr_names)) {
    name <- curr_names[[i]]
    first_num <- str_extract(name, "(?<=_)[^_]*(?=_[^_]*$)")
    second_num <-  str_extract(name, "(?<=_)[^_]+$")
    
    if (i == 1) {
      age_names <- c(age_names, paste0("\u2264", second_num))
    }
    else if (i == length(curr_names)) {
      # get first nums only, since very laste age group follows "Age_85_and_Over" format
      num <- str_extract(name, "\\d+") 
      age_names <- c(age_names, paste0("\u2265", num))
    }
    else {
      age_names <- c(age_names, paste0("[", first_num, ",", second_num, "]"))
    }
  }
  return(age_names)
}


#' Merge consecutive columns with zero variance with nearest column
#' with non-zero variance, to eliminate NA coeffs in regression
#'
#' @param binned_df Dataframe; checks each col's variance
#' @return A dataframe with merged cols. Does not change original dataframe
merge_no_var_cols <- function(binned_df) {
  # get idxs of cols with sd = 0 
  zero_sd <- (sapply(binned_df, sd) == 0) %>% which
  
  if (length(zero_sd) == 0) {
    return(binned_df)
  }
  
  # group together indices that r consecutive, aka indices that have a difference = 1 with neighbor index
  consec <- c(0, cumsum(diff(zero_sd) > 1))
  idxs <- split(zero_sd, consec)
  
  # merge cols using idxs
  for (i in 1:length(idxs)) {
    # merge interval with the col either before interval or after interval
    first_idx <- idxs[[i]][1]
    second_idx <- idxs[[i]][length(idxs[[i]])]
    
    if (first_idx == 1) {
      second_idx <- second_idx + 1
      idxs[[i]] <- append(idxs[[i]], second_idx) %>% sort
    }
    else {
      first_idx <- first_idx - 1
      idxs[[i]] <- append(idxs[[i]], first_idx) %>% sort
    }
    
    old_col_name_1 <- names(binned_df)[first_idx]
    old_col_name_2 <- names(binned_df)[second_idx]
    
    # get str before second-to-last underscore
    prefix <- str_extract(old_col_name_1, ".*(?=_[^_]*_[^_]*$)")
    # get str between second-to-last and last underscore
    first_num <- str_extract(old_col_name_1, "(?<=_)[^_]*(?=_[^_]*$)")
    # get str after last underscore
    second_num <-  str_extract(old_col_name_2, "(?<=_)[^_]+$")
    
    new_col_name <- paste(prefix, first_num, second_num, sep = "_")
    new_col <- rowSums(binned_df[first_idx:second_idx])
    
    binned_df[new_col_name] <- new_col
  }
  
  # drop cols, reorder
  binned_df <- binned_df[, -unlist(idxs)]
  reordered_names <- names(binned_df)[names(binned_df) %>% str_extract("\\d+") %>% as.numeric %>% order]
  binned_df <- binned_df[, reordered_names]
  
  return(binned_df)
}


#' Omit bin containing reference bin to avoid collinearity
#' 
#' @param binned_df Dataframe to filter bins from 
#' @param bin String indicator for bin "type" 
#' @param ref Number representing reference bin
#' @return A vector of filtered bin names
omit_bin <- function(binned_df, bin, ref) {
  curr_bin_names <- names(binned_df)[grepl(bin, names(binned_df))]
  
  # get nums corresponding to each bin name
  parsed <- lapply(curr_bin_names, function(x) {
    nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+", x))))
    return(c(min(nums), max(nums)))
  })
  
  # get all names except for name containing ref 
  filtered_bin_names <- curr_bin_names[!sapply(parsed, function(x) {
    x[1] <= ref & ref < x[2]
  })]
  
  return(filtered_bin_names)
}


#' Generate all-cause mortality vs temp bins, for all bin types (and heatwave counts)
#' 
#' @param df Panel data (dataframe) to extract data from 
#' @param bins A vector of identifiers for each bin "type" ("AVG" for avg temp bins, "MIN" for min temp bins, etc)
#' @param deg A number representing how large each bin interval should be
#' @param ref A number representing the bin to omit, to serve as our reference bin
#' @param response A string representing the response variable column name in df
#' @param y_title A string representing the title of the resulting graph
#' @param filename The filepath to save the graph to
#' @return summaries A list of summaries for each linear regression
get_bin_graphs <- function(df, bins, deg, ref, response, y_title, filename) { 
  grids <- list() 
  summaries <- list() 
  
  # get binned df, where cols with 0 variance are merged together
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols()
  
  # get plot for each bin
  for (i in 1:length(bins)) {
    bin <- bins[i]
    bin_names <- names(binned_df)[grepl(bin, names(binned_df))]
    bin_renamed <- get_bin_names(bin_names)
    
    # omit bin containing ref to avoid collinearity 
    filt_bin_names <- omit_bin(binned_df, bin, ref)
    filt_bin_renamed <- get_bin_names(filt_bin_names)
    
    ref_bin <- setdiff(bin_renamed, filt_bin_renamed)
    
    # create eqn 
    eqn <- as.formula(paste(response, "~", paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
    reg <- felm(eqn, data=binned_df)
    summaries[[bin]] <- summary(reg)
    
    # create df from reg 
    reg_df <- data.frame(bin=filt_bin_renamed, coeff=reg$coefficients[, 1], se=reg$se,
                         pval=format(round(reg$pval, digits=2), nsmall=2)) %>% na.omit
    
    # add row for reference bin data (all 0s) 
    reg_df <- rbind(reg_df, list(ref_bin, 0, 0, 0))
    
    reg_df$bin <- factor(reg_df$bin, levels=bin_renamed) # ensure that x ticks r labelled how we want
    
    # get plot
    plt_title <- paste0("Daily ", tolower(bin), " ", deg, "-degree temp bins")
    plt_x <- paste("Daily", tolower(bin), "temp bins")
    if (bin == "DAILY_MAX") {
      plt_title <- paste0("Daily max ", deg, "-degree temp bins")
      plt_x <- paste("Daily max temp bins")
    }
    plt_y <- y_title
    plt <- ggplot(reg_df, aes(x=bin, y=coeff, group=1)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
      geom_line() + 
      geom_point(shape=21, fill="white") +
      labs(title=plt_title, x=plt_x, y=plt_y) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5))
    
    grids[[i]] <- plt
  }
  
  g <- grid.arrange(grobs=grids)
  ggsave(filename=filename, height=7, width=10, g)
  # do.call("grid.arrange", grids)
  return(summaries)
}


#' Get all-cause heatwave graph 
#' 
#' @param df Panel dataframe
#' @param response A string representing the response variable column name in df
#' @param filename The filepath to save the graph to
#' @return Summary of heatwave regression
get_heatwave_graph <- function(df, response, filename) {
  eqn <- as.formula(paste(response, "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR"))
  heatwave <- felm(formula=eqn, data=df)
  heatwave_df <- data.frame(x=df[, "HEATWAVE_CNT"], y=df[, response])
  
  # get data from predicted polynomial
  x_vals <- seq(01, 16, by=0.5)
  predict_vals <- function(x) {
    as.numeric(heatwave$coefficients[1])*x +
      as.numeric(heatwave$coefficients[2])*x^2 +
      as.numeric(heatwave$coefficients[3])*x^3
  }
  y_vals <- predict_vals(x_vals)
  line_data <- data.frame(x=x_vals, y=y_vals) 
  
  # get plot 
  plt <- ggplot(heatwave_df, aes(x, y)) +
    geom_point() +
    geom_line(line_data, mapping=aes(x, y)) +
    labs(title=paste("Mortality rates vs heatwave day counts"),
         x ="Heatwave day counts", y = "Mortality rate") +
    theme(plot.title=element_text(hjust=0.5))
  
  g <- grid.arrange(plt)
  ggsave(filename=filename, height=7, width=10, g)
  
  return(summary(heatwave))
}


#' Get all-cause max-diurnal range graph 
#' 
#' @param df Panel dataframe
#' @param ref Reference bin to omit
#' @param response A string representing the response variable column name in df
#' @param filename The filepath to save the graph to
#' @return Summary of heatwave regression
get_max_rng_graph <- function(df, ref, y_title, response, filename) {
  # get relevant cols, and merge bins w zero variance
  merged_df <- merge_no_var_cols(df)
  max_rng_df <- merged_df[grepl("MAX_RNG", names(merged_df))] 
  bin_names <- get_bin_names(names(max_rng_df))

  # omit bin containing ref 
  filt_bin_names <- omit_bin(max_rng_df, "MAX_RNG", ref)
  filt_bin_renamed <- get_bin_names(filt_bin_names)
  ref_bin <- setdiff(bin_names, filt_bin_renamed)
  
  # create eqn, run reg 
  eqn <- as.formula(paste(response, "~",  paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
  reg <- felm(eqn, data=merged_df)
  
  # create df to use for plotting
  reg_df <- data.frame(bin=filt_bin_renamed, coeff=reg$coefficients[, 1], se=reg$se) %>% na.omit
  
  # add ref bin to df for plotting
  reg_df <- rbind(reg_df, list(ref_bin, 0, 0, 0))
  reg_df$bin <- factor(reg_df$bin, levels=bin_names)
  
  # get plot
  plt_title <- paste0("Daily maximum-diurnal range temeprature interactions")
  plt_x <- paste("Maximum-diurnal range temp bins")
  plt_y <- y_title
  plt <- ggplot(reg_df, aes(x=bin, y=coeff, group=1)) + 
    geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill = "grey70") + 
    geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
    geom_line() + 
    geom_point(shape=21, fill="white") +
    labs(title=plt_title, x=plt_x, y=plt_y) +
    theme(text=element_text(size=9), plot.title=element_text(hjust=0.5))
  
  g <- grid.arrange(plt)
  ggsave(filename=filename, height=7, width=10, g)
  return(summary(reg))
}

#' Get age-stratified mortaltiy rates vs bins for each bin type
#' 
#' @param df Panel data dataframe
#' @param deg Number representing degree itnerval width
#' @param bins Vector of bin "types" (avg, min, max, diurnal)
#' @param y_title String representing name of y axis for graphs
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_age_stratified_graphs <- function(df, deg, bins, ref, y_title, filename) {
  # group bins based on deg 
  binned_df <- get_binned_df(deg, df)
  processed_binned_df <- merge_no_var_cols(binned_df)
  
  # get formulas and eqns for each age group, for each bin
  grids <- list()
  eqns <- list()
  for (i in 1:length(bins)) {
    # omit ref  
    bin <- bins[i]
    bin_df <- processed_binned_df[grepl(bin, names(processed_binned_df))]
    filtered_bin_names <- omit_bin(bin_df, bin, ref) 
    
    # get col names (for bins) 
    bin_names <- get_bin_names(filtered_bin_names)
    age_names <- get_age_names(names(processed_binned_df)[grepl("Age", names(processed_binned_df))])
    actual_age_names <- names(processed_binned_df)[grepl("Age", names(processed_binned_df))]
    
    plt_df <- data.frame()
    for (j in 1:length(age_names)) {
      # get formula, eqn 
      age_formula <- as.formula(paste(actual_age_names[j], "~", paste(filtered_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
      age_eqn <- felm(age_formula, data=processed_binned_df)
      eqns[[paste(bin, actual_age_names[j])]] <- summary(age_eqn)
      
      # add to df 
      age_df <- data.frame(bin=bin_names, coeff=age_eqn$beta[, 1], se=age_eqn$se, 
                           pval=format(round(age_eqn$pval, digits=2), nsmall=2),
                           age_group=age_names[j]) %>% na.omit
      age_df$bin <- factor(age_df$bin, levels=age_df$bin)
      plt_df <- rbind(plt_df, age_df)
    }
    
    # create plot 
    plt_title <- paste("Age-stratified", bin %>% tolower, paste0(deg, "-deg"), "temp bins")
    plt_x <- paste("Daily", bin %>% tolower, "temp bins")

    plt <- ggplot(plt_df, aes(x=bin, y=coeff, group=age_group)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se, fill=age_group), alpha=0.3) + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se, color=age_group), show.legend=FALSE, width=.1) + 
      geom_line(aes(color=age_group), show.legend=FALSE) + 
      geom_point(aes(color=age_group), show.legend=FALSE) +
      labs(title=plt_title, x=plt_x, y=y_title) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
      scale_fill_discrete(breaks=age_names, name="Age group") 
    
    grids[[i]] <- plt
  }
  
  # return(grids)
  g <- ggarrange(plotlist=grids, common.legend=TRUE, legend="bottom")
  ggsave(filename=filename, height=7, width=10, g)
  # print(ggarrange(plotlist=grids, common.legend=TRUE, legend="bottom"))
  return(eqns)
}


#' Get age-stratified mortaltiy rates vs heatwave day counts
#'
#' @param df Dataframe containing zipcode, year, heatwave, mortaltiy data
#' @param y_title A string representing the title of the resulting graph
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_age_stratified_heatwave_graph <- function(df, y_title, filename) {
  # get age col names  
  df <- merge_no_var_cols(df)
  age_names <- get_age_names(names(df)[grepl("Age", names(df))])
  actual_age_names <- names(df)[grepl("Age", names(df))]
  summaries <- list()
  
  # get heatwave plot
  heatwave_df <- data.frame()
  for (i in 1:length(age_names)) {
    # get heatwave eqn, df 
    heatwave_formula <- as.formula(paste(actual_age_names[i], "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3)", "| ZIPCODE + YEAR"))
    heatwave <- felm(heatwave_formula, data = df)
    
    # get predicted y vals
    x_vals <- seq(0, 16, by=0.5)
    predict_vals <- function(x) {
      as.numeric(heatwave$coefficients[1])*x +
        as.numeric(heatwave$coefficients[2])*x^2 +
        as.numeric(heatwave$coefficients[3])*x^3
    }
    y_vals <- predict_vals(x_vals)
    
    data <- data.frame(x=x_vals, y=y_vals, age_group=age_names[i]) 
    heatwave_df <- rbind(heatwave_df, data)
    
    # add eqn to list 
    summaries[[actual_age_names[i]]] <- summary(heatwave)
  }
  
  # get heatwave plot!
  heatwave_df$age_group <- factor(heatwave_df$age_group, levels=age_names)
  
  plt <- ggplot(heatwave_df, aes(x, y, group=age_group)) + 
    geom_line(aes(color=age_group)) + 
    labs(title=paste("Age-stratified heatwave day counts vs" , tolower(y_title)), x="Heatwave day counts", y=y_title) +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_fill_discrete(name="Age group") 
  
  g <- grid.arrange(plt)
  ggsave(filename=filename, height=7, width=10, g)
  return(summaries)
}

#' Get age-stratified mortaltiy rates vs bins for each bin type
#' 
#' @param df Panel data dataframe
#' @param deg Number representing degree itnerval width
#' @param bins Vector of bin "types" (avg, min, max, diurnal)
#' @param y_title String representing name of y axis for graphs
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_age_stratified_max_rng <- function(df, ref, y_title, filename) {
  # group bins based on deg 
  max_rng_df <- df[grepl("MAX_RNG", names(df))] %>% merge_no_var_cols
  bin_names <- get_bin_names(names(max_rng_df))
  max_rng_df <- cbind(max_rng_df, df[grepl("YEAR|ZIPCODE|Age", names(df))])
  
  # omit bin containing ref 
  filt_bin_names <- omit_bin(max_rng_df, "MAX_RNG", ref)
  filt_bin_renamed <- get_bin_names(filt_bin_names)
  ref_bin <- setdiff(bin_names, filt_bin_renamed)
  
  # get age col names  
  age_names <- get_age_names(names(df)[grepl("Age", names(df))])
  actual_age_names <- names(df)[grepl("Age", names(df))]
  
  summaries <- list()
  plt_df <- data.frame()
  # get formulas and eqns for each age group
  for (i in 1:length(age_names)) {
    # get formula, eqn 
    age_formula <- as.formula(paste(actual_age_names[i], "~", paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
    age_eqn <- felm(age_formula, data=max_rng_df)
    summaries[[actual_age_names[i]]] <- summary(age_eqn)
    
    # add to df 
    age_df <- data.frame(bin=filt_bin_renamed, coeff=age_eqn$beta[, 1], se=age_eqn$se, age_group=age_names[i]) %>% na.omit
    
    # add back omitted bin 
    age_df <- rbind(age_df, list(ref_bin, 0, 0, age_names[i]))
    age_df$bin <- factor(age_df$bin, levels=bin_names)
    plt_df <- rbind(plt_df, age_df)
  }
    
  # create plot 
  plt_title <- paste("Age-stratified maximum-diurnal range temp interactions")
  plt_x <- paste("Daily max-diurnal range bins")
    
  plt <- ggplot(plt_df, aes(x=bin, y=coeff, group=age_group)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se, fill=age_group), alpha=0.3) + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se, color=age_group), show.legend=FALSE, width=.1) + 
      geom_line(aes(color=age_group), show.legend=FALSE) + 
      geom_point(aes(color=age_group), show.legend=FALSE) +
      labs(title=plt_title, x=plt_x, y=y_title) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) +
      scale_fill_discrete(breaks=age_names, name="Age group") 
  
  g <- grid.arrange(plt)
  ggsave(filename=filename, height=7, width=10, g)
  return(summaries)
}


#' Get all-age cause-specific graphs for each bin 
#'
#' @param df Panel data dataframe
#' @param causes The causes to extract from df. If null, then gets all causes
#' @param deg Number representing degree itnerval width
#' @param bins Vector of bin "types" (avg, min, max, diurnal)
#' @param y_title Name of y axis 
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_cause_specific_graphs <- function(df, causes, bins, deg, ref, y_title, filename) {
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols
  
  if (is.null(causes)) {
     # get relevant cols 
    not_cause_df <- binned_df[grepl("DAILY|DIURNAL|MAX|Age|HEATWAVE|YEAR|ZIPCODE", names(binned_df))]
    cause_df <- binned_df[!names(binned_df) %in% names(not_cause_df)]
  }
  else {
    cause_df <- binned_df[grepl(causes, names(binned_df))]
  }
  
  cause_names_unordered <- names(cause_df)
  cause_names <- cause_names_unordered[order(cause_names_unordered)]
  
  summaries <- list()
  for (bin in bins) {
    bin_df <- binned_df[grepl(bin, names(binned_df))]
    
    # omit reference 
    filtered_bin_names <- omit_bin(bin_df, bin, ref) 
    formatted_bin_names <- get_bin_names(filtered_bin_names)
    
    grids <- list()
    cause_summaries <- list()
    for (i in 1:length(cause_names)) {
      cause <- cause_names[[i]]
      # get eqn and dfs 
      eqn <- as.formula(paste(cause, "~", paste(filtered_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
      reg <- felm(eqn, data=binned_df)
      
      plt_df <- data.frame(bin=formatted_bin_names, coeff=reg$coefficients[, 1], se=reg$se, 
                           pval=format(round(reg$pval, digits=2), nsmall=2)) %>% na.omit
      plt_df$bin <- factor(plt_df$bin, levels=plt_df$bin)
      
      plt_title <- cause
      if (cause == "EDvisits") {
        plt_title <- "Total"
      }
      plt <- ggplot(plt_df, aes(x=bin, y=coeff, group=1)) + 
        geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill="grey") + 
        geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
        geom_line() + 
        geom_point(shape=21, fill="white") +
        labs(title=plt_title, x=NULL, y=NULL) +
        theme(text=element_text(size=9), plot.title=element_text(hjust=0.5)) 

      grids[[i]] <- plt
      cause_summaries[[cause]] <- summary(reg)
    }
    summaries[[bin]] <- cause_summaries
    # do.call("grid.arrange", grids)
    
    plt_x <- paste0("Daily ", tolower(bin), " ", deg, "-degree temp bins")
    if (bin == "DAILY_MAX") {
      plt_x <- paste0("Daily max ", deg, "-degree temp bins")
    }
    plt_y <- y_title
    
    g <- grid.arrange(grobs=grids, left=plt_y, bottom=plt_x)
    # g <- arrangeGrob(grobs=grids, left=plt_y, bottom=plt_x)
    ggsave(paste0(filename, "_", bin, ".png"), height=7, width=10, g)
    
  }
  return(summaries)
}


#' Get all-age cause-specific graphs for heatwave 
#'
#' @param df Panel data dataframe
#' @param y_title A string representing the title of the resulting graph
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_cause_specific_heatwave <- function(df, y_title, filename) {
  # get relevant cols 
  not_cause_df <- df[grepl("DAILY|DIURNAL|Age|HEATWAVE|YEAR|ZIPCODE", names(df))]
  cause_df <- df[!names(df) %in% names(not_cause_df)]
  cause_names_unordered <- names(cause_df)
  cause_names <- cause_names_unordered[order(cause_names_unordered)]
  
  grids <- list()
  summaries <- list()
  for (i in 1:length(cause_names)) {
    cause <- cause_names[[i]]
    # get eqn and dfs 
    eqn <- as.formula(paste(cause, "~", "HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR"))
    reg <- felm(eqn, data=df)
    
    plt_df <- data.frame(x=df[, "HEATWAVE_CNT"], y=df[, cause])
    
    # get data from predicted polynomial
    x_vals <- seq(0, max(plt_df$x), by=0.5)
    predict_vals <- function(x) {
      as.numeric(reg$coefficients[1])*x +
        as.numeric(reg$coefficients[2])*x^2 +
        as.numeric(reg$coefficients[3])*x^3
    }
    y_vals <- predict_vals(x_vals)
    line_data <- data.frame(x=x_vals, y=y_vals) 
    
    # get plot 
    plt <- ggplot(plt_df, mapping=aes(x=x, y=y)) +
      geom_point() +
      geom_line(line_data, mapping=aes(x, y)) +
      labs(title=cause, x=NULL, y =NULL) +
      theme(plot.title=element_text(hjust=0.5))
    
    grids[[i]] <- plt
    summaries[[cause]] <- summary(reg)
  }
  
  g <- grid.arrange(grobs=grids, left=y_title, bottom="Heatwave day counts")
  ggsave(filename, height=7, width=10, g)
  return(summaries)
}


#' Get all-age cause-specific graphs for max-diurnal range 
#'
#' @param df Panel data dataframe
#' @param causes Causes to extract from df. If null, then extracts all causes
#' @param ref The reference bin to omit
#' @param y_title A string representing the title of the resulting graph
#' @param filename The filepath to save the graph to
#' @return List of regression summaries used in graphs
get_cause_specific_max_rng <- function(df, causes, ref, y_title, filename) {
  # get relevant cols
  if (is.null(causes)) {
    not_cause_df <- df[grepl("DAILY|DIURNAL|MAX_RNG|Age|HEATWAVE|YEAR|ZIPCODE", names(df))]
    cause_df <- df[!names(df) %in% names(not_cause_df)]
  }
  else {
    cause_df <- df[grepl(causes, names(df))]
  }
  cause_names_unordered <- names(cause_df)
  cause_names <- cause_names_unordered[order(cause_names_unordered)]
  
  max_rng_df <- df[grepl("MAX_RNG", names(df))] %>% merge_no_var_cols
  bin_names <- get_bin_names(names(max_rng_df))
  max_rng_df <- cbind(max_rng_df, cause_df) %>% cbind(df[grepl("YEAR|ZIPCODE", names(df))])
  
  # omit bin containing ref 
  filt_bin_names <- omit_bin(max_rng_df, "MAX_RNG", ref)
  filt_bin_renamed <- get_bin_names(filt_bin_names)
  ref_bin <- setdiff(bin_names, filt_bin_renamed)
  
  grids <- list()
  summaries <- list()
  for (i in 1:length(cause_names)) {
    cause <- cause_names[[i]]
    # get eqn and dfs 
    eqn <- as.formula(paste(cause, "~", paste(filt_bin_names, collapse="+"), "| ZIPCODE + YEAR"))
    reg <- felm(eqn, data=max_rng_df)
    
    plt_df <- data.frame(bin=filt_bin_renamed, coeff=reg$coefficients[, 1], se=reg$se) %>% na.omit
    plt_df <- rbind(plt_df, list(ref_bin, 0, 0))
    plt_df$bin <- factor(plt_df$bin, levels=bin_names)
    
    plt_title <- cause
    if (cause == "EDvisits") {
      plt_title <- "Total"
    }
    plt <- ggplot(plt_df, aes(x=bin, y=coeff, group=1)) + 
      geom_ribbon(aes(ymin = coeff - se, ymax = coeff + se), fill="grey") + 
      geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.1) + 
      geom_line() + 
      geom_point(shape=21, fill="white") +
      labs(title=plt_title, x=NULL, y=NULL) +
      theme(text=element_text(size=9), plot.title=element_text(hjust=0.5))
    
    grids[[i]] <- plt
    summaries[[cause]] <- summary(reg)
  }

  plt_x <- paste0("Maximum-diurnal range temp interactions")
  plt_y <- y_title
  g <- grid.arrange(grobs=grids, left=plt_y, bottom=plt_x)
  ggsave(filename=filename, height=7, width=10, g)
  return(summaries)
}


#' Run fixed effects model on both temp bins and heatwave counts 
#' 
#' @param df Panel data dataframe
#' @param deg Number representing degree itnerval width
#' @param bins Vector of bin "types" (avg, min, max, diurnal)
#' @param ref Int representing bin to omit as reference bin
#' @param filename Base filepath to save each model's output
#' @return List of summaries from regressions
get_bin_heatwave_graphs <- function(df, bins, deg, ref, response, filename) {
  # group by deg 
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols
  summaries <- list()
  for (bin in bins) {
    # get corresponding bins, and omit 
    bin_df <- binned_df[grepl(bin, names(binned_df))]
    filtered_bin_names <- omit_bin(bin_df, bin, ref)
    
    eqn <- as.formula(paste(response, "~", paste(filtered_bin_names, collapse="+"), "+ HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR"))
    reg <- felm(eqn, data=binned_df)
    tidy(reg) %>% mutate_if(is.numeric, round, digits=3) %>% write.csv(file.path(paste0(filename, "_", tolower(bin), ".csv")), row.names=FALSE)
    summaries[[bin]] <- summary(reg)
  }
  return(summaries)
}

#' Run fixed effects model on both temp bins and heatwave counts 
#' 
#' @param df Panel data dataframe
#' @param deg Number representing degree itnerval width
#' @param bin Bin type (here, the max-diurbal range temp)
#' @param ref Int representing bin to omit as reference bin
#' @param response Response variable, here the total mortatliy (morbidity) counts
#' @param filename Filepath to save regression output
#' @return List of summaries from regressions
get_heatwave_max_rng_graph <- function(df, bin, deg, ref, response, filename) {
  # group by deg 
  binned_df <- get_binned_df(deg, df) %>% merge_no_var_cols
  summaries <- list()
  # get corresponding bins, and omit 
  bin_df <- binned_df[grepl(bin, names(binned_df))]
  filtered_bin_names <- omit_bin(bin_df, bin, ref)
  eqn <- as.formula(paste(response, "~", paste(filtered_bin_names, collapse="+"), "+ HEATWAVE_CNT + I(HEATWAVE_CNT^2) + I(HEATWAVE_CNT^3) | ZIPCODE + YEAR"))
  reg <- felm(eqn, data=binned_df)
  # save regression results as csv
  tidy(reg) %>% mutate_if(is.numeric, round, digits=3) %>% write.csv(filename, row.names=FALSE)
  return(summary(reg))
}
