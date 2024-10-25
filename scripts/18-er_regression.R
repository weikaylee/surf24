# load funcs to get graphs 
source(file.path("scripts", "regression_funcs.R"))

# consts
panel_path <- file.path("processed", "merged", "measures_er.csv")
panel <- read.csv(panel_path)
bins <- c("AVG", "MIN", "DAILY_MAX", "DIURNAL")
ref_temp <- 12 # to indicate which bin to omit 
bin <- 5 # bin width, based on results from trying dif temp bins 
total_counts_colname <- "EDvisits"
y_title <- "Change in annual ED visit\nrate per additional day"
causes <- "EDvisits|Symptoms|Circulatory|Respiratory|Other|InjuriesPoisoning"


# plot ed visit rates against temp bins 
three <- get_bin_graphs(panel, bins, 3, ref_temp, total_counts_colname, y_title, file.path("figures", "er_three_deg.png"))
four <- get_bin_graphs(panel, bins, 4, ref_temp, total_counts_colname, y_title, file.path("figures", "er_four_deg.png"))
five <- get_bin_graphs(panel, bins, 5, ref_temp, total_counts_colname, y_title, file.path("figures", "er_five_deg.png"))
six <- get_bin_graphs(panel, bins, 6, ref_temp, total_counts_colname, y_title, file.path("figures", "er_six_deg.png"))

heatwave <- get_heatwave_graph(panel, total_counts_colname, file.path("figures", "er_heatwave.png"))
age_strat <- get_age_stratified_graphs(panel, bin, bins, ref_temp, y_title, file.path("figures", "er_age_strat.png")) 
age_strat_heatwave <- get_age_stratified_heatwave_graph(panel, "ED visit rate", file.path("figures", "er_age_strat_heatwave.png"))
cause_specific_some <- get_cause_specific_graphs(panel, causes, bins, bin, ref_temp, y_title, file.path("figures", "er_cause_specific_some.png"))
cause_specific_all <- get_cause_specific_graphs(panel, NULL, bins, bin, ref_temp, y_title, file.path("figures", "er_cause_specific_all.png"))
cause_specific_heatwave <- get_cause_specific_heatwave(panel, "ED visit rate", file.path("figures", "er_cause_specific_heatwave.png"))
combo <- get_bin_heatwave_graphs(panel, bins, bin, ref_temp, total_counts_colname, file.path("figures", "er_heatwave"))

# for max rng 
max_rng_ref_bin <- 2.5
max_rng <- get_max_rng_graph(panel, max_rng_ref_bin, y_title, total_counts_colname, file.path("figures", "er_max_rng.png"))
max_rng_age_strat <- get_age_stratified_max_rng(panel, max_rng_ref_bin, y_title, file.path("figures", "er_max_rng_age_strat.png"))
max_rng_cause_some <- get_cause_specific_max_rng(panel, causes, max_rng_ref_bin, y_title, file.path("figures", "er_max_rng_cause_some.png"))
max_rng_cause_all <- get_cause_specific_max_rng(panel, NULL, max_rng_ref_bin, y_title, file.path("figures", "er_max_rng_cause_all.png"))
max_rng_combo <- get_heatwave_max_rng_graph(panel, "MAX_RNG", bin, max_rng_ref_bin, total_counts_colname, file.path("figures", "er_heatwave_max_rng.csv"))
