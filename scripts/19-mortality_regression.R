# load funcs to get graphs 
source(file.path("scripts", "regression_funcs.R"))

# consts
panel_path_mort <- file.path("processed", "merged", "measures_mortality.csv")
panel_mort <- read.csv(panel_path_mort)
bins <- c("AVG", "MIN", "DAILY_MAX", "DIURNAL")
ref_temp <- 12 # to indicate which bin to omit 
bin <- 5 # width of bins, based on results from get_bin_graphs below
total_counts_colname_mort <- "Total"
y_title_mort <- "Change in annual mortality\nrate per additional day"
causes_mort <- "Total|Accidents|Heart|Respiratory|Hypertension|Parkinsons"

# get_correlation_matrix(panel[grepl("AVG", names(panel))], "Avg one-deg temp bins")
# get_correlation_matrix(panel[grepl("MIN", names(panel))], "Min one-deg temp bins")
# get_correlation_matrix(panel[grepl("MAX", names(panel))], "Max one-deg temp bins")
# get_correlation_matrix(panel[grepl("DIURNAL", names(panel))], "Diurnal one-deg temp bins")

three <- get_bin_graphs(panel_mort, bins, 3, ref_temp, total_counts_colname_mort, y_title_mort, file.path("figures", "mort_three_deg.png"))
four <- get_bin_graphs(panel_mort, bins, 4, ref_temp, total_counts_colname_mort, y_title_mort, file.path("figures", "mort_four_deg.png"))
five <- get_bin_graphs(panel_mort, bins, 5, ref_temp, total_counts_colname_mort, y_title_mort, file.path("figures", "mort_five_deg.png"))
six <- get_bin_graphs(panel_mort, bins, 6, ref_temp, total_counts_colname_mort, y_title_mort, file.path("figures", "mort_six_deg.png"))

heatwave <- get_heatwave_graph(panel_mort, total_counts_colname_mort, file.path("figures", "mort_heatwave.png"))
age_strat <- get_age_stratified_graphs(panel_mort, bin, bins, ref_temp, y_title_mort, file.path("figures", "mort_age_strat.png")) # plot doesnt show up sometimes; can get by returning list of plots and plotting 
age_strat_heatwave <- get_age_stratified_heatwave_graph(panel_mort, "Mortality rate", file.path("figures", "mort_age_strat_heatwave.png"))
cause_specific_some <- get_cause_specific_graphs(panel_mort, causes_mort, bins, bin, ref_temp, y_title_mort, file.path("figures", "mort_cause_specific_some.png"))
cause_specific_all <- get_cause_specific_graphs(panel_mort, NULL, bins, bin, ref_temp, y_title_mort, file.path("figures", "mort_cause_specific_all.png"))
cause_specific_heatwave <- get_cause_specific_heatwave(panel_mort, "Mortality rate", file.path("figures", "mort_cause_specific_heatwave.png"))
combo <- get_bin_heatwave_graphs(panel_mort, bins, bin, ref_temp, total_counts_colname_mort, file.path("figures", "mort_heatwave"))  

# for max rng 
max_rng_ref_bin <- 2.5
max_rng <- get_max_rng_graph(panel_mort, max_rng_ref_bin, y_title_mort, total_counts_colname_mort, file.path("figures", "mort_max_rng_test.png"))
max_rng_age_strat <- get_age_stratified_max_rng(panel_mort, max_rng_ref_bin, y_title_mort, file.path("figures", "max_rng_age_strat.png"))
max_rng_cause <- get_cause_specific_max_rng(panel_mort, causes_mort, max_rng_ref_bin, y_title_mort, file.path("figures", "max_rng_cause_some.png"))
max_rng_cause <- get_cause_specific_max_rng(panel_mort, NULL, max_rng_ref_bin, y_title_mort, file.path("figures", "max_rng_cause_all.png"))
max_rng_combo <- get_heatwave_max_rng_graph(panel_mort, "MAX_RNG", bin, max_rng_ref_bin, total_counts_colname_mort, file.path("figures", "mort_heatwave_max_rng.csv"))
