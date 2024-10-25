message("Getting temps and bins (avg, diurnal range, heatwave counts, maximum temp-diurnal range)...")
source(file.path("scripts", "00-get_avg_temps.R"))
source(file.path("scripts", "01-daily_temp_bins.R"))
source(file.path("scripts", "02-get_diurnal_temp_range.R"))
source(file.path("scripts", "03-diurnal_range_bins.R"))
source(file.path("scripts", "04-heatwave_day_counts.R"))
source(file.path("scripts", "05-max_temp_diurnal_rng_interaction.R"))
source(file.path("scripts", "06-max_temp_diurnal_rng_bins.R"))

message("Collapsing measures...") 
source(file.path("scripts", "09-collapse_measures.R")) # runs 07 and 08 files inside

message("Getting health data (mortality + morbidity)...")
source(file.path("scripts", "10-er_health_data.ipynb"))
source(file.path("scripts", "11-mortality_health_data.ipynb"))
source(file.path("scripts", "12-catchment_er.R"))
source(file.path("scripts", "13-catchment_mortality.R"))
source(file.path("scripts", "14-zipcode_pca_data.ipynb"))
source(file.path("scripts", "15-zipcode_pca.ipynb"))
source(file.path("scripts", "16-merge_data.ipynb"))

message("Running regression and getting figures...")
source(file.path("scripts", "18-er_regression.R")) # runs 17 inside
source(file.path("scripts", "19-mortality_regression.R")) # runs 17 inside
