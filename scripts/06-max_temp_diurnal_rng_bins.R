# load libraries
library(terra)

# consts
years <- c(2003:2020)
bins <- c(-Inf, 0:5, Inf)
bin_labels <- c("Inf_0", paste0(seq(0, 4), "_", seq(1, 5)), "5_Inf")

base_paths <- list(
  max_rng = file.path("processed", "la_max_rng"),
  output = file.path("processed", "max_rng_bins")
)

# create list of vectors of max_rng paths for each year
yearly_data <- list() 
for (year in years) {
  # get input data
  yearly_data[[as.character(year)]] <- c(
    file.path(base_paths[["max_rng"]], paste("LA_MAX_RNG", year, sep="_"))
  )
}

# create dir for output data
if (!dir.exists(base_paths[["output"]])) {
  dir.create(base_paths[["output"]], recursive=TRUE)
}

# sort vector of pixel values into (-Inf, 0), [0, 1), [1, 2), ... bins
get_bins <- function(pixel_vec) {
  bins <- cut(pixel_vec, breaks=bins, include.lowest=TRUE, right=FALSE) 
  bins <- table(bins, useNA="no") # must get rid of NAs
  return(bins)
}

# get and save raster brick for all temperature paths and years, where each 
# layer of the raster brick represents a different bin and where each pixel 
# value represents the number of days in that bin at that location
for (year in names(yearly_data)) {
  year_path <- yearly_data[[year]]
  files <- list.files(year_path, full.names = TRUE)
  raster_stack <- rast(files)
  raster_basename <- basename(year_path)
  raster_path <- file.path(base_paths[["output"]], paste0(raster_basename, ".tif"))
  
  print(paste0("Getting binned raster for ", year_path, "..."))
  # apply fun on vector of pixel vals across the raster stack; save output as raster brick
  binned_raster <- app(raster_stack, fun = get_bins)
  
  # name layers for visualizations; better to use terra for naming layers
  # (in raster, layer names stay as default when loading the file later on)
  names(binned_raster) <- paste(raster_basename, bin_labels, sep = "_")
  
  # save raster brick
  writeRaster(binned_raster, raster_path, overwrite = TRUE)
}
