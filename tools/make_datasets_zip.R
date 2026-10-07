## Rebuild assets/bio144_practical_datasets.zip from everything in assets/datasets/.
## Run from the root of BIO144_Practicals_WAs after adding or changing a dataset
## (e.g. each year's reaction_times_YYYY.csv), then commit and push.
files <- list.files("assets/datasets", full.names = TRUE)
files <- files[!grepl("\\.DS_Store$", files)]
zip_file <- "assets/bio144_practical_datasets.zip"
if (file.exists(zip_file)) file.remove(zip_file)
utils::zip(zip_file, files = files, flags = "-j9X")  # -j: no folder paths in the zip
message("Zipped ", length(files), " files into ", zip_file)
