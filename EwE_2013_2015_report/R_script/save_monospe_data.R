

save_monospe_data <- function(file_name, file_object) {  
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  
  file_name_here <- here("Rds_export_adapted_data", paste0(file_name, "_", project_name, ".rds"))
  saveRDS(file_object, file = file_name_here)
}
