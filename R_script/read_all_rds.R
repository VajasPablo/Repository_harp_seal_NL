
### readRDS all .rds from adapted data ###

read_all_rds <- function(){
  
  rds_files <- list.files(here("Rds_import_adapted_data"), pattern="*.rds", full.names=TRUE, ignore.case=TRUE)
  
  for (file in rds_files) {
    obj_name <- tools::file_path_sans_ext(basename(file))
    obj <- readRDS(file)
    assign(obj_name, obj, envir = .GlobalEnv)
  }
}

