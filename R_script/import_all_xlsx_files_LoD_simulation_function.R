
# Function to import all files and assign them to named variables

import_all_xlsx_files_LoD_simulation_function <- function(directory) {
  # List of files to import
  files <- list.files(path = here(directory), pattern = "raw_data_percent_change\\.xlsx$", full.names = TRUE)
  
  # Function to read a file and assign its content to a named variable
  import_and_assign <- function(file) {
    # Read the Excel file
    data <- openxlsx::read.xlsx(file)
    
    # Name the object based on the file name without the extension
    object_name <- tools::file_path_sans_ext(basename(file))
    
    # Assign the object in the global environment
    assign(object_name, data, envir = .GlobalEnv)
  }
  
  # Apply the function to all files without printing tables in the console
  invisible(lapply(files, import_and_assign))
}
