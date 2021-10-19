library(dplyr)
library(here)

# this script was used to add the participant id to the audio file name 

loop_list <- list.files(here("data", "mono_uploads")) %>% 
  as.data.frame()


for (iteration in 1:nrow(loop_list)) {
  
  id <- loop_list$.[iteration]
  
  my_path <- here("data", "mono_uploads", paste(id)) # Define working directory
  
  file_names_old <- list.files(my_path) # get directory names/participant numbers 
  
  file_names_new <- matrix(nrow = length(file_names_old))
  
  for (thisRun in 1:length(file_names_old)) {
    file_names_new[thisRun] <- paste(id, file_names_old[thisRun], sep = "_")
  }
  
  file.rename(paste(my_path, file_names_old, sep = "/"),       # Rename files
              paste(my_path, file_names_new, sep = "/"))
  
}
