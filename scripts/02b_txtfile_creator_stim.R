file_names  <- list.files(path= here("exp", "stim"),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file = ".") %>% 
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/exp/stim/"),
         file = str_remove(file, ".wav")) 

file_names$file <- as.character(file_names$file)

for(thisRun in 1:nrow(file_names))
{
  filename <- file_names$file[thisRun]  
  direc <- paste("exp/stim", file_names$participant[thisRun], "/", sep = "")
  end <- ".txt"
  path <- paste0(direc,filename,end)
  fileConn<-file(paste0(path))
  writeLines(file_names$file[thisRun], fileConn)
  close(fileConn)
}
