### Copy the uploaded files to a new directory in order to change name and to 
### make auto-segmentation easier

library(here)

dat.files  <- list.files(path= here("data", "mono_uploads"),
                         recursive=T,
                         pattern=".wav"
                         ,full.names=T)





from.dir <- here("data", "mono_uploads")
to.dir   <- here("data", "all_uploads_mono")
files    <- list.files(path = from.dir, full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)




