library(tidyverse)

path1 = "./data/STROOP/"
path2 = "./data/task-stroop-raw_beh/"
path3 = "./data/task-stroop_beh/"

#### old pilot filenames to proper format ####
for (f in list.files(path1)) {
  id=as.numeric(substr(f,1,4)) 
  session=as.numeric(substr(f,6,6))
  to = paste0("sub-", id, "_ses-", session, "_task-stroop_beh.tsv")
  file.copy(paste0(path1, f), paste0(path2, to), overwrite = TRUE)
}

#### anonymise data ####
for (f in list.files(path2)) {
  d=read_tsv(paste0(path2, f), skip=3)
  write_tsv(d, path=paste(path3, f))
}
