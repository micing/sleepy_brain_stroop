library(tidyverse)

#### corrections ####

#id=14, file 2020-01-05 should be removed (double)
#id=19, file 2020-01-05 should be removed (double)
#id=31, file 2020-01-07 should be removed (double)

#### definitions ####

#path1 = "./data/STROOP/"
path1 = "data/new/"
path2 = "./data/task-stroop-raw_beh/"
path3 = "./data/task-stroop_beh/"

remove_files = c("SB3_014-2020-01-05-EMOTIONAL_NEUTRAL.log", 
                 "SB3_019-2020-01-05-EMOTIONAL_NEUTRAL.log", 
                 "SB3_031-2020-01-07-EMOTIONAL_NEUTRAL.log")

#### build file table to caluclate session # for each ID ####
f = list.files(path1)
files=f[!f %in% remove_files]
t=tibble(id=rep(NA,length(files)), date=NA)

for (i in 1:length(files)){
  t[i,]$id = substr(files[i], 5,7) # %>% as.numeric()
  t[i,]$date = substr(files[i], 9, 18)
}

t = t %>% group_by(id) %>% arrange(date) %>% mutate(session=row_number()) %>% ungroup() %>% arrange(id, session)

#### old filenames to proper format matching pilot filenames ####
for (f in files) {
  i=substr(f, 5,7)
  d=substr(f, 9, 18)
  session=t %>% filter(id == i & date == d) %>% select(session)
  to = paste0("sub-0", i, "_ses-", session, "_task-stroop_beh.tsv")
  file.copy(paste0(path1, f), paste0(path2, to), overwrite = FALSE)
}

#### anonymise data ####
for (f in list.files(path2)) {
  d=read_tsv(paste0(path2, f), skip=3)
  write_tsv(d, path=paste(path3, f))
}
