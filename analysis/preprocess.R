# preprocess data from the cit replication study, supervised by Robbert van der Mijn

# load package written by Robbert (pupilMineR) with functions for preprocessing
# install using devtools [install. packages("devtools")] -> [devtools::install_github("robbertmijn/pupilMiner")]

library(pupilMiner)

# specify where data lives
data_dir <- "../data/*.asc"

# List datafiles in a vector
files <- Sys.glob(data_dir)

# Specify which variables created from the OS-experiment we want to retain
keep_vars <- c("subject_nr", "T1", "T1pos", "T2", "T1_correct", "T2_correct",
               "practice", "sum_points", "score")
dat <- NULL
for(file in files){
  cat("Begin processing", file, "\n")
  # Parse datafile to temporary data table. See pupilMiner documentation for info about parse function
  tempdat <- parse_asc_file(file,
                            keep_vars = keep_vars,
                            timelock_to = "rsvp", # reset time markers to 0 at start of each rsvp
                            samptime = 20 # downsample to 50hz (20ms between samples)
  ) 
  
  # convert variables to appropriate format
  tempdat[, ":="(subject_nr = as.factor(subject_nr),
                 lie = ifelse(as.numeric(subject_nr) %% 2 == 0, "truth", "lie"), # odd pps lie, even don't
                 trial = trial - 10, # start counting trials after the 10 practice trials
                 sum_points = as.numeric(sum_points),
                 score = as.numeric(score),
                 time_T1 = time - as.numeric(T1pos) * 100, # timelock to T1 presentation
                 T1 = as.factor(T1),
                 T2 = as.factor(T2),
                 T1_correct = as.integer(T1_correct),
                 T2_correct = as.integer(T2_correct)
                 )]
  
  # Keep only relevant time period around rsvp
  tempdat <- tempdat[time %between% c(-200, 2000) & trial > 0]
  
  # Custom baselining procedure
  bldat <- NULL
  for(BL in unique(tempdat$T1pos)){
    bldat <- rbind(bldat, baseline(tempdat[T1pos == BL], c(-100, 0) + as.numeric(BL) * 100))
  }
  setkey(bldat, trial)
  tempdat <- bldat
  
  # Combine data of this pp to full dataset
  cat("Adding", file, "to datatable\n")
  dat <- rbind(dat, tempdat)
  
  # Erase temporary data
  tempdat <- NULL
}

# Save the dataset to an rdata file
save(dat, file = paste0(format(Sys.time(), "%Y%m%d%H%M%S_"), length(unique(dat$subject_nr)), "pps_cit_bch_dec21_targetBL.rdata"))

# Uncomment following lines to export individual traces for visual inspection
# for(s in unique(dat$subject_nr)){
#   cat("reporting for subject_nr", s, "\n")
#   trace_reports(dat[subject_nr == s & trial > 0])
# }

