source("read_diab_file.r")

get_filtered_data <- function() {
  dat <- read_diab_file()
  dat <- filter(dat, Outcome == 1)
  #dat <-  filter(dat, Glucose > 70)
  mod_dat = filter(dat, (BloodPressure  != 0) & (Glucose != 0) & (BMI != 0))
  return(mod_dat)
}

