read_diab_file <- function() {
  dat = read.csv("./diabetes.csv", header = TRUE)
  return(dat)
}

