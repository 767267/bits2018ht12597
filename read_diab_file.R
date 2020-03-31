read_diab_file <- function() {
  setwd("D:/BITS/svn")
  path = getwd()
  #print(path)
  dat = read.csv("./diabetes.csv", header = TRUE)
  return(dat)
}

