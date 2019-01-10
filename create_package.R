library(devtools)
library(roxygen2)

# create a template
#create("C:/Users/u1211862/OneDrive/App/R/utilr")

# put all .R files into the R folder

# generate document 

setwd(str_c(Sys.getenv("OneDrive"), "\\App\\R"))
document('utilr')

# push to github!

# install your package from github!
install_github('XiaomoWu/utilr')

# test
sv(dt)
ld(dt)
ld(dt, T)

