# - create a template

create("C:/Users/u1211862/OneDrive/App/R/utilr")

# - put all .R files into the R folder

# - generate document 

setwd(stringr::str_c(Sys.getenv("OneDrive"), "\\App\\R"))
devtools::document('utilr')

# - push to github!

# install your package from github!
devtools::install_github('XiaomoWu/utilr')

