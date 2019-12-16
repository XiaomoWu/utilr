# - create a template

# create("C:/Users/u1211862/OneDrive/App/R/utilr")

# - put all .R files into the R folder

# - generate document 



setwd(stringr::str_c('/mnt/c/Users/rossz/Onedrive/app/r/'))

# setwd(stringr::str_c(Sys.getenv("OneDrive"), "\\App\\R"))
devtools::document('utilr')

# - install the lib!

devtools::install('utilr')

# - push to github!
# - Actually, you don't need to push to Github to have `utilr` available across machines. That's because `utilr` has already been synced via OneDrive. So for every new machine, sign in your Onedrive, sync the `utilr` folder, enter it, and run `devtools::install('utilr')`. There you go!

# install your package from github!
devtools::install_github('XiaomoWu/utilr')

