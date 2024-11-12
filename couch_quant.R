
################################################################
installed.packages() %>% as.data.frame() %>% 
  select(Package) %>% unlist() -> local_packs

if("usethis" %in% local_packs){
  library(usethis)
}else{
  install.packages("usethis");
  library("usethis")
}
################################################################
# this creates 
usethis::use_description()
################################################################
# install.packages("roxygen2")
library(roxygen2)
roxygen2::roxygenise(getwd())


