# =====================================================
# Devtools workflow
install.packages("devtools")
library(devtools)
# =====================================================

devtools::install_github("richardjmin/roller", build_vignettes = TRUE)
devtools::document()          # generate documentation
devtools::check_man()         # check documentation
devtools::test()              # run tests
devtools::build_vignettes()   # build vignettes
devtools::build()             # build bundle
devtools::install()           # install package

