# for installing bestsubset on the cluster

install.packages(file.path(Sys.getenv("GUROBI_HOME"),"R/gurobi_8.1-0_R_3.5.0.tar.gz"),repos = NULL)
install.packages("slam", repos = "https://cloud.r-project.org")
library(devtools)
install_github(repo="ryantibs/best-subset", subdir="bestsubset")

