Supplementary information / R code for the manuscript  
"When choosing the best subset is not the best choice"

Authors: Hanke, M., Dijkstra, L., Foraita, R. and Didelez, V. (2022)
Code was written bei Hanke, M. and Dijkstra, L.
In case of questions or comments please contact hanke@leibniz-bips.de

The code was written R, run on a Linux High Performance Cluster and used 
Gurobi Optimizer version 8.1 (linux64)

> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS:   /home/local/R/4.0.2/lib64/R/lib/libRblas.so
LAPACK: /home/local/R/4.0.2/lib64/R/lib/libRlapack.so

Random number generation:
 RNG:     Mersenne-Twister 
 Normal:  Inversion 
 Sample:  Rounding 
 
locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] fitsham_0.1.0     simsham_0.1.0     forcats_0.5.1     stringr_1.4.0    
 [5] dplyr_1.0.7       purrr_0.3.4       readr_2.1.1       tidyr_1.1.4      
 [9] tidyverse_1.3.1   tibble_3.1.6      bestsubset_1.0.10 ggplot2_3.3.5    
[13] glmnet_4.0-2      Matrix_1.4-0      Rmpi_0.6-9.2      snow_0.4-4       

loaded via a namespace (and not attached):
 [1] httr_1.4.2        pkgload_1.2.4     jsonlite_1.7.3    splines_4.0.2    
 [5] foreach_1.5.2     modelr_0.1.8      assertthat_0.2.1  cellranger_1.1.0 
 [9] remotes_2.4.2     sessioninfo_1.2.2 pillar_1.6.4      backports_1.4.1  
[13] lattice_0.20-45   glue_1.6.0        rvest_1.0.2       colorspace_2.0-2 
[17] pkgconfig_2.0.3   devtools_2.4.3    broom_0.7.10      haven_2.4.3      
[21] scales_1.1.1      processx_3.5.2    tzdb_0.2.0        generics_0.1.1   
[25] usethis_2.1.5     ellipsis_0.3.2    cachem_1.0.6      withr_2.4.3      
[29] cli_3.1.0         survival_3.2-13   magrittr_2.0.1    crayon_1.4.2     
[33] readxl_1.3.1      memoise_2.0.1     ps_1.6.0          fs_1.5.2         
[37] fansi_0.5.0       xml2_1.3.3        pkgbuild_1.3.1    tools_4.0.2      
[41] prettyunits_1.1.1 hms_1.1.1         lifecycle_1.0.1   munsell_0.5.0    
[45] reprex_2.0.1      callr_3.7.0       compiler_4.0.2    rlang_0.4.12     
[49] grid_4.0.2        iterators_1.0.14  rstudioapi_0.13   testthat_3.1.1   
[53] gtable_0.3.0      codetools_0.2-18  curl_4.3.2        DBI_1.1.2        
[57] R6_2.5.1          lubridate_1.8.0   fastmap_1.1.0     utf8_1.2.2       
[61] rprojroot_2.0.2   shape_1.4.6       desc_1.4.0        stringi_1.7.6    
[65] Rcpp_1.0.8.3      vctrs_0.3.8       dbplyr_2.1.1      tidyselect_1.1.1 

A TCGA dataset is needed ofr the semi-synthetic data generation and 
is stored in the subfolder ./data. It is also available on-line at 
https://bioinformatics.mdanderson.org/Supplements/ResidualDisease/