Supplementary information / R code for the manuscript  
"When choosing the best subset is not the best choice"

Authors: Hanke, M., Dijkstra, L., Foraita, R. and Didelez, V. (2022)
Code was written bei Hanke, M. and Dijkstra, L.
In case of questions or comments please contact hanke@leibniz-bips.de

IMPORTANT: 

    In order to run the code, you need to source the masterscript: 
    
    > source("masterscript.R")

A TCGA dataset is needed for the semi-synthetic data generation and 
is stored in the subfolder ./data. It is also available online at 
https://bioinformatics.mdanderson.org/Supplements/ResidualDisease/


The code was written R, run on a Linux High Performance Cluster and used 
Gurobi Optimizer version 8.1 (linux64) which is mandatory to run the 
simulation study including best subset selection. However, we implemented 
also examples to re-run the code without best subset selection (the 
masterscript.R will ask what kind of simulation to re-run).


The following R output shows the session Info on our cluster: 

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
 [1] LC_CTYPE=de_DE.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=de_DE.UTF-8    
 [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=de_DE.UTF-8   
 [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] caret_6.0-92      lattice_0.20-45   simsham_0.1.0     batchtools_0.9.15
 [5] mvtnorm_1.1-3     forcats_0.5.1     stringr_1.4.0     dplyr_1.0.9      
 [9] purrr_0.3.4       readr_2.1.2       tidyr_1.2.0       tidyverse_1.3.1  
[13] tibble_3.1.7      bestsubset_1.0.10 ggplot2_3.3.6     glmnet_4.0-2     
[17] Matrix_1.4-1      Rmpi_0.6-9.2      snow_0.4-4       

loaded via a namespace (and not attached):
 [1] nlme_3.1-158         fs_1.5.2             lubridate_1.8.0     
 [4] progress_1.2.2       httr_1.4.3           tools_4.0.2         
 [7] backports_1.4.1      utf8_1.2.2           R6_2.5.1            
[10] rpart_4.1.16         DBI_1.1.3            colorspace_2.0-3    
[13] nnet_7.3-17          withr_2.5.0          tidyselect_1.1.2    
[16] prettyunits_1.1.1    compiler_4.0.2       cli_3.3.0           
[19] rvest_1.0.2          xml2_1.3.3           scales_1.2.0        
[22] checkmate_2.1.0      rappdirs_0.3.3       digest_0.6.29       
[25] pkgconfig_2.0.3      parallelly_1.32.0    dbplyr_2.2.1        
[28] rlang_1.0.3          readxl_1.4.0         rstudioapi_0.13     
[31] shape_1.4.6          generics_0.1.2       jsonlite_1.8.0      
[34] ModelMetrics_1.2.2.2 magrittr_2.0.3       Rcpp_1.0.8.3        
[37] munsell_0.5.0        fansi_1.0.3          lifecycle_1.0.1     
[40] pROC_1.18.0          stringi_1.7.6        MASS_7.3-57         
[43] plyr_1.8.7           recipes_0.2.0        grid_4.0.2          
[46] listenv_0.8.0        crayon_1.5.1         haven_2.5.0         
[49] splines_4.0.2        hms_1.1.1            pillar_1.7.0        
[52] base64url_1.4        stats4_4.0.2         reshape2_1.4.4      
[55] future.apply_1.9.0   codetools_0.2-18     reprex_2.0.1        
[58] glue_1.6.2           data.table_1.14.2    modelr_0.1.8        
[61] vctrs_0.4.1          tzdb_0.3.0           foreach_1.5.2       
[64] cellranger_1.1.0     gtable_0.3.0         future_1.26.1       
[67] assertthat_0.2.1     gower_1.0.0          prodlim_2019.11.13  
[70] broom_0.8.0          class_7.3-20         survival_3.3-1      
[73] timeDate_3043.102    iterators_1.0.14     hardhat_1.1.0       
[76] lava_1.6.10          globals_0.15.1       ellipsis_0.3.2      
[79] brew_1.0-7           ipred_0.9-13