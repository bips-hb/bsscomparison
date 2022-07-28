Supplementary information / R code for the manuscript  
"When choosing the best subset is not the best choice"

Authors: Hanke, M., Dijkstra, L., Foraita, R. and Didelez, V. (2022)
Code was written bei Hanke, M. and Dijkstra, L.
In case of questions or comments please contact hanke@leibniz-bips.de

IMPORTANT: 

    In order to run the code, you need to source the masterscript: 
    
    > source("masterscript.R")

The code was written R, run on a Linux High Performance Cluster and used 
Gurobi Optimizer version 8.1 (linux64)

> sessionInfo()
R version 4.2.0 (2022-04-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.6

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] reshape2_1.4.4    caret_6.0-92      lattice_0.20-45   progress_1.2.2   
 [5] fitsham_0.1.0     simsham_0.1.0     bestsubset_1.0.10 batchtools_0.9.15
 [9] devtools_2.4.3    usethis_2.1.6     forcats_0.5.1     stringr_1.4.0    
[13] dplyr_1.0.9       purrr_0.3.4       readr_2.1.2       tidyr_1.2.0      
[17] tidyverse_1.3.1   tibble_3.1.7      glmnet_4.1-4      Matrix_1.4-1     
[21] ggpubr_0.4.0      ggplot2_3.3.6     crayon_1.5.1     

loaded via a namespace (and not attached):
 [1] colorspace_2.0-3     ggsignif_0.6.3       ellipsis_0.3.2       class_7.3-20        
 [5] rprojroot_2.0.3      fs_1.5.2             rstudioapi_0.13      listenv_0.8.0       
 [9] remotes_2.4.2        prodlim_2019.11.13   fansi_1.0.3          lubridate_1.8.0     
[13] xml2_1.3.3           codetools_0.2-18     splines_4.2.0        cachem_1.0.6        
[17] pkgload_1.2.4        jsonlite_1.8.0       pROC_1.18.0          broom_0.8.0         
[21] dbplyr_2.2.1         compiler_4.2.0       httr_1.4.3           backports_1.4.1     
[25] assertthat_0.2.1     fastmap_1.1.0        cli_3.3.0            prettyunits_1.1.1   
[29] tools_4.2.0          gtable_0.3.0         glue_1.6.2           rappdirs_0.3.3      
[33] Rcpp_1.0.8.3         carData_3.0-5        cellranger_1.1.0     vctrs_0.4.1         
[37] nlme_3.1-158         iterators_1.0.14     timeDate_3043.102    gower_1.0.0         
[41] globals_0.15.0       ps_1.7.1             brio_1.1.3           testthat_3.1.4      
[45] rvest_1.0.2          lifecycle_1.0.1      rstatix_0.7.0        future_1.26.1       
[49] MASS_7.3-57          scales_1.2.0         ipred_0.9-13         hms_1.1.1           
[53] parallel_4.2.0       memoise_2.0.1        rpart_4.1.16         stringi_1.7.6       
[57] desc_1.4.1           foreach_1.5.2        checkmate_2.1.0      hardhat_1.1.0       
[61] pkgbuild_1.3.1       lava_1.6.10          shape_1.4.6          rlang_1.0.2         
[65] pkgconfig_2.0.3      recipes_0.2.0        processx_3.6.1       tidyselect_1.1.2    
[69] parallelly_1.32.0    plyr_1.8.7           magrittr_2.0.3       R6_2.5.1            
[73] generics_0.1.2       base64url_1.4        DBI_1.1.3            pillar_1.7.0        
[77] haven_2.5.0          withr_2.5.0          survival_3.3-1       abind_1.4-5         
[81] nnet_7.3-17          future.apply_1.9.0   modelr_0.1.8         car_3.1-0           
[85] utf8_1.2.2           tzdb_0.3.0           grid_4.2.0           readxl_1.4.0        
[89] data.table_1.14.2    callr_3.7.0          ModelMetrics_1.2.2.2 reprex_2.0.1        
[93] digest_0.6.29        brew_1.0-7           stats4_4.2.0         munsell_0.5.0       
[97] sessioninfo_1.2.2  
A TCGA dataset is needed ofr the semi-synthetic data generation and 
is stored in the subfolder ./data. It is also available on-line at 
https://bioinformatics.mdanderson.org/Supplements/ResidualDisease/