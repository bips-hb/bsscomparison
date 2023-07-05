Supplementary information / R code for the manuscript  
"When choosing the best subset is not the best choice"

Authors: Hanke, M., Dijkstra, L., Foraita, R. and Didelez, V. (2023)
Code was written bei Hanke, M. and Dijkstra, L.
In case of questions or comments please contact hanke@leibniz-bips.de


OVERVIEW/INSTRUCTIONS: 

    In order to run the code, you need to set the working directory to 
    the folder of the lokal copy of this repository and source the 
    masterscript.R: 
    
    > source("masterscript.R")

    The master script calls two types of scripts: 
    1. interactive scripts askin which simulations to run
    2. the actual scripts to run the selected simulations.
    You can also run each of the scripts manually via the master script. 
    Since our simulation study is computationally challenging we also 
    give the user the option either to run examples of the simulation 
    study (including an comparison of the results to the original ones) 
    or to generate just the plots based on the raw results provided by us
    (see below for the links).

    The master script contains 8+1 chapters:
    
    0.: Set-Up
    A script which asks the user to install the necessary packages. 
    If you run into trouble installing the packages via this script please 
    install them manually. See our session info at the end of this README.

    I.: Run the simulation for the semi-synthetic setting 

    II.: Run the simulation for the synthetic setting

    III.: Apply Best Subset Selection with different time limits

    IV.: Generate all plots for the results of Chapter I and II (Semi-
    synthetic and synthetic settings, different subset sizes and 
    correlation vs dimensionality)
    (NOTE: you can run this script without running Chapter I & II
    but in this case you will need to download the raw results and save 
    them under "./results". See below for the links.)

    V.: Generate all plots for the results of Chapter III (time limits)
    (NOTE: you can run this script without running Chapter III based
    on the raw results in ./results. No extra download needed)

    VI.: Simulation for Stability Selection and BIC, mBIC2 and HQC
    This is omitted by default because of its long running time. You 
    have to change "runCriteriaSimu <- FALSE" to 
    "runCriteriaSimu <- TRUE" in the master script to run these simulations. 
    Please see also the default parameter values in the corresponding 
    chapter (they correspond to Figure 9 in the paper). 

    VII.: Plot the results of Stability Selection, BIC, mBIC2 and HQC 
    The default setting is a high-dimensional block structure with 
    correlation rho = 0.7 and consecutive ("adjacent") non-zero betas.
    These values correspond to Figure 9 in the paper. 

    VIII.: Rename plots
    A short script to rename the previous generated plots according to 
    the figures of the paper.


PLOTS/FIGURES: 

    This repository generates all plots of our simulation study and saves
    them under "./plots".
    The following table gives the name of the plots with their corresponding
    figure numbers according to their appearance in our paper. Further, the 
    table contains the chapter number of the master script to generate the 
    plots.

    To compare these plots to the plots of the paper the master script
    calls a script to rename them as "Figure_02", "Figure_03", etc. 
    If you do not want to rename the plots please delete the 
    "source(rename_plots.R)" line at the end of the master script (see 
    Chapter VIII).

    Note: Figure 1 in the paper is just a schematic representation of the 
    different correlation structures and the positioning of the direct 
    predictors. Hence, we do not provide any code for generating this figure.

    -------------------------------------------------------------------------|
    |        Name of generated plot                |  Figure No. |  Chapter  |
    =========================================================================|
    |                                              |             |           |
    |  Resultshigh_block_spread_35.png             |      2      |    IV     |
    |                                              |             |           |
    |  Resultshigh_toeplitz_first_70.png           |      3      |    IV     |
    |                                              |             |           |
    |  Resultslow_block_spread_70.png              |      4      |    IV     |
    |                                              |             |           |
    |  Resultslow_block_first_70.png               |      5      |    IV     |
    |                                              |             |           |
    |  Results_high_semisyn.png                    |      6      |    IV     |
    |                                              |             |           |
    |  Results_low_semisyn.png                     |      7      |    IV     |
    |                                              |             |           |
    |  Corr_and_Dim.png                            |      8      |    IV     |
    |                                              |             |           |
    |  BestCriterion_block_high_0.7.png            |      9      |    VII    |
    |                                              |             |           |
    |  Value_vs_k.png                              |     10      |    IV     |
    |                                              |             |           |
    |  Time_comparison_blocklowfirst70_snr042.png  |     11      |     V     |
    --------------------------------------------------------------------------


DATASETS & AVAILABILITY:

    A TCGA dataset is needed for the semi-synthetic data generation and 
    is stored in the subfolder ./data. It is also available online at 
    https://bioinformatics.mdanderson.org/Supplements/ResidualDisease/

    If you do not want to re-run the Stability Selection and BIC/mBIC2/HQC 
    simulation our raw results are stored in ./results to plot them.

    If you do not want to re-run the time-limit simulation you find the 
    raw results under ./results.

    If you do not want to re-run the simulation for the synthetic data you
    can download the result for the medium- and high-dimensional settings
    under https://figshare.com/articles/dataset/Simualtion_Results/23578647
    or alternatively under 
    https://www.bips-institut.de/fileadmin/downloads/BestSubsetResults.zip


FURTHER INFORMATION:

    The code was written R, run on a Linux High Performance Cluster and used 
    Gurobi Optimizer version 8.1 (linux64) which is mandatory to run the 
    simulation study including best subset selection. However, we implemented 
    also examples to re-run the code without best subset selection (the 
    masterscript.R will ask what kind of simulation to re-run. See above.).

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
