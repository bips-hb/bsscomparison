# short script to rename files according to the figure numbers in the paper

# chanhge to plots folder
setwd("./plots")

# Files to rename
filenames.from <- c("Resultshigh_block_spread_35.png",
                    "Resultshigh_toeplitz_first_70.png",
                    "Resultslow_block_spread_70.png",
                    "Resultslow_block_first_70.png",
                    "Results_high_semisyn.png",
                    "Results_low_semisyn.png",
                    "Corr_and_Dim.png",
                    "BestCriterion_block_high_0.7.png",
                    "Value_vs_k.tiff",
                    "Time_comparison_blocklowfirst70_snr042.png")

# New file names
filenames.to <- c("Figure_02.png",
                    "Figure_03.png",
                    "Figure_04.png",
                    "Figure_05.png",
                    "Figure_06.png",
                    "Figure_07.png",
                    "Figure_08.png",
                    "Figure_09.png",
                    "Figure_10.png",
                    "Figure_11.png")

file.rename(filenames.from, filenames.to)

# set wd back
setwd("..")
