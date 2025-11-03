# General overview

This repository contains the data and code used in the analysis of the
manuscript entitled **“Fine-tuning the buzz: comparing visitation frequency and pollination effectiveness in plant-pollinator networks”** accepted for publication in New Phytologist.

In this study, we evaluate how the currencies used to build ecological networks may affect our understanding of the processes that shape community dynamics. We focus on pollination and use patterns of pollen deposition and pollen export across different species of _Chamaecrista_ to compare quantitative (visitation frequency) and qualitative (pollen removal and deposition) metrics. By combining these two metrics, we discuss how functional aspects of interactions may influence our perception of network structure and stability.


# Repository structure

## 1.RawData

This folder stores raw and processed data used to perform all the
analysis presented in this study.

- `dados_obs_group.csv` a data frame containing the `id_exp` id of the pollinator; `id_flow` id of the plant species; `polen_expor` the total amount of pollen exportation; `polen_depo` the total of pollen deposition; `bee_sp` the pollinator species; `ch_sp` the plant species; `func_group` the functional group of the pollinator.

- `rede_frequencia.csv` a data frame containing all species and the frequency of visits of each pollinator species to each plant species.

- `rede_deposicao.csv` a data frame containing all species and the total amount of pollen deposited on the stigma by each pollinator species to each plant species.

- `rede_remocao.txt` a data frame containing all species and the total amount of pollen removed from the anthers by each pollinator species after a visit to a plant species.

- `rede_eficacia_f.csv` a data frame containing all species and the total amount of pollen deposited multiplied by the frequency of each pollinator visiting each plant species.

- `rede_eficacia_m.csv` a data frame containing all species and the total amount of pollen exported multiplied by the frequency of each pollinator visiting each plant species.

- `z_score_bee.csv` a data frame containing z score for bees in all networks

- `z_score_plant.csv` a data frame containing z score for plants in all networks


## 2.Scripts

This folder contains all the code used to perform data processing,
analysis and visualization of results. The code is written in R language.
The script sequence to reproduce the workflow is indicated by the numbers at
the beginning of the name of the script file

- `1_analysis_Deposition.Rmd` analysis to investigate the patterns of pollen deposition by distinct pollinators on different _Chamaecrista_ species, grouped by functional roles. Then, plot the network using only pollen deposition as interaction currency.

- `2_analysis_Exportation.Rmd` analysis to investigate the patterns of pollen exportation (how many pollen grains were taken from the anthers after a visit) by distinct pollinators on different _Chamaecrista_ species, grouped by functional roles. Then, plot the network using only pollen export as interaction currency.

- `3_analysis_DepoExpoRation.qmd` comparison of the relation between pollen deposition and pollen exportation for the pairwise interactions between pollinators and _Chamaecrista_ species. Pollinators were classified into functional groups to facilitate interpretation.

- `4_networks.R` code to generate the networks based on visitation frequency, pollen deposition, female perfomance (pollen deposition multiplied by frequency), pollen exportation and male perfomance (pollen export multiplied by frequency). In this script, we also calculated network metrics to compare the structure of distinct networks.

- `5_cz_species_role.R` code used to compare species role in the distinct networks using C&Z metrics.

- `6_New_analysis_030425_review.R` this code contains all the analysis, output tables and networks used in the paper.

### Figures

In this folder you will find all figures generated to produce this study.

# Authors

Lorena Bueno Valadão-Mendes, Pamela Cristina Santana, André Rodrigo Rech,
Vinícius Lourenço Garcia Brito, Pietro Kiyoshi Maruyama

# Contact

[Pamela Santana](https://github.com/PSantana) and [Lorena Bueno Valadão-Mendes](lorena.bvmendes@gmail.com)

# Session info

    R version 4.4.2 (2024-10-31)
    Platform: aarch64-apple-darwin20 (64-bit)
    Running under: macOS Sequoia 15.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Stockholm
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] scales_1.3.0         tidygraph_1.3.1      ggraph_2.2.1        
 [4] future.apply_1.20.0  future_1.58.0        RColorBrewer_1.1-3  
 [7] ggrepel_0.9.6        ggeffects_1.6.0      cowplot_1.1.3       
[10] glmmTMB_1.1.9        MuMIn_1.47.5         DHARMa_0.4.6        
[13] lme4_1.1-35.3        Matrix_1.7-1         igraph_2.1.4        
[16] iNEXT_3.0.1          bipartite_2.20       sna_2.7-2           
[19] network_1.18.2       statnet.common_4.9.0 vegan_2.6-6.1       
[22] lattice_0.22-6       permute_0.9-7        janitor_2.2.0       
[25] patchwork_1.2.0      lubridate_1.9.3      forcats_1.0.0       
[28] stringr_1.5.1        dplyr_1.1.4          purrr_1.0.2         
[31] readr_2.1.5          tidyr_1.3.1          tibble_3.2.1        
[34] ggplot2_3.5.1        tidyverse_2.0.0      knitr_1.48          
[37] devtools_2.4.5       usethis_2.2.3       

loaded via a namespace (and not attached):
  [1] rstudioapi_0.16.0   jsonlite_1.8.8      datawizard_0.11.0  
  [4] magrittr_2.0.3      estimability_1.5.1  farver_2.1.2       
  [7] nloptr_2.0.3        rmarkdown_2.27      fs_1.6.4           
 [10] fields_16.2         vctrs_0.6.5         memoise_2.0.1      
 [13] minqa_1.2.7         base64enc_0.1-3     htmltools_0.5.8.1  
 [16] haven_2.5.4         parallelly_1.45.0   KernSmooth_2.23-24 
 [19] htmlwidgets_1.6.4   plyr_1.8.9          emmeans_1.10.2     
 [22] cachem_1.1.0        TMB_1.9.11          mime_0.12          
 [25] lifecycle_1.0.4     iterators_1.0.14    pkgconfig_2.0.3    
 [28] sjlabelled_1.2.0    gap_1.5-3           R6_2.5.1           
 [31] fastmap_1.2.0       rbibutils_2.2.16    shiny_1.8.1.1      
 [34] snakecase_0.11.1    digest_0.6.37       numDeriv_2016.8-1.1
 [37] colorspace_2.1-1    pkgload_1.3.4       qgam_1.3.4         
 [40] labeling_0.4.3      fansi_1.0.6         timechange_0.3.0   
 [43] polyclip_1.10-7     mgcv_1.9-1          compiler_4.4.2     
 [46] proxy_0.4-27        remotes_2.5.0       bit64_4.0.5        
 [49] withr_3.0.1         doParallel_1.0.17   viridis_0.6.5      
 [52] pkgbuild_1.4.4      ggforce_0.4.2       maps_3.4.2         
 [55] MASS_7.3-61         sessioninfo_1.2.2   classInt_0.4-10    
 [58] tools_4.4.2         httpuv_1.6.15       glue_1.8.0         
 [61] nlme_3.1-166        promises_1.3.0      grid_4.4.2         
 [64] cluster_2.1.6       reshape2_1.4.4      generics_0.1.3     
 [67] isoband_0.2.7       gtable_0.3.5        tzdb_0.4.0         
 [70] class_7.3-22        hms_1.1.3           utf8_1.2.4         
 [73] foreach_1.5.2       pillar_1.9.0        spam_2.10-0        
 [76] vroom_1.6.5         later_1.3.2         splines_4.4.2      
 [79] tweenr_2.0.3        bit_4.0.5           tidyselect_1.2.1   
 [82] miniUI_0.1.1.1      gridExtra_2.3       stats4_4.4.2       
 [85] xfun_0.46           graphlayouts_1.2.2  skimr_2.1.5        
 [88] stringi_1.8.7       yaml_2.3.10         boot_1.3-31        
 [91] evaluate_0.24.0     codetools_0.2-20    bbmle_1.0.25.1     
 [94] cli_3.6.5           xtable_1.8-4        Rdpack_2.6         
 [97] repr_1.1.7          munsell_0.5.1       Rcpp_1.1.0         
[100] globals_0.18.0      coda_0.19-4.1       bdsmatrix_1.3-7    
[103] parallel_4.4.2      ellipsis_0.3.2      dotCall64_1.1-1    
[106] gap.datasets_0.0.6  profvis_0.3.8       urlchecker_1.0.1   
[109] listenv_0.9.1       viridisLite_0.4.2   mvtnorm_1.2-5      
[112] e1071_1.7-16        insight_0.20.4      crayon_1.5.2       
[115] rlang_1.1.6        
> 