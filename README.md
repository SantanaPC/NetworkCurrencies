# General overview

This repository contains the data and code used in the analysis of the
manuscript entitled **“Fine-tuning the buzz: comparing visitation frequency and pollination effectiveness in plant-pollinator networks”** accepted for publication in New Phytologist.

In this study, we evaluate how the currencies used to build ecological networks may affect our understanding of the processes that shape community dynamics. We focus on pollination and use patterns of pollen deposition and pollen export across different species of _Chamaecrista_ to compare quantitative (visitation frequency) and qualitative (pollen removal and deposition) metrics. By combining these two metrics, we discuss how functional aspects of interactions may influence our perception of network structure and stability.


# Repository structure

## 1.RawData

This folder stores raw and processed data used to perform all the
analysis presented in this study

## 2.Scripts

This folder contains all the code used to perform data processing,
analysis and visualization of results. The code is written in R language.
The script sequence to reproduce the workflow is indicated by the numbers at
the beginning of the name of the script file

- `0_data_summary.Rmd` initial data exploration

- `1_analysis_Demo.Rmd` analysis to investigate the patterns of pollen deposition by distinct pollinators on different _Chamaecrista_ species.

- `2_analysis_Expo.Rmd` analysis to investigate the patterns of pollen exportation (how many pollen grains were taken from the anthers after a visit) by distinct pollinators on different _Chamaecrista_ species.

- `3_analysis_DepoExpoRation.Rmd` comparison of the relation between pollen deposition and pollen exportation for the pairwise interactions between pollinators and _Chamaecrista_ species. Pollinators were classified into functional groups to facilitate interpretation.

- `4_networks.R` code to generate the networks based on visitation frequency, pollen deposition and pollen exportation.

- `5_cz_species_role.R` code used to compare species role in the distinct networks using C&Z metrics.

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
