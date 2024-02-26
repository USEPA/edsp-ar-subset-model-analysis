# edsp-ar-subset-model-analysis
This repository contains the code and data for the analysis present in the manuscript "Optimizing androgen receptor prioritization using high-throughput assay-based activity models" by R.J. Bever et al. (2024). 

This workflow requires a local installation of the invitroDB (v3.3) database from the US EPA's ToxCast program. Installation instructions are here: https://www.epa.gov/chemical-research/exploring-toxcast-data-how-set-r-and-mysql.
This workflow also requires the results from the R package "ARminassaymodel" that accompanies the Judson et al. (2020) paper (10.1016/j.yrtph.2020.104764). The package can be downloaded from EPA's FTP site:
ftp://anonymous@newftp.epa.gov/COMPTOX/STAFF/rjudson/publications/Judson%20AR%202019/ARmodelForPublication.zip

# Workflow
There are three main scripts to run for this analysis:
    1. 02_Model_performance_v4_1.Rmd,
    2. R/ar_model_radial_plots.R, and
    3. R/create_figure_5_compara_vs_ar_model_results.R
    
The files required to run these scripts not created here are in the `inputs` folder.

## Disclaimer 
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 