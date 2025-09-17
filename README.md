# A methodological assessment of confounding in studies of long-term risks of air pollution

An theoretical and empirical evaluation of confounding mechanisms in the analysis of the health effects associated with long-term exposure to air pollution, with a case-study example using the UK Biobank cohort

------------------------------------------------------------------------

This repository stores the updated R code to reproduce the analysis presented in the article:

Vanoli J, et al. Confounding issues in air pollution epidemiology: an empirical assessment with the UK Biobank cohort. *International Journal of Epidemiology*. 2025;54(5):dyaf163. DOI: 10.1093/ije/dyaf163 [[freely available here](http://www.ag-myresearch.com/2025_vanoli_IJE.html)]

The work was supported by the Medical Research Council-UK (Grant ID: [MR/Y003330/1](https://gtr.ukri.org/projects?ref=MR%2FY003330%2F1)).

### R scripts

The repo provides a series of R scripts that can be used to replicate the analysis described in the article. The main scripts included in the main folder *Rcode* can be used as a tutorial, and the user can run them to replicate all the steps of the full results of the analysis using synthetic data from a public repository. The original scripts used for the analysis are included in sub-folder *origcode*.

### Data

The original data from the UKB cohort is available upon request (see [www.ukbiobank.ac.uk](https://www.ukbiobank.ac.uk)) but cannot be shared. For reproducibility purposes, a series of synthetic datasets are stored in Zenodo repository (<https://zenodo.org/records/13983170>), from which they can be downloaded manually or automatically using the R scripts. The datasets resemble the real data used in the analysis, and they were generated using the R package `synthpop` ([www.synthpop.org.uk](www.synthpop.org.uk)). Details on the data synthesis and codebooks for the datasets are provided in the repository.

The series of synthetic datasets are the following:

-   `synthbdcohortinfo`: basic cohort information regarding the follow-up period and birth/death dates for 502,360 participants.
-   `synthbdbasevar`: baseline variables, mostly collected at recruitment.
-   `synthpmdata`: annual average exposure to PM2.5 for each participant reconstructed using their residential history.
-   `synthoutdeath`: death records that occurred during the follow-up with date and ICD-10 code.

**Note**: the synthetic versions of the datasets resemble the real ones in several aspects, including the distribution and cross-correlation of variables as well as some of the effect associations. These can be used to replicate the analysis using the scripts provided in this repo, as well as for illustrative purposes in other tutorials. However, users are warned that **these datasets are fake** and must not be used for making inferences and addressing research hypotheses.
