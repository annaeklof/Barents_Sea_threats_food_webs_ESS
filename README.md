# Tracing anthropogenic threats through food webs reveals their total impact on ecosystem services

Data and code for the manuscript

> Eklöf A., Sollén Mattsson J. & Barabás G. *Tracing anthropogenic threats
> through food webs reveals their total impact on ecosystem services.*
> Submitted to *Nature Ecology & Evolution*.

The analysis follows eight anthropogenic threats from the species they directly
affect, through the food webs of 25 subregions of the Barents Sea, to the
species that provide four ecosystem services (Carbon sequestration, Habitat
provisioning, Food provisioning, Recreation and tourism). Extinction risk is
propagated through each food web with a Bayesian network model, and every
analysis is run both with and without species interactions, so that the
service loss caused directly by a threat can be separated from the loss that
arises through the food web.

An interactive visualization of the food web, the threats and the services is
available at
[annaeklof/visualization_barents_sea_foodweb](https://github.com/annaeklof/visualization_barents_sea_foodweb).


## Quick start

All simulation results are included, so the figures can be made without
running the simulations.

1. Install R (the results were produced with R 4.3.3) and the packages listed
   below.
2. Set the working directory to `code/figure_code/` and run

   ```r
   source("make_ms_figures.R")
   ```

   This creates all figures of the manuscript and the Supplementary
   Information and copies them to `results/figures/manuscript_b0/`, named
   after their figure number. It takes a few minutes.

To check that the simulation itself runs, without waiting several hours, set
the working directory to `code/` and run

```r
quick_test <- TRUE
source("run_one_job.R")
```

This runs one subregion with linear consumer responses and 100 iterations
(results in `results/quick_test/`, not used by the figures). The retained
services agree with the full results to within about 0.02.


## Folder structure

```
data/
  threats_ecosystem_services_by_species.xlsx
                         Our compilation from the literature:
                         sheet "Threats": which species each threat affects,
                           with the threat score (Magnitude = scope +
                           severity + 1), the number of the two factors that
                           were unknown (Unknown_factors), certainty and
                           references
                         sheet "Ecosystem Services": which species provide
                           each service, with references
  sub_networks_full/     Food webs of the 25 subregions, one file per
                         subregion (consumer;resource)
  metaweb.txt            The Barents Sea metaweb, same format
  species_props.csv      Species properties (trophic level, number of links,
                         taxonomy) per subregion, calculated from the food
                         webs

code/
  settings.R             How threat scores become extinction probabilities,
                         and where results are stored
  run_one_job.R          Runs the Bayesian network simulations for all
                         subregions and consumer response functions, with and
                         without species interactions, and computes the
                         ecosystem services retained
  run_all_simulations.R  Runs run_one_job.R for all threat magnitudes of the
                         sensitivity analysis (x 0.8, 0.9, 1.0, 1.1, 1.2)
  run_sensitivity_analyses.R
                         Sensitivity analysis of the treatment of threat
                         factors scored "Unknown"
  threat_functions.R, threat_functions_no_nw.R, get_ESS.R, get_ESS_no_nw.R,
  bayesian_nw_monte_carlo.cpp
                         Functions used by run_one_job.R

code/figure_code/
  make_ms_figures.R      Runs all figure scripts below and collects the
                         manuscript figures; lists which script makes which
                         figure

results/
  var_b0/                Main results
    ESS_changes_var.rds, ESS_changes_no_nw_var.rds
                         Ecosystem services retained with and without species
                         interactions, per threat, subregion and combination of
                         response functions
    persistence_results_var.rds, persistence_results_no_nw_var.rds
                         Probability that each species persists
    wiggle_magnitude/<factor>/
                         The same for all extinction probabilities multiplied
                         by 0.8-1.2
  var_b0_U0/, var_b0_U2/ Factors scored "Unknown" given level 0 (as scored)
                         or 2 (middle level) instead of 1 (main analysis)
  figures/manuscript_b0/ The figures of the manuscript and SI
```


## Model

The threat score of a species is scope + severity + 1 (1-7). A species
affected by a threat with score *s* has extinction probability *s*/10;
species not affected have 0. A factor (scope or severity) scored "Unknown" is
given the lowest level in the main analysis (column `Unknown_factors`). Ice loss and increased water
temperature are merged into one threat, as are pollution and marine litter.
See `code/settings.R` and the Methods of the manuscript.

How consumers respond to the loss of their resources, and how services respond
to the loss of their providers, are described by beta-distribution response
functions. The simulations use five consumer response functions (alpha, beta):
(1, 5), (1, 2), (1, 1), (2, 1) and (5, 1), and five service response
functions with the same values.


## Reproducing the simulations

Set the working directory to `code/` and run

```r
source("run_all_simulations.R")       # main results and threat magnitudes
source("run_sensitivity_analyses.R")  # treatment of Unknown scores
```

With 10,000 iterations (`nreps` in `run_one_job.R`) this takes several hours.
The simulations are Monte Carlo based without a fixed seed, so a rerun gives
slightly different numbers.


## R packages

tidyverse (dplyr, ggplot2, forcats, purrr, stringr), readxl, Rcpp, igraph, fs,
glue, scales, broom, magrittr, patchwork, pals, viridis, RColorBrewer

The C++ code in `bayesian_nw_monte_carlo.cpp` is compiled by Rcpp, which needs
a C++ compiler (Rtools on Windows, Xcode command line tools on macOS).


## Data sources

The metaweb and the subregional food webs are from the data of the following
study; the species properties in `species_props.csv` were calculated by us from
these data.

> Kortsch S., Primicerio R., Aschan M., Lind S., Dolgov A.V. & Planque B.
> (2018) Data from: Food-web structure varies along environmental gradients in
> a high-latitude marine ecosystem. Dryad.
> https://doi.org/10.5061/dryad.k04q2kd

> Kortsch S., Primicerio R., Aschan M., Lind S., Dolgov A.V. & Planque B.
> (2019) Food-web structure varies along environmental gradients in a
> high-latitude marine ecosystem. *Ecography* 42: 295-308.

Please cite these sources if you use the food web data. The assessments of
threats and ecosystem service providers in
`threats_ecosystem_services_by_species.xlsx` were compiled for this study;
the references for each assessment are given in the file.


## License

Code: MIT License (see `LICENSE`). Data compiled for this study
(`threats_ecosystem_services_by_species.xlsx`) and the simulation results:
CC BY 4.0. The food web data are redistributed from Dryad under the terms
given there.


## Contact

Anna Eklöf, Department of Physics, Chemistry and Biology (IFM), Linköping
University, Sweden.
