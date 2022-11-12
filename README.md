# Psychonomics 2022 - Metamemory I (232)

## Exploring the Mechanisms of Output Interference During Cued Recall Using Metamemory Judgments

The files in this repo support the spoken presentation for the Psychonomic Society. For Quarto files that are in a subdirectory, they are called by the main script. R files prepended by an underscore (`_`) are called by the main R script.

Unfortunately, I am not able to upload raw data files to Github/OSF. Please contact TC if you have any questions or concerns.

## Files

_Main presentation_:

  * `./tc_talk.pptx`

_Quarto files_:

  * `./tc_talk.qmd`
    + The main working file.
  * `./_quarto.yml`
    + Contains the necessary YAML declarations
  * `./sections/1_intro.qmd`
  * `./sections/2_methods.qmd`
  * `./sections/3_results.qmd`
  * `./sections/4_conclusions.qmd`
  * `./etc/*`
    + Contains the custom Powerpoint template.

_Analysis files_:

  * `./analysis/pnomics_analyses.R`
    + Main analysis file.
  * `./analysis/_helper_functions.R`
    + Declaration of custum functions to help with analyses.
  * `./analysis/_import_dat.R`
  * `./analysis/_overall_gamma.R`
    + Calculates canonical Goodman-Kruskal gamma correlations for different DVs and data subsets.
  * `./analysis/_partset_cor.R`
    + Calculates GK gammas using more unconventional methods.
  * `./analysis/_save_figs.R`
    + Explicit specifications for saving figures to my personal computer that are otherwise unhelpful.
  * `./img/*`
    + Default path for saved images.

## External Calls

These files are linked to the project's OSF repository:

  * [osf.io/2eqv7](https://osf.io/2eqv7/)