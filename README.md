# threshold-test

General Usage Notes
-------------------
The scripts included in this repo reproduce the results for the paper "The Problem of Infra-marginality
in Outcome Tests for Discrimination", by Camelia Simoiu, Sam Corbett-Davies, and Sharad Goel.
 
More details regarding methodology can be found in the paper: https://5harad.com/papers/threshold-test.pdf

Scripts should be run as executables from `src/`.  
Example usage: ./download.py


Recommended order to produce main result
-----------------------------------------
1. download.py
2. to_tsv.py
3. data_processing.R
4. fit_main_result.R
5. generate_plots_main_result.R
6. generate_numbers_main_result.R


Recommended order to produce model check results
-------------------------------------------------
1. robustness_tests.R
2. threshold_noise.R
3. generate_plots_model_checks.R
4. generate_numbers_data_subset_decisions.R



Script Details
--------------

**download.py**
  - Downloads the original North Carolina traffic stop data as received from the state as a tar file
  - Libraries: urllib, tarfile
  - Requires: NA
  - Output: saves `orig-data` folder in `data/`

**to_tsv.py**
  - Converts the original txt files to tsv files
  - Libraries: sys, os, pandas, csv
  - Requires: `orig-data/*.txt` files
  - Output: .tsv files (in `orig-data/`)

**data_processing.R**
  - Combines tsv files and saves two clean RData files in `data/`
  - Libraries: stringr, dplyr
  - Requires: `orig-data/*.tsv` files
  - Output 
    - `north_carolina_complete.RData` includes all stops in the original files 
    - `north_carolina.tsv` includes only stops from the top 100 local police departments used for the paper 

**fit_main_result.R**
  - Runs the model in Stan and saves result (3 objects) in `output/`
  - Requires: `data/north_carolina.tsv.gz`
  - Libraries: dplyr, rstan
  - Output
    - obs: the data frame containing aggregate statistics for each police department and race
	- fit: the fit object returned by Stan
	- post: the posterior object returned by Stan
  - Run time: ~ 8-10 hours, chains sampled in parallel on 5 cores

**generate_plots_main_result.R**
  - Generates Figures 3-7 and Tables 1-3 in the paper
  - Libraries: xtable, dplyr, rstan, ggplot2, pracma, matrixStats, scales, boot, stats, reshape2
  - Requires: `output/main_result.RData`
  - Output: saves plots in `output/`

**generate_numbers_main_result.R**
  - Prints out the statistics relating to the main result quoted in the paper
  - Libraries: dplyr, pracma, matrixStats
  - Requires: `output/main_result.RData`
  - Output: prints to stdout

**robustness_tests.R**
  - Implements the omitted variable bias tests (age, year, time, gender), and the placebo tests (season, weekday)
  - Libraries: dplyr, rstan
  - Requires: `data/north_carolina.tsv.gz`
  - Output: saves results in `output/`
  - Run time: ~ 4-8 hours per test x 21 tests, chains sampled in parallel on 5 cores

**threshold_noise.R**
  - Implements the threshold heterogenity test for 0% - 5% noise
  - Libraries: dplyr, rstan, boot
  - Requires: `output/main_result.RData`
  - Output: saves results in `output/`
  - Run time: ~ 8-20 hours per test x 5 tests, chains sampled in parallel on 5 cores

**generate_plots_model_checks.R**
  - Generates Figures 1,2 8-10 in the paper
  - Libraries: xtable, dplyr, rstan, ggplot2, pracma, matrixStats, scales, boot, stats, reshape2
  - Requires: `output/main_result.RData`, and results from `threshold_noise.R`, `robustness_tests.R`
  - Output: saves plots in `output/`

**generate_numbers_data_subset_decision.R**
  - Prints out all other statistics quoted in the paper
  - Libraries: dplyr
  - Requires: `output/north_carolina_complete.RData`
  - Output: prints to stdout

