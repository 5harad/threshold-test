# threshold-test

General Usage Notes
-------------------
The scripts included in this repo produce the threshold test results for the paper "Testing for Racial Discrimination in Police Searches of Motor Vehicles". 
More details regarding methodology can be found in the paper: https://5harad.com/papers/threshold-test.pdf


Recommended order to produce main result:
-----------------------------------------
1. download.py
2. to_tsv.py
3. data_processing.R
4. fit_main_result.R
5. generate_plots_main_result.R
6. generate_numbers_main_result.R


Recommended order to produce model check results:
-------------------------------------------------
1. robustness_tests.R
2. threshold_noise.R
3. generate_plots_model_checks.R
4. generate_numbers_model_checks.R



Script Details
--------------

download.py <br />
	- downloads the original North Carolina traffic stop data as received from the state as a tar file <br />
	- input: NA <br />
	- output: saves orig_data folder in data/ <br />

to_tsv.py
	- converts the original txt files to tsv files
	- input: .txt files (in orig_data folder)
	- output: .tsv files (in orig_data folder)

data_processing.R
	- combines tsv files and saves two clean RData files in data/
	- input: orig_data/*.tsv files
	- output 
		- "north_carolina_complete.RData" includes all stops in the original files 
		- "north_carolina.RData" includes only stops from the top 100 local police departments used for the paper 

fit_main_result.R
	- runs the model in Stan and saves result (3 objects) in output/
	- input: data/north_carolina.RData
	- output
		- obs: the data frame containing aggregate statistics for each police department and race
		- fit: the fit object returned by Stan
		- post: the posterior object returned by Stan

generate_plots_main_result.R
	- generates Figures 3-7 and Tables 1-3 in the paper, and saves in output/

generate_numbers_main_result.R
	- prints out the statistics relating to the main result quoted in the paper

robustness_tests.R
	- implements the omitted variable bias tests (age, year, time, gender), and the placebo tests (season, weekday), and saves results in output/

threshold_noise.R
	- implements the threshold heterogenity test for 0% - 5% noise

generate_plots_model_checks.R
	- generates Figures 1,2 8-10 in the paper, and saves in output/

generate_numbers_model_checks.R
	- prints out all other statistics quoted in the paper


