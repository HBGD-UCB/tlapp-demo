#install and load tltools
library(devtools)
install_github("jeremyrcoyle/tltools")
library(tltools)

# template file (adjust demo folder)
rmd_filename <- "~/Dropbox/gates/tlapp-demo/templates/tmle3_template.Rmd"

# generate inputs file (can also use an existing one)
temp_sample_json <- tempfile()
params_from_rmd(rmd_filename, temp_sample_json)

# to run on your machine (currently must install dependencies manually -- see dependency list in Rmd header)
run_locally(rmd_filename, temp_sample_json, open_result = TRUE)

# to run on ghap cluster (dependencies will be installed automatically)
# put your ghap credentials in a json file
configure_ghap("~/ghap.json")
inputs_json <- "~/Dropbox/gates/tlapp-demo/templates/birthweight_inputs.json"
run_on_cluster(rmd_filename, inputs_json, open_result = TRUE)

# ghap_ip: 52.90.130.177
