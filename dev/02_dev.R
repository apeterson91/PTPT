# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("leaflet")
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("sf")
usethis::use_package("shinyjs")
usethis::use_package("vroom")
usethis::use_package("glue")
usethis::use_package("shinythemes")
usethis::use_package("stringr")
usethis::use_package("DT")
# TODO: look into incorporating leafgl for street segments
# usethis::use_package("leafgl")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "DisplayMap" ) # Name of the module
golem::add_module(name = "AboutPage")
golem::add_module(name = "Methods")
golem::add_module(name = "Analysis")
golem::add_module(name = "EducationAnalysis")

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers") 
golem::add_fct("TripEntry")
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "streets", open = FALSE ) 
usethis::use_data_raw( name = "hoods", open = FALSE) 
usethis::use_data_raw(name = "pghcs", open = FALSE) 
usethis::use_data_raw(name = "s1hdata",open = FALSE)
usethis::use_data_raw(name = "s1tdf", open = FALSE)
usethis::use_data_raw(name = "s1sdata",open = FALSE)
usethis::use_data_raw(name = "hptt",open = FALSE)
usethis::use_data_raw(name = "s1mods", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("PTPT")
usethis::use_vignette("Methods")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
#usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action(name = "R-CMD-check.yaml") 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
#usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
#usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

