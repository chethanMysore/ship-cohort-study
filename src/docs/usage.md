## Packages Used

 - evoxploit - https://github.com/unmnn/evoxploit
 - tidyverse
 - checkmate
 - rlist
 - hash
 - hms
 - ggplot2
 - visdat
 - naniar
 - pre
 - caret
 - R6
 - xlsx
 - DescTools
 - shinydashboard
 - groupdata2
 - iml
 - ICEbox
 - devtools
 - plumber - https://github.com/rstudio/plumber
 - jsonlite
 - yaml
 
## Usage Example
 
Run sample.R to launch the api

```r
source('./R/api.R')

# Inspect data
sample_df <- data_df      # import dataset here

# Create ShipCohortStudy object
ship_study_results <- ShipCohortStudy$new(sample_df)

# print summary
ship_study_results$summary()

# launch api
launchAppDev(port = 3000, ship_study_results = ship_study_results)
```
