# Why have the ambulance waiting times been getting worse?

## Project Status: [In progress]

## Project Description

Ambulance services are under immense pressure. This analysis looks at ambulance service performance and explores contributing factors and priorities for improvement.

## Outputs

The findings will be published as a Health Foundation long chart

## Data sources:

This repository only includes code for the charts in the above publication and is not an extensive reference list for the publication.

* [Ambulance quality indicators](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/ambulance-quality-indicators-data-2021-22/) 

* [NHS ambulance handover data](https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/) 

* [NHS staff sickness and absence rates](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates/april-2020-provisional-statistics)    

* [A&E waiting times](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/)

* [Critical care and general acute](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/)

## How does it work? 

This repository outlines how to download the data, process, analyse and visualise the data used in the publication. **The analysis and charts can be reproduced using the latest data by updating the data source links, updating the data range for some of the sources and creating a data folder within your projects folder.** 

It must be noted that we use AWS S3 cloud storage therefore where necessary the code for saving and retrieving files will need to be adapted based on your local storage structure. 

### Requirements 

These scripts were written in R version 4.0.02 and RStudio Workbench Version 1.1.383. The following R packages (available on CRAN) are needed:

* tidyverse
* ISO week 
* lubridate 
* curl
* plotly
* rio
* readxl
* janitor
* ggplot2 
* tsibble
* ggtext
* readr
* stirngr

### Getting started

The src folder contains 
 
* [01_data_load.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/01_data_load.R) - Download data- links need to be updated for the latest data
* [02_ambulance_data_cleaning.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/02_ambulance_data_cleaning.R)- Clean and save ambulance quality indicator variables (including response times, calls and incidents)
* [03_ambulance_response_times_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/03_ambulance_response_times_visualisation.R) - Visualising ambulance response times
* [04_ambulance_incidents_visualisations.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/04_ambulance_incidents_visualisations.R) - Visualising number of incidents handled by the ambulance services
* [05_ambulance_calls_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/05_ambulance_calls_visualisation.R)- Visualising number of calls to the ambulance services
* [06_workforce_data_cleaning.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/06_workforce_data_cleaning.R) - Clean and save workforce variables (includes staff sickness and absence rate)
* [07_workforce_data_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/07_workforce_data_visualisation.R) - Visualise workforce data including staff sickness and absence rate
* [08_bed_occupancy_data_cleaning.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/08_bed_occupancy_data_cleaning.R) - Clean and save bed occupancy data 
* [09_bed_occupancy_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/09_bed_occupancy_visualisation.R) - Visualise bed occupancy
* [10_ambulance_handover_times_data_cleaning.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/10_ambulance_handover_times_data_cleaning.R)- Clean and save ambulance handover times data
* [11_handover_times_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/11_handover_times_visualisation.R) - Visualise ambulance handover times 
* [12_AE_attendances_data_cleaning_and_visualisation.R](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/Scripts/12_AE_attendances_data_cleaning_and_visualisation.R) - Clean, save and visualise A&E attendances

## Authors
* Anne Alarilla - [Twitter](https://twitter.com/AlarillaAnne) - [GitHub](https://github.com/annealarilla)
* Mai Stafford - [Twitter](https://twitter.com/stafford_xm) - [GitHub](https://github.com/maistafford)

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/LICENSE).









