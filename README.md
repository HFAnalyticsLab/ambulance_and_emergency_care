# Why have the ambulance waiting times been getting worse?

## Project Status: [In progress]

## Project Description

Ambulance services are under immense pressure. This analysis looks at ambulance service performance and explores contributing factors and priorities for improvement.

## Outputs

The findings will be published as Health Foundation long chart

## Data sources:

This repository only includes code for the charts in the above publication and is not an extensive reference list for the publication. 

* [Ambulance quality indicators](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/ambulance-quality-indicators-data-2021-22/) 

* [NHS ambulance handover data](https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/) 
N.B. Only available during winter

* [NHS staff sickness and absence rates](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates/april-2020-provisional-statistics)    

* [A&E waiting times](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/)

* [Critical care and general acute](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/)

## How does it work? 

This repository outlines how to download the data, process, analyse and visualise the data used in the publication. The analysis and charts can be reproduced using the latest data by updating the data source links and creating a data folder within your projects folder. 

It must be noted that we use AWS S3 cloud storage therefore saving and retrieving files will need to be adapted based on your local storage structure. 

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
 
* 







