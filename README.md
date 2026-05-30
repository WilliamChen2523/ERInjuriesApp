# ERInjuriesApp

## Overview

ERInjuriesApp is a Shiny application for exploring emergency room injury data from the ER Injuries case study in *Mastering Shiny*.

The application allows users to:

* Explore summary statistics for injury-related variables
* Visualize distributions using histograms and bar charts
* Filter injury narratives by body part
* Interactively examine patient injury records

## Required Packages

Before running the application, install the following packages:

```r
install.packages(c(
  "shiny",
  "openintro",
  "dplyr",
  "DT"
))
```

## Installation

Clone or download this repository from GitHub.

## Running the Application

Open R or RStudio and navigate to the project directory.

Run:

```r
source("R/run_app.R")
run_app()
```

The Shiny application will then launch in a browser window.

## Data Source

The application uses the `injuries` dataset available in the `openintro` package.

## Author

William Chen
