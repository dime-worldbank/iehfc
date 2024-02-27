# `iehfc`
Welcome to the `iehfc` platform by DIME Analytics!

The `iehfc` platform is a Shiny Dashboard that simplifies the process of setting up data quality checks. `iehfc` provides easy-to-create, customizable, and shareable high-frequency check outputs.

This platform is currently under construction. If you find any issues or have any suggestions, please open an issue and let us know!

## Contents

This repository will eventually be converted into a functional R package, which will contain:
- R scripts with functions to run simple high-frequency checks on datasets
- An iehfc Shiny dashboard to allow users to engage with the functions in an interactive manner.

## iehfc Platform — Current Use Instructions

NOTE — Work still needs to be done to create a fully independent set of scripts that can find each other on any individual's local setup. For now, the user is requested to **open the `iehfc.Rproj` file**, which will automatically set up the correct working directory for any user.

1. Either (a) clone the `iehfc` repository using Github Desktop or equivalent or (b) download the files into a standalone folder
2. Open `iehfc.Rproj`
3. Run `iehfc_app/global.R`. This should install the required packages and launch the `iehfc` application. If at any point you want to relaunch the `iehfc` application after closing it, you can run `iehfc_app()` in the console.

## iehfc Platform — Quick Guide

Once you have managed to open the `iehfc` Shiny Dashboard, you can follow the steps below to obtain your data quality check outputs. The `iehfc` Platform is composed of three principal tabs.

### Upload Data

This is where you can upload the dataset whose quality you want to check and inspect its contents. You can either upload your own dataset or use the provided test dataset. Currently, the platform only supports .csv files. Once you have successfully loaded your dataset, you can move to the next tab.

### Check Selection and Setup

This is where you set up the content and parameters of the checks you want to run. There are currently four available types of checks, with two more under construction.

---
**Duplicate Checks** — Allows you to verify whether an ID variable is uniquely identified.
- Please provide the name of the variable, as well as any additional variables that would help address the duplicates in the outputs.
- The output is a table with the duplicate observations.

---
**Outlier Checks** — Allows you to check whether an individual variable or a group of variables has any outliers.
- You can provide the names of individual (numeric) variables to check for outliers.
- You can also provide a common prefix (e.g. "income" for "income_01", "income_02", etc.) for a group of (numeric) variables whose aggregated values you would like to check for outliers. This is particularly useful if you have the same indicator divided into multiple variables, such as income for multiple household members, for instance.
- The platform currently considers values that are over three standard deviations from the mean to be outliers. In the next version of the platform, the user will be able to set manual limits or adjust the distance to define outliers.
- You'll need to provide the dataset's ID variable and can add additional variables here as well.
- The output is a table with a row for each identified outlier.

---
**Enumerator Checks** — Allows you to check average values and progress for individual enumerators if you are conducting primary data collection.
- Please provide the variable that identifies the enumerator in the dataset.
- You can then add (numeric) variables for which you'd like to see the average value per enumerator. This can be useful to check whether an enumerator has an unusually different set of values to the others.
- You can also add a submission date variable. This is strongly encouraged. This allows you to see the number of submissions per enumerator per day, and thus to track their progress.
- You can also add a "complete submission" variable. This will allow you to see the percentage of complete submissions per enumerator.
- The outputs are (1) a table with submissions per enumerator and — if a submission date was provided — submissions per day, (2) a table with average values of variables per enumerator if variables were provided in the "Enumerator Average Value Variable" fields, and (3) a graph showing cumulative submissions per enumerator if a submission date was provided.

---
**Administrative Unit-Level Checks** — Allows you to check submissions and progress for individual administrative units (e.g. villages) in your dataset.
- Please provide the variable that identifies the administrative unit of interest in the dataset.
- You can then add additional, higher-level administrative units (e.g. if you administrative unit of interest is "village", these could be "district" and "county") that would help either locate your administrative units of interest or uniquely identify them.
- You can also add a submission date variable. This is strongly encouraged. This allows you to see the number of submissions per administrative unit per day, and thus to track their progress.
- You can also add a "complete submission" variable. This will allow you to see the percentage of complete submissions per administrative unit.
- The outputs are (1) a table with submissions per administrative unit and — if a submission date was provided — submissions per day and (2) a graph showing cumulative submissions per administrative unit if a submission date was provided.

---
**Under Construction** — Unit of Observation-Level Checks and Survey Logic Checks
