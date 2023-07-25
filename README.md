# `iehfc`
`iehfc` Function and Application Repository

## Contents

This repository will eventually be converted into a functional R package, which will contain:
- R scripts with functions to run simple high-frequency checks on datasets
- An iehfc Shiny dashboard to allow users to engage with the functions in an interactive manner.

## iehfc Dashboard — Current Use Instructions

NOTE — Work still needs to be done to create a fully independent set of scripts that can find each other on any individual's local setup. For now, the user is requested to **open the `iehfc.Rproj` file**, which will automatically set up the correct working directory for any user.

1. Either (a) replicate the `iehfc` repository using Github Desktop or equivalent or (b) download the files into a standalone folder
2. Open `iehfc.Rproj`
3. Run `iehfc_app/global.R`. This should install the required packages and launch the `iehfc` application. If at any point you want to relaunch the `iehfc` application after closing it, you can run `iehfc_app()` in the console.
