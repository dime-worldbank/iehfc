# 📦 IEHFC: High-Frequency Data Quality Checks

## 📄 Overview

The **IEHFC** package, developed by **DIME Analytics**, is a Shiny-based application designed to facilitate high-frequency data quality assessments in survey datasets. This tool provides an interactive dashboard to detect data inconsistencies, duplicate records, outliers, and other integrity issues, ensuring robust data quality control.

**Note**: This package is in Beta version; expect updates. 

### ✨ Key Features

- **Data Upload & Inspection**: Users can upload datasets in `.csv` format and inspect data before running quality checks.
- **Automated Quality Checks**: The platform provides built-in functions for identifying duplicates, detecting outliers, and assessing enumerator performance.
- **Customizable Reports**: Users can generate and export reports summarizing data integrity checks.
- **Collaborative Analysis**: The tool allows for easy sharing of quality control outputs with research teams.

## 🛠️ Installation

To install the package directly from GitHub, open RStudio and run the following commands:

````r
# Install devtools if you don't have it yet
install.packages("devtools")

# Install the IEHFC package from GitHub
devtools::install_github("dime-worldbank/iehfc")
````

> **Note**: You may see a warning that says:  
> *"WARNING: Rtools is required to build R packages, but is not currently installed."*  
> You can safely ignore this message. Rtools is **not** required to install and use the app.

### ❗ Troubleshooting Installation Issues

If you run into errors related to the `promises` package (or another package), try restarting your R session and installing the package manually:

````r
install.packages("promises")
````

## 🚀 Launching the IEHFC Application

After installation, launch the **IEHFC** Shiny dashboard by running:

```r
library(iehfc)
iehfc_app()
```

This will open the application in your default web browser, enabling interactive data quality analysis and visualization.

> 🖼️ The app looks like this:
>
> <img src="https://github.com/dime-worldbank/iehfc/raw/main/inst/iehfc_app/www/iehfc_app.png" width="50%" />


## 🧭 Quick Guide

Once the dashboard is open, follow these steps to conduct data quality checks:

### 1️⃣ Upload Data

- Import a dataset in `.csv` format for validation.
- Preview the dataset, variable names, and types before applying checks.

### 2️⃣ Select and Configure Checks

- **Duplicate Checks**: Verify whether an ID variable is uniquely identified. Provide the name of the ID variable and any additional variables to assist in resolving duplicates. The output is a table of duplicate observations.

- **Outlier Detection**: Identify outliers in individual numeric variables or in grouped variables using common prefixes (e.g., `income_01`, `income_02`, etc.). The platform lets the user decide between Standard Deviation or IQR. The output is a table listing all identified outliers, as well as graphs with individual and grouped outliers.

- **Enumerator Checks**: Assess enumerator performance during data collection. Specify the enumerator ID variable, numeric variables to summarize, and (optionally) a submission date and a completeness indicator. Outputs include:
  1. Submission counts per enumerator (with daily breakdown if a date is provided),
  2. Average values of specified variables by enumerator, and
  3. A cumulative submissions graph if a date is provided.

- **Administrative Unit Checks**: Monitor submissions across geographic areas (e.g., villages). Specify the main administrative unit variable and optionally higher-level geographic identifiers. You may also provide submission dates and completeness indicators. Outputs include:
  1. Submission counts per unit and per day (if applicable), and
  2. A cumulative submissions graph if a date is provided.

### 3️⃣ Review and Export Results

- View results through interactive tables and visualizations.
- Download reports summarizing identified issues and recommendations.

### 4️⃣ Extra Features

- **Template Code Export**: You can download a template R or Stata script to replicate the checks programmatically. You’ll need to edit the variables to match your dataset.
- **Parameter Upload**: After configuring checks, you can download your settings to reuse them later. Use "Upload Parameters" to automatically populate fields without needing to configure everything again.

## 🤝 Contributing

We welcome contributions! If you have feature requests or encounter issues, please open an issue on [GitHub](https://github.com/dime-worldbank/iehfc/issues).

## 📝 License

This package is distributed under the **MIT License**.

---

👥 **Authors:**

## Authors and Contributors

This app was developed by DIME Analytics team

| Name                  | Role                     |
|-----------------------|--------------------------|
| Marc-Andrea Fiorina   | Author                   |
| Maria Reyes Retana    | Author                   |
| Marina Visintini      | Author                   |
| Mehrab Ali            | Author                   |
| Ankriti Singh         | Stata Code Reviewer      |

For inquiries, visit [DIME Analytics](https://www.worldbank.org/en/about/unit/unit-dec/impactevaluation/dime-analytics).

