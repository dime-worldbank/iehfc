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

To install the package directly from GitHub, execute the following commands in R:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Install IEHFC from GitHub
devtools::install_github("dime-worldbank/iehfc", ref = "create-package")
```

### ❗ Troubleshooting Installation Issues

- If you encounter errors related to the `promises` package, restart your R session and reinstall `promises` manually:
  ```r
  install.packages("promises")
  ```
- Ensure you have the latest version of R and RStudio installed.
- Check for dependency issues and update outdated packages using:
  ```r
  update.packages(ask = FALSE)
  ```

## 🚀 Launching the IEHFC Application

After installation, launch the **IEHFC** Shiny dashboard by running:

```r
library(iehfc)
iehfc_app()
```

This will open the application in your default web browser, enabling interactive data quality analysis and visualization.

## 🧭 Quick Guide

Once the dashboard is open, follow these steps to conduct data quality checks:

### 1️⃣ Upload Data

- Import a dataset in `.csv` format for validation.
- Preview the dataset before applying checks.

### 2️⃣ Select and Configure Checks

- **Duplicate Checks**: Verify whether an ID variable is uniquely identified. Provide the name of the ID variable and any additional variables to assist in resolving duplicates. The output is a table of duplicate observations.

- **Outlier Detection**: Identify outliers in individual numeric variables or in grouped variables using common prefixes (e.g., `income_01`, `income_02`, etc.). The platform detects values more than three standard deviations from the mean. Future versions will allow custom thresholds. The output is a table listing all identified outliers.

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

## 🤝 Contributing

We welcome contributions! If you have feature requests or encounter issues, please open an issue on [GitHub](https://github.com/dime-worldbank/iehfc/issues).

## 📝 License

This package is distributed under the **MIT License**.

---

👥 **Authors:**

- DIME Analytics Team

For inquiries, visit [DIME Analytics](https://www.worldbank.org/en/research/dime/data-and-analytics).

