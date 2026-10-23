# Credit Card Default EDA

Exploratory data analysis of the **Default of Credit Card Clients** dataset, prepared for ICT515. The analysis uses R and SmartEDA to summarize the dataset and explore categorical variables in relation to the default outcome.

## Project files

- `515.R`: imports and prepares the data, then creates an automated EDA report and categorical visualizations.
- `default of credit card clients.xls`: source dataset used by the script.
- `smarteda_report.html`: generated EDA report.
- `x6.png`–`x9.png`: categorical plots saved alongside the report.

## Requirements

- R
- R packages: `readxl`, `SmartEDA`, and `dplyr`

The script calls `install.packages()` for `readxl` and `SmartEDA` when it runs. Install `dplyr` once in R if it is not already available:

```r
install.packages("dplyr")
```

## Run the analysis

Open `ict515.Rproj` in RStudio, or set the working directory to this project folder. Keep the Excel dataset in the project folder, then run:

```sh
Rscript 515.R
```

The script removes the first row of the imported spreadsheet (the column-description row) and the first column (the ID), then converts selected fields to numeric. It writes `smarteda_report.html` to the project folder. `ExpCatViz()` also produces categorical plots for target `Y`; the report currently includes plots added manually, so update those images in the report if the dataset or analysis changes.

Open `smarteda_report.html` in a browser to view the report.