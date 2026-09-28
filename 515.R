install.packages("readxl")
library(readxl)
install.packages("SmartEDA")
library(SmartEDA)
library(dplyr)


#loaded dataset into df
raw_data <- read_excel("default of credit card clients.xls")

#dropped the first data row(column description) and first column(id)
clean_data <- raw_data[-1, -1]

#checked the data types for the columns.
#all of them were catagorical so conversion had to be done
table(sapply(clean_data, class))

#converted the numerical columns into their proper datatype
credit_data <- clean_data %>%
  mutate(across(
    c("X1", "X5", X12:X23), 
    ~ as.numeric(as.character(.))
  ))


#created the report. Some categorical variables... 
#...with over 10 distinct values did not show.
ExpReport(data = credit_data, Target = NULL, op_file = "smarteda_report.html")


#generated the visualizations for the extra categorical columns
#and added them manually to the html report
ExpCatViz(
  data = credit_data, 
  target = Y, 
  clim = 15
)


