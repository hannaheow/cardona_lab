# Animal Data Analysis App

This Shiny app allows you to upload an animal database, automatically
standardize the data, and create plots for analysis.

The goal of this app is to make it possible to go from the experimental
data spreadsheet to a consistent, analysis-ready plot without having
to manually clean the data each time.

---

## How to use the app

### 1. Prepare your data

The app is designed to work with an Excel (`.xlsx`) file following the
format of the originally supplied:

`Data_template-v3.1-ac.xlsx`

It is important to follow this template as closely as possible.

In particular, please try to keep:

- Column names consistent
- The same general structure of the spreadsheet
- Consistent ways of entering categories such as sex, diet, region,
  diabetes status, and time point

The more closely the uploaded spreadsheet follows the original
template, the more smoothly the app will run.

---

## What happens when you upload a file?

When you upload your Excel file, the app automatically:

1. Reads the Excel spreadsheet
2. Standardizes the column names
3. Checks that the required columns are present
4. Converts variables to the appropriate data types
5. Standardizes diet group names
6. Adds optional columns that are missing
7. Creates the requested plot

You do **not** need to manually clean the spreadsheet before uploading it
if it follows the expected template.

---

# Making changes to the data structure

## If you need to add a new variable or change a column name

The file to edit is:

`standardize_animal_data.R`

This file contains the `standardize_animal_data()` function.

The function is responsible for making sure that uploaded spreadsheets
are converted into a consistent format that the rest of the app can use.

### For example

Suppose a new version of the experiment adds a variable called:

`Treatment Dose`

You could add that variable to the standardization function so that the
app knows about it.

Similarly, if the spreadsheet changes a column name, such as:

`Treatment Group`

becoming:

`Treatment`

the `standardize_animal_data()` function can be updated to recognize the
new name.

The function can also be updated when a new variable needs to be treated
as:

- A date
- A numeric measurement
- A categorical/text variable

### Required vs. optional columns

The standardization function has two types of columns.

**Required columns** are necessary for the app to understand the
experimental design and create the current plot.

If a required column is missing, the app will report an error and ask
for the missing column.

**Optional columns** are useful for analysis but are not required for
every experiment.

If an optional column is missing, the app will automatically create the
column and fill it with `NA`.

`NA` means that no value was provided.

This means that it is okay for an uploaded spreadsheet to leave out
optional variables that are not relevant to a particular experiment.

---

# Making changes to the plot

## If you want to change how the plot looks or how the data are grouped

The file to edit is:

`make_animal_plot.R`

This file contains the `make_animal_plot()` function.

This is the file to modify if you want to change things about the
**visualization or analysis used to create the plot**.

For example, changes to this file could include:

- Changing the plot type
- Changing the colors
- Changing the legend
- Changing axis labels
- Changing how experimental groups are displayed
- Changing how time points are grouped
- Changing how diets are displayed
- Changing how regions are displayed
- Changing the error bars
- Changing whether the plot shows medians, means, or another summary

### Important

If the question is:

> "I want the plot to look different."

Start by looking at:

`make_animal_plot.R`

If the question is:

> "The uploaded spreadsheet has a new column, a different column name,
> or a variable that needs to be treated differently."

Start by looking at:

`standardize_animal_data.R`

---

# Choosing which variable to plot

The app is designed so that the measurement being plotted can be
selected from the variables available in the uploaded dataset.

For example, the same app could eventually be used to plot:

- NeuN counts
- Iba1 counts
- Another cell count
- A different numeric measurement

This means that you should **not** need to create a completely separate
plotting script for every measurement.

The plotting function (`make_animal_plot.R`) controls *how* the selected
measurement is summarized and displayed.

The Shiny app controls *which* measurement is selected.

---

# Files in this project

The most important files are:

### `app.R`

This is the Shiny app itself.

It controls:

- The user interface
- File upload
- Variable selection
- Running the cleaning function
- Running the plotting function
- Downloads

Most changes to the data cleaning or visualization should **not**
require editing this file.

---

### `standardize_animal_data.R`

This contains the data-cleaning function.

Use this file when:

- A new column is added
- A column name changes
- A variable needs a different data type
- A new optional variable should be recognized
- A new required variable is needed
- The Excel template changes

---

### `make_animal_plot.R`

This contains the plotting function.

Use this file when:

- The visual appearance needs to change
- Colors need to change
- Legends need to change
- Axis labels or units need to change
- Groups need to be displayed differently
- Time points need to be grouped differently
- Regions need to be displayed differently
- The summary statistic needs to change
- Error bars need to change

---

### `Data_template-v3.1-ac.xlsx`

This is the original data template used to develop the app.

Whenever possible, use this template as the starting point for new
data collection.

If the experimental data structure changes, the standardization function
can be updated to accommodate those changes.

---

# If something goes wrong

If the app reports that required columns are missing, first check the
uploaded Excel file against:

`Data_template-v3.1-ac.xlsx`

Make sure the column names and general structure match the template.

If the experiment has intentionally changed and the new column names or
variables are correct, the app may simply need to be updated.

In that case, update:

`standardize_animal_data.R`

rather than changing the uploaded data just to make the app work.

If the data are being read correctly but the resulting plot is not what
you want, update:

`make_animal_plot.R`

---

# Quick guide

| What do you want to change? | File to edit |
|---|---|
| Add a new column | `standardize_animal_data.R` |
| Change a column name | `standardize_animal_data.R` |
| Change a variable's data type | `standardize_animal_data.R` |
| Make a new column optional/required | `standardize_animal_data.R` |
| Change plot colors | `make_animal_plot.R` |
| Change the legend | `make_animal_plot.R` |
| Change axis labels | `make_animal_plot.R` |
| Change units | `make_animal_plot.R` |
| Change experimental grouping | `make_animal_plot.R` |
| Change time-point grouping | `make_animal_plot.R` |
| Change region display | `make_animal_plot.R` |
| Change medians/error bars | `make_animal_plot.R` |
| Change the file upload or app interface | `app.R` |

---

## The basic rule

**Keep the Excel data as close as possible to the original
`Data_template-v3.1-ac.xlsx` format.**

If the **data structure changes**, update:

`standardize_animal_data.R`

If the **plot changes**, update:

`make_animal_plot.R`

If the **app interface changes**, update:

`app.R`

You should generally **not need to edit `app.R` just to accommodate a new
variable or change how a plot looks.**