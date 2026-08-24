# ============================================================
# FUNCTION: Standardize Animal Data
# ============================================================
#
# PURPOSE
# -------
#
# This function takes an uploaded animal database and cleans
# and standardizes it so that it can be used for analysis
# and plotting.
#
# The function does NOT read a file and does NOT save a file.
#
# Instead:
#
#     INPUT:
#         An animal database (data frame)
#
#     OUTPUT:
#         A cleaned animal database (data frame)
#
#
# The cleaning process does the following:
#
#   1. Renames columns so they have consistent names
#   2. Standardizes ALL column names
#   3. Checks that required columns are present
#   4. Converts columns to the appropriate data type
#      (dates, numbers, or text)
#   5. Standardizes diet group names
#   6. Adds missing optional columns as NA
#   7. Puts columns into a consistent order
#
#
# IMPORTANT:
#
# Some columns are REQUIRED for the app to understand the
# experimental structure and make the current plot.
#
# If a required column is missing, the function will STOP
# and tell the user which columns need to be added.
#
# Other columns are OPTIONAL.
#
# If an optional column is missing, the function will create
# that column and fill it with NA.
#
#
# This function is designed to be used by the Shiny app.
# The Shiny app will:
#
#     1. Let the user upload a CSV
#     2. Read the CSV
#     3. Send the data to this function
#     4. Receive the cleaned data back
#     5. Use the cleaned data to create plots
#
# ============================================================

# -------------------------------
# Load packages
# -------------------------------

# tidyverse provides the tools we use to clean and
# manipulate the data.

library(tidyverse)


# lubridate provides tools for working with dates.

library(lubridate)


# ============================================================
# Define the function
# ============================================================

# "standardize_animal_data" is the name of our function.
#
# The function has one input:
#
#     data
#
# "data" should be the animal database that was uploaded
# by the user.
#
# The function will return a cleaned version of that data.

standardize_animal_data <- function(data) {
  # ==========================================================
  # STEP 1: DEFINE REQUIRED AND OPTIONAL COLUMNS
  # ==========================================================

  # Some columns are REQUIRED for the current analysis.
  #
  # These columns tell us:
  #
  #   - which animal each observation belongs to
  #   - the animal's sex
  #   - which diet the animal received
  #   - which time point the animal belongs to
  #   - whether the animal is diabetic
  #   - which brain/tissue region was measured
  #
  # Without these columns, the current plot cannot be
  # constructed correctly.
  #
  # If one of these columns is missing, the function will
  # stop and tell the user which required columns are missing.

  required_columns <- c(
    "animal_id",
    "sex",
    "diet_group",
    "time_point",
    "diabetes_status",
    "region"
  )

  # These columns are OPTIONAL.
  #
  # They may be useful for future analyses, but they are not
  # required to create the current plot.
  #
  # If one of these columns is missing from the uploaded file,
  # we will create it and fill it with NA.
  #
  # NA means "no value was provided."

  optional_columns <- c(
    "id_sex",
    "dob",
    "age_weeks",
    "genotype",
    "treatment_group",
    "experiment_start_date",
    "experiment_end_date",
    "glucose_mg_dl",
    "biological_replica",
    "diet_start_date",
    "diet_end_date",
    "tissue_type",
    "tissue_processing",
    "date_tissue_sectioning",
    "image_id",
    "technical_replica",
    "stain_researcher",
    "stain_completion_date",
    "imagist",
    "imaging_date",
    "iba1_count",
    "neun_count",
    "x_confocal",
    "y_confocal",
    "z_confocal",
    "final_neu_n",
    "final_iba1",
    "notes"
  )

  # ==========================================================
  # STEP 2: RENAME COLUMNS
  # ==========================================================

  # The uploaded database may contain column names with:
  #
  #   - spaces
  #   - special characters
  #   - inconsistent capitalization
  #
  # Here we give those columns consistent names.
  #
  # For example:
  #
  #     "ID & Sex" becomes "id_sex"
  #
  # The backticks (`) around some original column names
  # are necessary because those names contain spaces
  # or special characters.

  data <- data %>%
    rename(
      # "ID & Sex" becomes "id_sex"
      id_sex = `ID & Sex`,

      # "DOB (YYYY-MM-DD)" becomes "dob"
      dob = `DOB (YYYY-MM-DD)`,

      # "Age (weeks)" becomes "age_weeks"
      age_weeks = `Age (weeks)`,

      # "Time Point" becomes "time_point"
      time_point = `Time Point`,

      # "Glucose_mg/dL" becomes "glucose_mg_dl"
      glucose_mg_dl = `Glucose_mg/dL`,

      # "Technical Replica" becomes "technical_replica"
      technical_replica = `Technical Replica`,

      # "Stain Researcher" becomes "stain_researcher"
      stain_researcher = `Stain Researcher`,

      # "Image ID" becomes "image_id"
      image_id = `Image ID`,

      # "Tissue_Type" becomes "tissue_type"
      tissue_type = Tissue_Type,

      # "Tissue_Processing" becomes "tissue_processing"
      tissue_processing = Tissue_Processing,

      # These columns already have the names we want.
      stain_completion_date = stain_completion_date,
      imaging_date = imaging_date,

      # Rename the NeuN and Iba1 count columns.
      iba1_count = Iba1_count,
      neun_count = NeuN_count
    ) %>%

    # clean_names() provides another layer of
    # standardization across ALL column names.
    #
    # For example, it converts names to:
    #
    #   lowercase
    #   underscores instead of spaces
    #   no special characters
    #
    # This makes the column names predictable for the
    # rest of the app.

    janitor::clean_names()

  # ==========================================================
  # STEP 3: CHECK FOR REQUIRED COLUMNS
  # ==========================================================

  # Now that the column names have been standardized, we can
  # check whether all of the required columns are present.
  #
  # setdiff() compares two lists and finds items that appear
  # in the first list but not the second.
  #
  # In plain English:
  #
  #     "Which required columns are missing from the
  #      uploaded data?"

  missing_required <- setdiff(
    required_columns,
    names(data)
  )

  # If one or more required columns are missing, STOP.
  #
  # We do not want the app to continue trying to make a plot
  # because the plot would not be meaningful without these
  # columns.
  #
  # length() tells us how many columns are missing.
  #
  # If length() is greater than zero, at least one required
  # column is missing.

  if (length(missing_required) > 0) {
    stop(
      paste0(
        "The uploaded file is missing the following ",
        "required columns: ",
        paste(
          missing_required,
          collapse = ", "
        ),
        ". Please add these columns and upload the ",
        "file again."
      )
    )
  }

  # ==========================================================
  # STEP 4: CONVERT COLUMNS TO THE CORRECT DATA TYPE
  # ==========================================================

  # Different types of information need to be stored
  # differently.
  #
  # For example:
  #
  #   Dates        -> Date
  #   Measurements -> numeric
  #   Categories   -> character/text
  #
  # This section makes sure each column has the appropriate
  # data type.

  data <- data %>%
    mutate(
      # ------------------------------------------------------
      # DATE COLUMNS
      # ------------------------------------------------------

      # as.Date() tells R that these columns contain dates.
      #
      # This allows R to correctly sort dates and perform
      # calculations involving dates.

      across(
        c(
          dob,
          experiment_start_date,
          experiment_end_date,
          diet_start_date,
          diet_end_date,
          date_tissue_sectioning,
          stain_completion_date,
          imaging_date
        ),
        as.Date
      ),

      # ------------------------------------------------------
      # NUMERIC COLUMNS
      # ------------------------------------------------------

      # as.numeric() tells R that these columns contain
      # numbers.
      #
      # This allows us to calculate things like:
      #
      #   - means
      #   - medians
      #   - differences
      #   - ranges
      #
      # and to use the variables in plots.

      across(
        c(
          age_weeks,
          glucose_mg_dl,
          biological_replica,
          iba1_count,
          neun_count,
          x_confocal,
          y_confocal,
          z_confocal,
          final_neu_n,
          final_iba1
        ),
        as.numeric
      ),

      # ------------------------------------------------------
      # CATEGORICAL / TEXT COLUMNS
      # ------------------------------------------------------

      # as.character() tells R that these columns contain
      # text or categories.
      #
      # Examples:
      #
      #   sex = "Male" / "Female"
      #   diet_group = "normal_diet" / "dox_diet"
      #   region = "Region 1" / "Region 2"

      across(
        c(
          animal_id,
          sex,
          id_sex,
          genotype,
          treatment_group,
          time_point,
          diabetes_status,
          diet_group,
          tissue_type,
          tissue_processing,
          region,
          image_id,
          technical_replica,
          stain_researcher,
          imagist,
          notes
        ),
        as.character
      )
    )

  # ==========================================================
  # STEP 5: STANDARDIZE DIET GROUP NAMES
  # ==========================================================

  # People may enter the same diet in slightly different ways.
  #
  # For example:
  #
  #     "Normal Diet"
  #     "normal diet"
  #     "Normal Diet "
  #     "normal_diet"
  #
  # Without standardization, R would treat these as
  # different groups.
  #
  # We convert them all to:
  #
  #     "normal_diet"
  #
  # str_to_lower() converts all letters to lowercase.
  #
  # str_replace_all() replaces spaces with underscores.

  data <- data %>%
    mutate(
      diet_group = diet_group %>%
        str_to_lower() %>%
        str_replace_all("\\s+", "_")
    )

  # ==========================================================
  # STEP 6: CHECK FOR MISSING OPTIONAL COLUMNS
  # ==========================================================

  # Now look for optional columns that are not present.
  #
  # Unlike required columns, missing optional columns do NOT
  # cause an error.

  missing_optional <- setdiff(
    optional_columns,
    names(data)
  )

  # ==========================================================
  # STEP 7: ADD MISSING OPTIONAL COLUMNS
  # ==========================================================

  # If any optional columns are missing, create them.
  #
  # The new columns will contain NA for every row.
  #
  # This means that all cleaned datasets will have the same
  # overall structure, even if a particular upload does not
  # contain every possible measurement.

  if (length(missing_optional) > 0) {
    data[missing_optional] <- NA
  }

  # ==========================================================
  # STEP 8: DEFINE THE STANDARD COLUMN ORDER
  # ==========================================================

  # This list defines the order in which columns should
  # appear in the cleaned database.
  #
  # It contains both the required and optional columns.

  needed_columns <- c(
    "animal_id",
    "sex",
    "id_sex",
    "dob",
    "age_weeks",
    "genotype",
    "treatment_group",
    "time_point",
    "experiment_start_date",
    "experiment_end_date",
    "diabetes_status",
    "glucose_mg_dl",
    "biological_replica",
    "diet_group",
    "diet_start_date",
    "diet_end_date",
    "tissue_type",
    "tissue_processing",
    "date_tissue_sectioning",
    "region",
    "image_id",
    "technical_replica",
    "stain_researcher",
    "stain_completion_date",
    "imagist",
    "imaging_date",
    "iba1_count",
    "neun_count",
    "x_confocal",
    "y_confocal",
    "z_confocal",
    "final_neu_n",
    "final_iba1",
    "notes"
  )

  # ==========================================================
  # STEP 9: REORDER THE COLUMNS
  # ==========================================================

  # Put the columns into the standard order defined above.
  #
  # This does NOT change any of the data.
  #
  # It only changes the order in which the columns appear.

  data <- data %>%
    select(
      all_of(needed_columns)
    )

  # ==========================================================
  # STEP 10: RETURN THE CLEANED DATA
  # ==========================================================

  # This sends the cleaned dataset back to whatever code
  # called the function.
  #
  # We do NOT write a CSV here.
  #
  # The Shiny app can decide what it wants to do with the
  # cleaned data.
  #
  # For example, the Shiny app can:
  #
  #   - use it to make a plot
  #   - let the user download it
  #   - show a summary
  #   - perform additional checks

  return(data)
}
