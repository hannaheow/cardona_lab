library(tidyverse)
library(lubridate)

# Load existing database
animal_database <- read_csv(
  "CardonaLab/data/animal_database.csv",
  show_col_types = FALSE
)


# -------------------------------
# Rename columns
# -------------------------------

animal_database <- animal_database %>%
  rename(
    
    id_sex = `ID & Sex`,
    
    dob = `DOB (YYYY-MM-DD)`,
    
    age_weeks = `Age (weeks)`,
    
    time_point = `Time Point`,
    
    glucose_mg_dl = `Glucose_mg/dL`,
    
    technical_replica = `Technical Replica`,
    
    stain_researcher = `Stain Researcher`,
    
    image_id = `Image ID`,
    
    tissue_type = Tissue_Type,
    
    tissue_processing = Tissue_Processing,
    
    stain_completion_date = stain_completion_date,
    
    imaging_date = imaging_date,
    
    iba1_count = Iba1_count,
    
    neun_count = NeuN_count
    
  ) %>%
  janitor::clean_names()


# -------------------------------
# Convert column types
# -------------------------------

animal_database <- animal_database %>%
  
  mutate(
    
    # dates
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
    
    
    # numeric
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
    
    
    # categorical/text
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


# -------------------------------
# Add missing columns
# -------------------------------

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


missing <- setdiff(
  needed_columns,
  names(animal_database)
)


animal_database[missing] <- NA


# -------------------------------
# Reorder columns
# -------------------------------

animal_database <- animal_database %>%
  select(all_of(needed_columns))


# -------------------------------
# Save cleaned database
# -------------------------------

write_csv(
  animal_database,
  "CardonaLab/data/animal_database_clean.csv"
)
