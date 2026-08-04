# create dummy data 

library(tidyverse)
library(lubridate)

set.seed(123)

n_animals <- 10
n_images <- 5

animals <- tibble(
  animal_id = paste0("TEST_", sprintf("%03d", 1:n_animals)),
  sex = sample(c("F", "M"), n_animals, replace = TRUE),
  genotype = sample(
    c(
      "CamK2a+/ctFKN+",
      "CamK2a+/ctFKN-",
      "CamK2a-/ctFKN+",
      "CamK2a-/ctFKN-"
    ),
    n_animals,
    replace = TRUE
  ),
  dob = sample(
    seq.Date(
      as.Date("2024-01-01"),
      as.Date("2024-06-01"),
      by = "day"
    ),
    n_animals
  ),
  age_weeks = sample(18:30, n_animals, replace = TRUE),
  diabetes_status = sample(
    c("Diabetic", "Nondiabetic"),
    n_animals,
    replace = TRUE
  ),
  diet_group = sample(
    c("Normal Diet", "Dox Diet"),
    n_animals,
    replace = TRUE
  ),
  time_point = sample(
    c("4w", "10w"),
    n_animals,
    replace = TRUE
  )
)


test_data <- animals %>%
  slice(rep(1:n(), each = n_images)) %>%
  mutate(
    
    id_sex = paste(animal_id, sex),
    
    treatment_group = paste(
      diabetes_status,
      diet_group,
      time_point,
      sep = "-"
    ),
    
    experiment_start_date = as.Date("2026-01-01"),
    experiment_end_date = as.Date("2026-03-01"),
    
    glucose_mg_dl = case_when(
      diabetes_status == "Diabetic" ~ rnorm(n(), 300, 30),
      TRUE ~ rnorm(n(), 150, 20)
    ),
    
    biological_replica = sample(1:5, n(), replace = TRUE),
    
    diet_start_date = as.Date("2026-01-15"),
    diet_end_date = as.Date("2026-03-15"),
    
    tissue_type = sample(
      c("Brain", "Retina"),
      n(),
      replace = TRUE
    ),
    
    tissue_processing = sample(
      c("free_floating", "WM", "OCT"),
      n(),
      replace = TRUE
    ),
    
    date_tissue_sectioning = as.Date("2026-02-01"),
    
    region = sample(
      c(
        "Brain_LGN",
        "Brain_SC",
        "Retina_Central",
        "Retina_Peripheral"
      ),
      n(),
      replace = TRUE
    ),
    
    image_id = sprintf("%03d", 1:n()),
    
    technical_replica = sample(
      c("B1","B2","C1","C2"),
      n(),
      replace = TRUE
    ),
    
    stain_researcher = "Test_User",
    stain_completion_date = as.Date("2026-02-15"),
    
    imagist = "Test_User",
    imaging_date = as.Date("2026-03-01"),
    
    iba1_count = rpois(n(), lambda = 40),
    neun_count = rpois(n(), lambda = 150),
    
    x_confocal = 512,
    y_confocal = 512,
    z_confocal = sample(5:15, n(), replace = TRUE),
    
    final_neu_n = neun_count * runif(n(), .8, 1.2),
    final_iba1 = iba1_count * runif(n(), .8, 1.2),
    
    notes = NA_character_
    
  ) %>%
  select(
    animal_id,
    sex,
    id_sex,
    dob,
    age_weeks,
    genotype,
    treatment_group,
    time_point,
    experiment_start_date,
    experiment_end_date,
    diabetes_status,
    glucose_mg_dl,
    biological_replica,
    diet_group,
    diet_start_date,
    diet_end_date,
    tissue_type,
    tissue_processing,
    date_tissue_sectioning,
    region,
    image_id,
    technical_replica,
    stain_researcher,
    stain_completion_date,
    imagist,
    imaging_date,
    iba1_count,
    neun_count,
    x_confocal,
    y_confocal,
    z_confocal,
    final_neu_n,
    final_iba1,
    notes
  )




write.csv(
  test_data,
  "test_data/animal_database_test.csv",
  row.names = FALSE
)
