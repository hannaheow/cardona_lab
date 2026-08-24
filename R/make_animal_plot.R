# ============================================================
# FUNCTION: Make Animal Plot
# ============================================================
#
# PURPOSE
# -------
# This function takes the cleaned animal database and creates
# a plot for a measurement selected by the user.
#
# The measurement could be:
#
#   - final_neu_n
#   - final_iba1
#   - or another numeric measurement in the database
#
#
# The function:
#
#   1. Calculates one median value for each animal + region
#   2. Groups all non-diabetic animals together as "Control"
#   3. Calculates the median and IQR for each group
#   4. Creates a plot showing:
#        - Bars = median measurement
#        - Error bars = IQR
#        - Dots = individual animal/region medians
#        - Dot color = sex
#        - Bar color = diet
#        - Separate panels = brain region
#
#
# IMPORTANT
# ---------
# This function does NOT read or write any files.
#
# It receives a cleaned dataset and the name of the
# measurement to plot, and returns a ggplot object.
#
# ============================================================

# -------------------------------
# Load packages
# -------------------------------

library(tidyverse)


# ============================================================
# Define the function
# ============================================================

# "make_animal_plot" is the name of the function.
#
# It has two inputs:
#
#   data:
#       The cleaned animal database.
#
#   outcome:
#       The NAME of the column containing the measurement
#       we want to plot.
#
# For example:
#
#   outcome = "final_neu_n"
#
# or:
#
#   outcome = "final_iba1"

make_animal_plot <- function(
  data,
  outcome
) {
  # ==========================================================
  # STEP 1: Check that the requested measurement exists
  # ==========================================================

  # Before doing any calculations, we want to make sure the
  # requested column actually exists in the dataset.
  #
  # This gives the user a helpful error message instead of
  # allowing R to fail later with a confusing error.

  if (!outcome %in% names(data)) {
    stop(
      paste0(
        "The requested measurement '",
        outcome,
        "' was not found in the dataset."
      )
    )
  }

  # ==========================================================
  # STEP 2: Calculate one value for each animal and region
  # ==========================================================

  # The original data may contain multiple measurements
  # for the same animal and region.
  #
  # We want each dot in the final plot to represent ONE
  # median value for each animal + region combination.
  #
  # The important difference from the previous version is
  # that we are NOT hard-coding "final_neu_n" here.
  #
  # Instead, ".data[[outcome]]" tells R:
  #
  #     "Use whichever column the user selected."
  #
  # If outcome = "final_neu_n", R uses final_neu_n.
  #
  # If outcome = "final_iba1", R uses final_iba1.

  plot_data <- data %>%

    group_by(
      animal_id,
      region,
      diet_group,
      time_point,
      diabetes_status,
      sex
    ) %>%

    summarise(
      # Calculate the median of the selected measurement.
      #
      # .data[[outcome]] means:
      # "look inside the dataset for the column whose
      #  name is stored in the variable called 'outcome'."

      value = median(
        .data[[outcome]],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    # --------------------------------------------------------
    # Create the x-axis grouping
    # --------------------------------------------------------

    # All non-diabetic animals are called "Control".
    #
    # Diabetic animals retain their original time point,
    # such as "4w" or "10w".

    mutate(
      x_group = if_else(
        diabetes_status == "Nondiabetic",
        "Control",
        time_point
      )
    )

  # ==========================================================
  # STEP 3: Calculate the values used for the bars
  # ==========================================================

  # The dots represent individual animals.
  #
  # The bars represent the overall median for each:
  #
  #   - Region
  #   - Control/time-point group
  #   - Diet
  #
  # We also calculate the 25th and 75th percentiles so that
  # we can display the interquartile range (IQR) as the
  # error bars.

  bar_data <- plot_data %>%

    group_by(
      region,
      x_group,
      diet_group
    ) %>%

    summarise(
      # Median of the selected measurement.

      median_value = median(
        value,
        na.rm = TRUE
      ),

      # 25th percentile.

      q1 = quantile(
        value,
        0.25,
        na.rm = TRUE
      ),

      # 75th percentile.

      q3 = quantile(
        value,
        0.75,
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    # --------------------------------------------------------
    # Put the x-axis groups in the desired order
    # --------------------------------------------------------

    # Without this, R may arrange the groups alphabetically.
    #
    # If your experiment changes to use different time points,
    # this is one place that may need to be updated.

    mutate(
      x_group = factor(
        x_group,
        levels = c(
          "Control",
          "10w",
          "4w"
        )
      )
    )

  # ==========================================================
  # STEP 4: Create the plot
  # ==========================================================

  plot <- ggplot() +

    # --------------------------------------------------------
    # Layer 1: Bars
    # --------------------------------------------------------

    # Each bar represents the median of the selected
    # measurement.

    geom_col(
      data = bar_data,

      aes(
        x = x_group,
        y = median_value,
        fill = diet_group
      ),

      position = position_dodge(
        width = 0.75
      ),

      width = 0.65,

      alpha = 0.6
    ) +

    # --------------------------------------------------------
    # Layer 2: Error bars
    # --------------------------------------------------------

    # The error bars represent the IQR:
    #
    #   bottom = 25th percentile
    #   top    = 75th percentile

    geom_errorbar(
      data = bar_data,

      aes(
        x = x_group,
        ymin = q1,
        ymax = q3,
        group = diet_group
      ),

      position = position_dodge(
        width = 0.75
      ),

      width = 0.2
    ) +

    # --------------------------------------------------------
    # Layer 3: Individual animal dots
    # --------------------------------------------------------

    # Each dot represents one animal + region combination.
    #
    # The y-value is the median of the selected measurement
    # for that animal and region.

    geom_jitter(
      data = plot_data,

      aes(
        x = x_group,
        y = value,
        color = sex,
        group = diet_group
      ),

      position = position_jitterdodge(
        jitter.width = 0.12,
        dodge.width = 0.75
      ),

      size = 3,

      alpha = 0.8
    ) +

    # --------------------------------------------------------
    # Separate panels by region
    # --------------------------------------------------------

    facet_wrap(
      ~region
    ) +

    # --------------------------------------------------------
    # Labels and legends
    # --------------------------------------------------------

    labs(
      x = NULL,

      # Use the name of the selected measurement as the
      # y-axis label.
      #
      # For example:
      #
      #   "final_neu_n"
      #
      # or:
      #
      #   "final_iba1"

      y = paste0(
        "Median ",
        outcome
      ),

      fill = "Diet",

      color = "Sex"
    ) +

    # --------------------------------------------------------
    # Plot appearance
    # --------------------------------------------------------

    theme_classic() +

    theme(
      strip.background = element_rect(
        fill = "grey95"
      ),

      strip.text = element_text(
        face = "bold"
      )
    ) +

    # --------------------------------------------------------
    # Color scales
    # --------------------------------------------------------

    # Diet fill: purple tones for bars
    scale_fill_manual(
      values = c(
        "dox_diet" = "#7B68EE", # medium slate blue
        "normal_diet" = "#20B2AA" # light sea green
      )
    ) +

    # Sex color: warm tones for dots, distinct from fill
    scale_color_manual(
      values = c(
        "F" = "#E8735A", # terracotta orange
        "M" = "#F2C14E" # golden yellow
      )
    )

  # ==========================================================
  # STEP 5: Return the plot
  # ==========================================================

  # Give the finished plot back to whoever called the function.

  return(plot)
}
