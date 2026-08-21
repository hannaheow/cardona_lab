# Cardona Lab data collection and visualization 


## Adding Custom Visualizations

This guide covers the basics of adding a custom visualization to the Visualizations tab. Most visualization work happens in two places:

1. The Visualizations tab in the UI which controls what appears on screen
2. The server which prepares the data and creates the plot

The goal is to provide enough structure to modify existing visualizations or add new ones without needing to understand the rest of the app.

### 1. Basic pattern

Most visualizations follow this structure:

```r
df <- database()

df <- df %>%
  filter(...)

ggplot(
  df,
  aes(
    x = ...,
    y = ...
  )
) +
  geom_point() +
  theme_bw()
```

The main questions are:

* Which rows of the database should be included?
* Which columns should be plotted?
* What should go on the x and y axes?
* Should the data be summarized before plotting?

Some of the main database columns are:

```text
animal_id
sex
genotype
diabetes_status
diet_group
time_point
tissue_type
region
glucose_mg_dl
neun_count
iba1_count
technical_replica
image_id
```

These column names can be used directly in plots.

### 2. Adding a new plot

First, add a `plotOutput()` inside the Visualizations tab:

```r
plotOutput(
  "animal_plot",
  height = "600px"
)
```

Then add the corresponding plot code inside `server`:

```r
output$animal_plot <- renderPlot({

  df <- database()

  # Prepare the data here

  ggplot(
    df,
    aes(
      x = ...,
      y = ...
    )
  ) +
    geom_point() +
    theme_bw()

})
```

The name must match in both places:

```text
plotOutput("animal_plot")
        ↓
output$animal_plot
        ↓
ggplot(...)
```

### 3. Example: visualization for an individual animal

An individual animal viewer can be created with a dropdown and a plot.

Add to the Visualizations tab:

```r
selectInput(
  "selected_animal",
  "Animal",
  choices = NULL
)

plotOutput(
  "animal_plot",
  height = "600px"
)
```

Then, inside `server`, populate the dropdown:

```r
observe({

  animals <- sort(
    unique(database()$animal_id)
  )

  updateSelectInput(
    session,
    "selected_animal",
    choices = animals
  )

})
```

Then create the plot:

```r
output$animal_plot <- renderPlot({

  req(input$selected_animal)

  df <- database() %>%
    filter(
      animal_id == input$selected_animal
    )

  ggplot(
    df,
    aes(
      x = region,
      y = iba1_count
    )
  ) +
    geom_jitter(
      width = 0.15,
      size = 3
    ) +
    theme_bw() +
    labs(
      title = paste(
        "IBA1, Animal",
        input$selected_animal
      ),
      x = "Region",
      y = "IBA1 Count"
    )

})
```

This creates a dropdown containing the animal IDs in the database and displays the selected animal's IBA1 measurements by region.

Changing:

```r
y = iba1_count
```

to:

```r
y = neun_count
```

would produce the equivalent NeuN plot.

### 4. Common customizations

Filter the data:

```r
filter(
  tissue_type == "Retina"
)
```

or:

```r
filter(
  diabetes_status == "Diabetic"
)
```

or:

```r
filter(
  animal_id == input$selected_animal
)
```

Multiple filters can be combined:

```r
filter(
  tissue_type == "Retina",
  region == "Retina_Central"
)
```

Change the variables being plotted:

```r
aes(
  x = diabetes_status,
  y = glucose_mg_dl
)
```

or:

```r
aes(
  x = region,
  y = neun_count
)
```

Change the plot type:

```r
geom_point()       # individual points
geom_jitter()      # slightly separated points
geom_boxplot()     # distribution
geom_col()         # bars
geom_line()        # measurements over time
```

These can also be combined:

```r
geom_boxplot() +
geom_jitter(width = 0.1)
```

### 5. Summarizing data before plotting

The database may contain multiple rows for the same animal, region, technical replicate, etc. Depending on the visualization, it may be useful to plot the rows as they are or calculate a summary first.

To plot the existing rows:

```r
df <- database()
```

To calculate one mean value per animal:

```r
df <- database() %>%
  group_by(
    animal_id,
    diabetes_status
  ) %>%
  summarise(
    mean_iba1 = mean(
      iba1_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )
```

The resulting `df` can then be used in `ggplot()` just like any other data frame.

### 6. Recommended workflow

For a new visualization:

1. Decide what should be displayed.
2. Identify the database columns needed.
3. Filter or summarize the data if necessary.
4. Make a simple `ggplot()`.
5. Add the plot to the Visualizations tab with `plotOutput()`.
6. Add dropdowns, checkboxes, or other controls once the basic plot works.

Copying an existing visualization and modifying it is often the easiest approach.

The basic structure is:

```text
database()
    ↓
filter / summarize
    ↓
ggplot()
    ↓
renderPlot()
    ↓
plotOutput()
```
