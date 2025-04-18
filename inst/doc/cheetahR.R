## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)


## ----setup--------------------------------------------------------------------
library(cheetahR)


## -----------------------------------------------------------------------------
# Render table
cheetah(iris)


## -----------------------------------------------------------------------------
# Change some feature of some columns in the data
cheetah(
  iris,
  columns = list(
    Sepal.Length = column_def(name = "Sepal_Length", width = 120),
    Sepal.Width = column_def(name = "Sepal_Width", width = 120),
    Petal.Length = column_def(name = "Petal_Length", width = 120),
    Petal.Width = column_def(name = "Petal_Width", width = 120),
    Species = column_def(name = "Species")
  )
)


## -----------------------------------------------------------------------------
# Example of customizing rownames with color and width
cheetah(
  mtcars,
  columns = list(
    rownames = column_def(width = 150, style = list(color = "red"))
  )
)


## -----------------------------------------------------------------------------
# Using checkbox column type to indicate NA values
head(airquality, 10) %>%
  mutate(
    has_na = if_any(everything(), is.na),
    has_na = ifelse(has_na, "true", "false"),
    .before = 1
  ) %>%
  cheetah(
    columns = list(
      has_na = column_def(
        name = "Contains NA",
        column_type = "check",
        style = list(
          uncheckBgColor = "#FDD",
          checkBgColor = "rgb(255, 73, 72)",
          borderColor = "red"
        )
      )
    )
  )

