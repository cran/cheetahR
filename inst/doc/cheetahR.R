## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(cheetahR)
library(palmerpenguins)
library(dplyr)


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


## -----------------------------------------------------------------------------
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
        action = "check",
        style = list(
          uncheckBgColor = "#FDD",
          checkBgColor = "rgb(255, 73, 72)",
          borderColor = "red"
        )
      )
    )
  )


## -----------------------------------------------------------------------------
img_url <- c(
"https://assets.shannons.com.au/JD166AU033OKSAEF/Z8E73PWLE94D365M/7g7i8dxq4xdx0g61-me9867ab4krp0uio/jpg/2000x1500x3/vehicle/1973-mazda-rx4.jpg",
"https://live.staticflickr.com/2529/3854349010_4783baf575_o.jpg",
"https://photos.classiccars.com/cc-temp/listing/129/6646/18583131-1974-datsun-710-std.jpg",
"https://tse2.mm.bing.net/th?id=OIP.qV1ZAVktze35RwUpEgtPmwAAAA&pid=Api&P=0&h=180",
"https://tse1.mm.bing.net/th?id=OIP.OA_V14TTdu8nx36mC6VUeAHaEo&pid=Api&P=0&h=180",
"https://res.cloudinary.com/carsguide/image/upload/f_auto,fl_lossy,q_auto,t_cg_hero_low/v1/editorial/dp/albums/album-3483/lg/Valiant-50-years-_6_.jpg"
)


## ----fig.height=3-------------------------------------------------------------
data <- mutate(head(mtcars), image = img_url, .before = 1) 
cheetah(
  data,
  columns = list(
    rownames = column_def(width = 150),
    image = column_def("images", width = 100, column_type = "image", style = list(imageSizing = "keep-aspect-ratio" ))
  )
)


## -----------------------------------------------------------------------------
cheetah(
  iris,
  columns = list(
    Species = column_def(
      action = "input",
      message = htmlwidgets::JS(
        "function(rec) {
          return {
            type: 'error',
            message: rec.Species === 'setosa' ? 'Invalid specie type.' : null,
          }
        }"
      )
    )
  )
)


## -----------------------------------------------------------------------------
# Prepare data
set.seed(123)
iris_rows <- sample(nrow(iris), 10)
data <- iris[iris_rows, ]


## -----------------------------------------------------------------------------
# Simple cell message
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(type = "info", message = "Ok")
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(message = js_ifelse(Species == "setosa", "", "Invalid"))
    )
  )
)

## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "warning",
        message = js_ifelse(Sepal.Length > 5, "BigSepal", "")
      )
    )
  )
)

## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "warning",
        message = js_ifelse(Sepal.Width <= 3, "NarrowSepal", "WideSepal")
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "info",
        message = js_ifelse(Sepal.Length > 5 & Species %notin% c("setosa"), "E", "X")
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        message = js_ifelse(Species %in% c("setosa", "virginica"), "Bad")
      )
    )
  )
)

## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "info",
        message = js_ifelse(Species %notin% c("setosa"), "OK")
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "info",
        message = js_ifelse(grepl("^vir", Species), "Yes")
      )
    )
  )
)

## -----------------------------------------------------------------------------
cheetah(
  data,
  columns = list(
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "warning",
        message = js_ifelse(!grepl("set", Species), "NoSet", "")
      )
    )
  )
)


## -----------------------------------------------------------------------------
# Add an extra column to the data
check <-
  c(
    "", NA,
    "ok", NA,
    "good", "",
    "", "good",
    "ok", "better"
  )

data <- mutate(data, Check = check, .before = 1)

cheetah(
  data,
  columns = list(
    Check = column_def(
      name = "",
      column_type = "check",
      action = "check",
      message = add_cell_message(
        message = js_ifelse(Check, if_false = "Please check.")
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data[,-1],
  columns = list(
    Sepal.Width = column_def(
      action = "input",
      style = list(textAlign = "center"),
      message = add_cell_message(
        type = "warning",
        message = "rec['Sepal.Width'] <= 3 ? 'NarrowSepal' : 'WideSepal';"
      )
    )
  )
)


## -----------------------------------------------------------------------------
#| fig-height: 3
numeric_data <- data.frame(
  price_USD = c(125000.75, 299.99, 7890.45),
  price_EUR = c(410.25, 18750.60, 1589342.80),
  price_INR = c(2200.50, 134999.99, 945.75),
  price_NGN = c(120000, 2100045, 1750),
  liter = c(20, 35, 42),
  percent = c(0.875, 0.642, 0.238)
)

cheetah(
  numeric_data,
  columns = list(
    price_USD = column_def(
      name = "USD",
      column_type = number_format(
        style = "currency",
        currency = "USD"
      )
    ),
    price_EUR = column_def(
      name = "EUR",
      column_type = number_format(
        style = "currency",
        currency = "EUR",
        locales = "de-DE"
      )
    ),
    price_INR = column_def(
      name = "INR",
      column_type = number_format(
        style = "currency",
        currency = "INR",
        locales = "hi-IN"
      )
    ),
    price_NGN = column_def(
      name = "NGN",
      column_type = number_format(
        style = "currency",
        currency = "NGN"
      )
    ),
    liter = column_def(
      name = "Liter",
      column_type = number_format(
        style = "unit",
        unit = "liter",
        unit_display = "long"
      )
    ),
    percent = column_def(
      name = "Percent",
      column_type = number_format(style = "percent")
    )
  )
)


## -----------------------------------------------------------------------------
#| fig-height: 3
cheetah(
  data.frame(
    date = as.Date(c("2023-01-15", "2023-02-28", "2023-03-10")),
    datetime = as.POSIXct(c(
      "2023-01-15 09:30:00", 
      "2023-02-28 14:45:00",
      "2023-03-10 18:15:00"
    ))
  ),
  columns = list(
    date = column_def(
      name = "Date",
      column_type = date_format(
        locales = "en-US",
        month = "long",
        day = "numeric",
        year = "numeric"
      )
    ),
    datetime = column_def(
      name = "Date & Time",
      column_type = date_format(
        locales = "de-DE",
        date_style = "full",
        time_style = "medium"
      )
    )
  )
)


## ----eval=TRUE----------------------------------------------------------------
cheetah(mtcars, rownames = FALSE, sortable = FALSE)


## -----------------------------------------------------------------------------
cheetah(
  mtcars,
  sortable = FALSE,
  columns = list(
    rownames = column_def(
      width = 150,
      sort = TRUE
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  mtcars,
  sortable = FALSE,
  columns = list(
    rownames = column_def(
      width = 150,
      sort = htmlwidgets::JS(
        "function(order, col, grid) {
          // your logic
        }"
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(
  iris,
  columns = list(
    Sepal.Length = column_def(name = "Length"),
    Sepal.Width = column_def(name = "Width"),
    Petal.Length = column_def(name = "Length"),
    Petal.Width = column_def(name = "Width")
  ),
  column_group = list(
    column_group(
      name = "Sepal",
      columns = c("Sepal.Length", "Sepal.Width"),
      header_style = list(textAlign = "center", bgColor = "#fbd4dd")
    ),
    column_group(
      name = "Petal",
      columns = c("Petal.Length", "Petal.Width"),
      header_style = list(textAlign = "center", bgColor = "#d8edfc")
    )
  )
)


## -----------------------------------------------------------------------------
# Define a named list of styles
user_theme <- list(
  color = "#2c3e50",
  frozenRowsColor = "#2c3e50",
  defaultBgColor = "#ecf0f1",
  frozenRowsBgColor = "#bdc3c7",
  selectionBgColor = "#d0ece7",
  highlightBgColor = "#f9e79f",
  underlayBackgroundColor = "#f4f6f7",
  # This is also possible to change the theme apply in the state by using callback.
  frozenRowsBorderColor = '
  function(args) {
   const { row, grid: { frozenRowCount } } = args;
    if (frozenRowCount - 1 === row) {
     return ["#7f8c8d", "#7f8c8d", "#34495e"];
    } else {
     return "#7f8c8d";
    }
  }',
  borderColor = '
  function(args) {
   const { col, grid: { colCount } } = args;
    if (colCount - 1 === col) {
     return ["#34495e", "#7f8c8d", "#34495e", null];
    } else {
     return ["#34495e", null, "#34495e", null];
    }
  }',
  highlightBorderColor = "#1abc9c",
  checkbox = list(
    uncheckBgColor = "#ecf0f1",
    checkBgColor = "#1abc9c",
    borderColor = "#16a085"
  ),
  font = "14px 'Helvetica Neue', sans-serif",
  header = list(sortArrowColor = "#2980b9"),
  messages = list(
    infoBgColor = "#95a5a6",
    errorBgColor = "#e74c3c",
    warnBgColor = "#f1c40f",
    boxWidth = 12,
    markHeight = 15
  )
)


## -----------------------------------------------------------------------------
cheetah(
  data,
  theme = user_theme,
  columns = list(
    Check = column_def(
      name = "",
      column_type = "check",
      action = "check",
      message = add_cell_message(
        message = js_ifelse(Check, if_false = "Please check.")
      )
    ),
    Petal.Width = column_def(
      action = "input",
      style = list(textAlign = "center"),
      message = add_cell_message(
        type = "info",
        message = js_ifelse(Petal.Width <= 1, "NarrowPetal", "WidePetal")
      )
    ),
    Species = column_def(
      action = "input",
      message = add_cell_message(
        type = "warning",
        message = js_ifelse(!grepl("set", Species), "NoSet", "")
      )
    )
  )
)


## -----------------------------------------------------------------------------
cheetah(penguins, search = "contains")


## -----------------------------------------------------------------------------
# Disable resize effect in a cheetah table
cheetah(penguins, search = "contains", disable_column_resize = TRUE)


## -----------------------------------------------------------------------------
# Freeze the first column
cheetah(penguins, search = "contains", column_freeze = 1)


## -----------------------------------------------------------------------------
# Define default row height and column width
cheetah(penguins, search = "contains", default_row_height = 30, default_col_width = 100)


## -----------------------------------------------------------------------------
# Define default row height for the header
cheetah(penguins, search = "contains", header_row_height = 20)


## -----------------------------------------------------------------------------
# Define a default font setting for the table
cheetah(penguins, search = "contains", font = "8px sans-serif")


## -----------------------------------------------------------------------------
# Make the table editable
cheetah(penguins, search = "contains", editable = TRUE)


## -----------------------------------------------------------------------------
# Move cells on tab and delete cell values on with the Delete and BackSpace keys
cheetah(
  penguins,
  search = "contains",
  editable = TRUE,
  keyboard_options =
    list(
      deleteCellValueOnDel = TRUE,
      moveCellOnTab = TRUE
    )
)


## ----eval=FALSE---------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(cheetahR)
# 
# 
# ui <- page_fluid(cheetahOutput("grid"))
# 
# server <- function(input, output) {
#   output$grid <- renderCheetah({
#     cheetah(data = iris)
#   })
# }
# 
# shinyApp(ui = ui, server = server)


## ----eval=FALSE---------------------------------------------------------------
# library(shiny)
# library(bslib)
# library(cheetahR)
# 
# ui <- page_fluid(cheetahOutput("grid"))
# 
# server <- function(input, output) {
#   output$grid <- renderCheetah({
#     cheetah(data = iris,
#             columns = list(
#               Species = column_def(
#                 column_type = "menu",
#                 action = "inline_menu",
#                 menu_options = list(
#                   setosa = "Option Setosa",
#                   versicolor = "Option Vericolor" ,
#                   virginica = "Option Virginica"
#                 )
#               )
#             )
#           )
#   })
# }
# 
# shinyApp(ui = ui, server = server)

