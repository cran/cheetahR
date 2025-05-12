## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
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
`%notin%` <- Negate('%in%')

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
cheetah(penguins, search = "contains")


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

