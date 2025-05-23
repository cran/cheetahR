#' Create a Cheetah Grid widget
#'
#' Creates a high-performance table widget using Cheetah Grid.
#'
#' @param data A data frame or matrix to display
#' @param columns A list of column definitions. Each column can be customized using
#'   \code{column_def()}.
#' @param column_group A list of column groups. Each group can be customized using
#' @param width Width of the widget
#' @param height Height of the widget
#' @param elementId The element ID for the widget
#' @param rownames Logical. Whether to show rownames. Defaults to TRUE.
#' @param search Whether to enable a search field on top of the table.
#' Default to `disabled`. Use `exact` for exact matching
#' or `contains` to get larger matches.
#' @param sortable Logical. Whether to enable sorting on all columns. Defaults to TRUE.
#'
#' @return An HTML widget object of class 'cheetah' that can be:
#'   \itemize{
#'     \item Rendered in R Markdown documents
#'     \item Used in Shiny applications
#'     \item Displayed in R interactive sessions
#'   }
#'   The widget renders as an HTML table with all specified customizations.
#'
#' @import htmlwidgets
#' @import jsonlite
#' @import tibble
#'
#' @export
cheetah <- function(
  data,
  columns = NULL,
  column_group = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL,
  rownames = TRUE,
  search = c("disabled", "exact", "contains"),
  sortable = TRUE
) {
  search <- match.arg(search)
  # Only show rownames if they are character strings (meaningful) and rownames is TRUE
  processed_rn <- process_rownames(data, columns, rownames)

  data <- processed_rn$data
  columns <- processed_rn$columns

  stopifnot(
    is.null(columns) |
      is_named_list(columns) & names(columns) %in% colnames(data)
  )

  stopifnot(
    "If not NULL, `column_groups` must be a named list or list of named lists" =
      is.null(columns) |
      is_named_list(column_group) |
      all(unlist(lapply(column_group, is_named_list)))
  )

  columns <-
    update_col_list_with_classes(data, columns) %>%
    make_table_sortable(sortable = sortable) %>%
    add_field_to_list()

  data_json <- toJSON(data, dataframe = "rows")
  # forward options using x
  x <- list(data = data_json, columns = columns, colGroup = column_group, search = search)

  # create widget
  htmlwidgets::createWidget(
    name = 'cheetah',
    x = x,
    width = width,
    height = height,
    package = 'cheetahR',
    elementId = elementId
  )
}

#' Shiny bindings for cheetah
#'
#' Output and render functions for using cheetah within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a cheetah
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @return \code{cheetahOutput} returns a Shiny output function that can be used in the UI definition.
#'   \code{renderCheetah} returns a Shiny render function that can be used in the server definition.
#'
#' @name cheetah-shiny
#'
#' @export
cheetahOutput <- function(outputId, width = '100%', height = '400px') {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    'cheetah',
    width,
    height,
    package = 'cheetahR'
  )
}

#' @rdname cheetah-shiny
#' @export
renderCheetah <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, cheetahOutput, env, quoted = TRUE)
}
