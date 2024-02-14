def_fonts <- if (Sys.info()["sysname"] == "Windows") {
  "Arial"
} else if (Sys.info()["sysname"] == "Darwin") {
  "Helvetica"
} else {
  "DejaVu Sans"
}

flextable_global <- new.env(parent = emptyenv())
default_flextable_settings <- list(
  font.family = def_fonts,
  cs.family = def_fonts, eastasia.family = def_fonts, hansi.family = def_fonts,
  font.size = 11, font.color = "black",
  text.align = "left", padding.bottom = 5, padding.top = 5,
  padding.left = 5, padding.right = 5,
  line_spacing = 1,
  border.color = "#666666",
  border.width = .75,
  background.color = "transparent",
  table.layout = "fixed",
  decimal.mark = ".",
  big.mark = ",",
  digits = 1,
  pct_digits = 1,
  na_str = "",
  nan_str = "",
  fmt_date = "%Y-%m-%d", fmt_datetime = "%Y-%m-%d %H:%M:%S",
  extra_css = "",
  scroll = NULL,
  split = TRUE, keep_with_next = FALSE,
  tabcolsep = 0, arraystretch = 1.5, float = "none",
  fonts_ignore = FALSE,
  theme_fun = "theme_booktabs",
  post_process_all = function(x) x,
  post_process_pdf = function(x) x,
  post_process_docx = function(x) x,
  post_process_html = function(x) x,
  post_process_pptx = function(x) x
)

flextable_global$defaults <- default_flextable_settings




#' @export
print.flextable_defaults <- function(x, ...) {
  cat("## style properties\n")
  styles <- c(
    "font.family", "hansi.family", "eastasia.family", "cs.family",
    "font.size", "font.color", "text.align", "padding.bottom",
    "padding.top", "padding.left", "padding.right", "line_spacing",
    "border.color", "border.width",
    "background.color"
  )
  df <- data.frame(property = styles, value = unlist(x[styles]), stringsAsFactors = FALSE)
  row.names(df) <- NULL
  print(df)
  cat("\n")

  cat("## cell content settings\n")
  contents <- c(
    "decimal.mark", "big.mark",
    "digits", "pct_digits",
    "na_str", "nan_str", "fmt_date", "fmt_datetime"
  )
  df <- data.frame(property = contents, value = unlist(x[contents]), stringsAsFactors = FALSE)
  row.names(df) <- NULL
  print(df)
  cat("\n")

  cat("## table.layout is:", x$table.layout, "\n")
  if (is.character(x$theme_fun)) cat("## default theme is:", x$theme_fun, "\n")

  invisible(NULL)
}





