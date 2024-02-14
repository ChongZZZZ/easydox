# cell_struct -----
cell_struct <- function(nrow, keys,
                        vertical.align = "top", text.direction = "lrtb",
                        margin.bottom = 0, margin.top = 0,
                        margin.left = 0, margin.right = 0,
                        border.width.bottom = 1, border.width.top = 1, border.width.left = 1, border.width.right = 1,
                        border.color.bottom = "transparent", border.color.top = "transparent", border.color.left = "transparent", border.color.right = "transparent",
                        border.style.bottom = "solid", border.style.top = "solid", border.style.left = "solid", border.style.right = "solid",
                        background.color = "#34CC27", width = NA_real_, height = NA_real_, hrule = "auto",
                        ...) {
  check_choice(value = vertical.align, choices = c("top", "center", "bottom"))
  check_choice(value = text.direction, choices = c("lrtb", "tbrl", "btlr"))

  x <- list(
    vertical.align = fpstruct(nrow = nrow, keys = keys, default = vertical.align),
    width = fpstruct(nrow = nrow, keys = keys, default = width),
    height = fpstruct(nrow = nrow, keys = keys, default = height),
    margin.bottom = fpstruct(nrow = nrow, keys = keys, default = margin.bottom),
    margin.top = fpstruct(nrow = nrow, keys = keys, default = margin.top),
    margin.left = fpstruct(nrow = nrow, keys = keys, default = margin.left),
    margin.right = fpstruct(nrow = nrow, keys = keys, default = margin.right),
    border.width.bottom = fpstruct(nrow = nrow, keys = keys, default = border.width.bottom),
    border.width.top = fpstruct(nrow = nrow, keys = keys, default = border.width.top),
    border.width.left = fpstruct(nrow = nrow, keys = keys, default = border.width.left),
    border.width.right = fpstruct(nrow = nrow, keys = keys, default = border.width.right),
    border.color.bottom = fpstruct(nrow = nrow, keys = keys, default = border.color.bottom),
    border.color.top = fpstruct(nrow = nrow, keys = keys, default = border.color.top),
    border.color.left = fpstruct(nrow = nrow, keys = keys, default = border.color.left),
    border.color.right = fpstruct(nrow = nrow, keys = keys, default = border.color.right),
    border.style.bottom = fpstruct(nrow = nrow, keys = keys, default = border.style.bottom),
    border.style.top = fpstruct(nrow = nrow, keys = keys, default = border.style.top),
    border.style.left = fpstruct(nrow = nrow, keys = keys, default = border.style.left),
    border.style.right = fpstruct(nrow = nrow, keys = keys, default = border.style.right),
    text.direction = fpstruct(nrow = nrow, keys = keys, default = text.direction),
    background.color = fpstruct(nrow = nrow, keys = keys, default = background.color),
    hrule = fpstruct(nrow = nrow, keys = keys, default = hrule)
  )
  class(x) <- "cell_struct"
  x
}

add_rows.cell_struct <- function(x, nrows, first, ...) {
  for (i in seq_len(length(x))) {
    x[[i]] <- add_rows(x[[i]], nrows, first = first)
  }
  x
}


cell_struct_to_df <- function(object, ...) {
  data <- lapply(object, function(x) {
    as.vector(x$data)
  })

  data$ft_row_id <- rep(seq_len(nrow(object$background.color$data)), ncol(object$background.color$data))
  data$col_id <- rep(object$background.color$keys, each = nrow(object$background.color$data))
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$col_id <- factor(data$col_id, levels = object$background.color$keys)
  data
}

# chunkset_struct ---------------------------------------------------------

# This object is used to capture paragraphs of a part of a flextable
# It is a matrix, each column is a colkey, each row is a row
# It contains paragraphs, paragraphs are made of chunks
new_chunkset_struct <- function(col_keys, data) {
  chunkdata <- fpstruct(nrow = nrow(data), keys = col_keys, default = as_paragraph(as_chunk("")))
  class(chunkdata) <- c("chunkset_struct")

  if (nrow(data) > 0) {
    newchunkdata <- lapply(
      data[col_keys],
      function(x) {
        as_paragraph(as_chunk(x, formatter = format_fun.default))
      }
    )
    newchunkdata <- matrix(
      do.call(c, newchunkdata),
      ncol = length(col_keys),
      dimnames = list(NULL, col_keys))
    chunkdata <- set_chunkset_struct_element(
      x = chunkdata,
      i = seq_len(nrow(data)),
      j = col_keys,
      value = newchunkdata)
  }
  chunkdata
}

add_rows_to_chunkset_struct <- function(x, nrows, first, data, ...) {
  names_ <- names(data)
  stopifnot(!is.null(names_))

  x <- add_rows_fpstruct(x, nrows, first = first, default = as_paragraph(as_chunk("")))
  if (first) {
    id <- seq_len(nrows)
  } else {
    id <- rev(rev(seq_len(x$nrow))[seq_len(nrows)])
  }

  newchunkdata <- lapply(data[x$keys], function(x) as_paragraph(as_chunk(x, formatter = format_fun.default)))
  newchunkdata <- matrix(
    do.call(c, newchunkdata),
    ncol = length(x$keys),
    dimnames = list(NULL, x$keys))

  x <- set_chunkset_struct_element(
    x = x,
    i = id, j = x$keys, value = newchunkdata)
  x
}

as_chunkset_struct <- function(l_paragraph, keys, i = NULL) {
  if (!is.null(i) &&
      length(l_paragraph) == length(i) &&
      length(keys) > 1) {
    l_paragraph <- rep(l_paragraph, length(keys))
  }
  matrix(
    data = l_paragraph,
    ncol = length(keys),
    dimnames = list(NULL, keys)
  )
}

is_paragraph <- function(x) {
  chunk_str_names <- c("txt", "font.size", "italic", "bold", "underlined", "color",
                       "shading.color", "font.family", "hansi.family", "eastasia.family",
                       "cs.family", "vertical.align", "width", "height", "url", "eq_data",
                       "word_field_data", "img_data",
                       ".chunk_index")
  is.data.frame(x) &&
    all(colnames(x) %in% chunk_str_names)

}
set_chunkset_struct_element <- function(x, i, j, value) {

  names_ <- colnames(value)
  stopifnot(
    is.matrix(value),
    !is.null(names_),
    mode(value) == "list",
    all(sapply(value, is_paragraph)),
    all(names_ %in% x$keys)
  )

  x$data[i, j] <- value
  x
}

append_chunkset_struct_element <- function(x, i, j, chunk_data, last = TRUE) {
  chunk_str_names <- c("txt", "font.size", "italic", "bold", "underlined", "color",
                   "shading.color", "font.family", "hansi.family", "eastasia.family",
                   "cs.family", "vertical.align", "width", "height", "url", "eq_data",
                   "word_field_data", "img_data")
  stopifnot(
    is.data.frame(chunk_data),
    all(chunk_str_names %in% colnames(chunk_data))
  )
  chunk_data <- chunk_data[, chunk_str_names, drop = FALSE]

  chunk_data_length <- nrow(chunk_data)
  i_length <- length(i)
  j_length <- length(j)
  expected_length <- j_length * i_length

  if (chunk_data_length == 1L && i_length != chunk_data_length) {
    chunk_data <- rep(list(chunk_data), i_length)
    chunk_data <- rbind_match_columns(chunk_data)
  }

  if (expected_length / nrow(chunk_data) == j_length) {
    chunk_data <- rep(list(chunk_data), j_length)
    chunk_data <- rbind_match_columns(chunk_data)
  }

  stopifnot(nrow(chunk_data) == expected_length)

  if (nrow(chunk_data) == 1) {
    chunk_data <- list(chunk_data)
  } else {
    chunk_data <- split(chunk_data, seq_len(expected_length))
    names(chunk_data) <- NULL
  }

  values <- get_chunkset_struct_element(x, i = i, j = j)
  values <- do.call(c, apply(values, 2, function(x) x))
  names(values) <- NULL

  values <- mapply(
    function(x, y, last = TRUE) {
      if (last) {
        y$.chunk_index <- max(x$.chunk_index, na.rm = TRUE) + 1
        x <- rbind_match_columns(list(x, y))
      } else {
        y$.chunk_index <- min(x$.chunk_index, na.rm = TRUE) - 1
        x <- rbind_match_columns(list(y, x))
      }
      x$.chunk_index <- rleid(x$.chunk_index)
      x
    },
    x = values,
    y = chunk_data, SIMPLIFY = FALSE,
    MoreArgs = list(last = last)
  )

  x$data[i, j] <- values
  x
}

get_chunkset_struct_element <- function(x, i, j) {
  x$data[i, j, drop = FALSE]
}


replace_missing_fptext_by_default <- function(x, default) {
  by_columns <- c(
    "font.size", "italic", "bold", "underlined", "color", "shading.color",
    "font.family", "hansi.family", "eastasia.family", "cs.family",
    "vertical.align"
  )

  keys <- default[, setdiff(names(default), by_columns), drop = FALSE]
  values <- default[, by_columns, drop = FALSE]
  names(values) <- paste0(by_columns, "_default")
  defdata <- cbind(keys, values)

  newx <- x
  setDT(newx)
  setDT(defdata)
  newx <- newx[defdata, on = names(keys)]
  setDF(newx)
  for (j in by_columns) {
    if (!is.null(newx[[j]])) {
      newx[[j]] <- ifelse(is.na(newx[[j]]), newx[[paste0(j, "_default")]], newx[[j]])
    } else {
      newx[[j]] <- newx[[paste0(j, "_default")]]
    }
    newx[[paste0(j, "_default")]] <- NULL
  }
  newx
}

