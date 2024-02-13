as_struct <- function(nrow, keys, pr, fun) {
  args <- pr
  args$nrow <- nrow
  args$keys <- keys

  borders_id <- c("border.bottom", "border.top", "border.left", "border.right")
  borders_side <- c("bottom", "top", "left", "right")
  avail_bdr_id <- borders_id %in% names(pr)
  bdr_id <- borders_id[avail_bdr_id]
  bdr_side <- borders_side[avail_bdr_id]
  z <- mapply(function(name, fp_b) {
    x <- unclass(fp_b)
    names(x) <- paste("border", names(x), name, sep = ".")
    x
  }, bdr_side, pr[bdr_id], SIMPLIFY = FALSE, USE.NAMES = FALSE)
  z <- Reduce(f = append, z)

  args <- append(args, z)
  args[bdr_id] <- NULL

  obj_str <- do.call(fun, args)
  obj_str
}
