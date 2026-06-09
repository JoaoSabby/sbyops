sby_internal_table_detect_integer_type <- function(current_column) {
  .Call("_sbyops_sby_internal_table_detect_integer_type", current_column, PACKAGE = "sbyops")
}

sby_internal_table_detect_numeric_type <- function(current_column) {
  .Call("_sbyops_sby_internal_table_detect_numeric_type", current_column, PACKAGE = "sbyops")
}
