#' Write a list of data frames to a formatted Excel workbook
#'
#' @description Skeleton helper that writes one sheet per data frame with a
#' bold header row, wrapped text, fixed column widths and a frozen header.
#' Used to document data sources and variables (see `dev/make_summary_xlsx.R`).
#'
#' @param sheets named list of data frames; names become sheet names.
#' @param file path of the .xlsx file to write.
#' @param col_width column width applied to all columns.
#' @param overwrite if FALSE (default), error when `file` already exists.
#'
#' @returns `file`, invisibly.
#' @export
write_summary_xlsx <- function(sheets, file, col_width = 32, overwrite = FALSE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for write_summary_xlsx()")
  }
  stopifnot(is.list(sheets), !is.null(names(sheets)), all(vapply(sheets, is.data.frame, logical(1))))

  wb <- openxlsx::createWorkbook()
  hdr <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                               border = "bottom", wrapText = TRUE, valign = "top")
  body <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

  for (nm in names(sheets)) {
    d <- sheets[[nm]]
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, d, headerStyle = hdr)
    openxlsx::addStyle(wb, nm, body, rows = seq_len(nrow(d)) + 1,
                       cols = seq_along(d), gridExpand = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_along(d), widths = col_width)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
  invisible(file)
}
