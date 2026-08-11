# Title: Excel Export (writexl)
# Writes the two ZHOUSE deliverables produced by utils_dwc.R (SPEC §9):
#   1. standardized DwC sheet — one row per record, the input column model
#      preserved, reproducing the ZHOUSE double header (row 1 = PT-BR labels,
#      row 2 = Darwin Core names), then the data rows.
#   2. audit report — two sheets: `auditoria` and `nao_resolvidos`.
# writexl only (no styling), matching the reference scripts.

#' Blank out NA across a data frame so writexl writes empty cells, not "NA"
#' @noRd
blank_na <- function(df) {
    as.data.frame(
        lapply(df, function(col) {
            col <- as.character(col)
            col[is.na(col)] <- ""
            col
        }),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
}

#' Build the double-header frame (PT labels + DwC names + data) for writexl
#'
#' Reproduces the ZHOUSE model: the first written row holds the PT-BR labels, the
#' second the Darwin Core column names, and the rest the data. readxl auto-names
#' (`...3`) for blank PT labels are blanked; the label vector is aligned to the
#' column count.
#'
#' @param dwc_df Standardized output from [build_dwc_output()].
#' @param model_cols Output column order (Darwin Core names).
#' @param pt_labels Optional PT-BR labels aligned to `model_cols`.
#' @return Data frame to write with `col_names = FALSE`.
#' @noRd
dwc_double_header_frame <- function(dwc_df, model_cols = names(dwc_df), pt_labels = NULL) {
    dwc_df <- ensure_columns(dwc_df, model_cols)
    body_df <- blank_na(dwc_df[, model_cols, drop = FALSE])

    if (is.null(pt_labels)) {
        pt_labels <- rep("", length(model_cols))
    } else {
        pt_labels <- as.character(pt_labels)
        pt_labels[is.na(pt_labels)] <- ""
        pt_labels[grepl("^\\.\\.\\.[0-9]+$", pt_labels)] <- ""
        if (length(pt_labels) < length(model_cols)) {
            pt_labels <- c(pt_labels, rep("", length(model_cols) - length(pt_labels)))
        } else if (length(pt_labels) > length(model_cols)) {
            pt_labels <- pt_labels[seq_along(model_cols)]
        }
    }

    body <- as.matrix(body_df)
    dimnames(body) <- NULL
    out <- as.data.frame(
        rbind(unname(pt_labels), unname(as.character(model_cols)), body),
        stringsAsFactors = FALSE
    )
    out
}

#' Write the standardized DwC sheet to .xlsx (double header, no column names)
#'
#' @param dwc_df Standardized output from [build_dwc_output()].
#' @param path Destination .xlsx path.
#' @param model_cols Output column order. Defaults to the data frame columns.
#' @param pt_labels Optional PT-BR labels for the first header row.
#' @return `path`, invisibly.
#' @noRd
write_standardized_xlsx <- function(dwc_df, path, model_cols = names(dwc_df),
                                    pt_labels = NULL) {
    frame <- dwc_double_header_frame(dwc_df, model_cols = model_cols, pt_labels = pt_labels)
    writexl::write_xlsx(frame, path, col_names = FALSE)
    invisible(path)
}

#' Write the audit report (.xlsx) with `auditoria` + `nao_resolvidos` sheets
#'
#' @param audit_tbl Data frame from [build_audit_table()].
#' @param path Destination .xlsx path.
#' @param unresolved Optional pre-computed unresolved subset; derived when NULL.
#' @return `path`, invisibly.
#' @noRd
write_audit_xlsx <- function(audit_tbl, path, unresolved = NULL) {
    if (is.null(unresolved)) {
        unresolved <- audit_unresolved(audit_tbl)
    }
    writexl::write_xlsx(
        list(
            auditoria = blank_na(audit_tbl),
            nao_resolvidos = blank_na(unresolved)
        ),
        path
    )
    invisible(path)
}
