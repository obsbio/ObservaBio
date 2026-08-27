# Title: Species-table IO
# Reads the ObservaBio species table from .xlsx/.csv/.tsv/.txt. The header row is
# DETECTED (the first row carrying a `scientificName` cell), not fixed at row 2,
# so the reader accepts the ObservaBio double-header model (PT-BR labels on row 1,
# Darwin Core names on row 2, data from row 3) as well as a plain single-header
# sheet or one with preamble rows above the header. Delimited text sniffs its
# separator (tab/;/,/|) and strips a UTF-8 BOM. Only base + readxl — no new deps.

#' The scientific-name column that anchors header detection (Darwin Core).
#' @noRd
.ObservaBio_name_key <- function() "scientificName"

#' Count occurrences of a literal delimiter in one line.
#' @noRd
.count_delim <- function(line, delim) {
    lengths(regmatches(line, gregexpr(delim, line, fixed = TRUE)))
}

#' Guess the field delimiter of a delimited-text file from its lines
#'
#' Picks, among tab/;/,/|, the one with the highest median count per line. Comma
#' loses to semicolon on PT-BR exports (where "," is the decimal mark), and
#' commas inside quoted fields only over-count, which is harmless downstream.
#'
#' @param lines Character vector of file lines (BOM already stripped).
#' @return A single-character delimiter string.
#' @noRd
sniff_delimiter <- function(lines) {
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0L) {
        return(",")
    }
    probe <- utils::head(lines, 20L)
    delims <- c("\t", ";", ",", "|")
    score <- vapply(delims, function(d) {
        stats::median(vapply(probe, .count_delim, integer(1), delim = d))
    }, numeric(1))
    if (all(score == 0)) {
        return(",")
    }
    delims[[which.max(score)]]
}

#' Read a delimited-text file into a raw character grid (no header assumption)
#' @noRd
read_text_grid <- function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    if (length(lines) > 0L) {
        lines[[1]] <- sub("^\uFEFF", "", lines[[1]])  # strip a UTF-8 BOM
    }
    lines <- lines[!is.na(lines)]
    if (length(lines) == 0L || !any(nzchar(trimws(lines)))) {
        stop("O arquivo está vazio.")
    }
    delim <- sniff_delimiter(lines)
    # Fix the column count from the widest line so preamble rows with fewer
    # fields do not confuse read.table's auto-detection (over-count from quoted
    # delimiters is harmless — extra blank columns are dropped below).
    ncol_max <- max(vapply(lines, function(ln) .count_delim(ln, delim) + 1L, integer(1)))
    grid <- utils::read.table(
        text = lines, sep = delim, header = FALSE, quote = "\"",
        colClasses = "character", fill = TRUE, check.names = FALSE,
        comment.char = "", col.names = paste0("V", seq_len(ncol_max)),
        stringsAsFactors = FALSE, na.strings = character(0)
    )
    as.data.frame(grid, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read an .xlsx into a raw character grid (all rows as data, character-typed)
#' @noRd
read_xlsx_grid <- function(path) {
    raw <- suppressMessages(readxl::read_excel(
        path, col_names = FALSE, .name_repair = "minimal"
    ))
    grid <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
    grid[] <- lapply(grid, as.character)
    grid
}

#' Find the header row (1-based) in a raw grid
#'
#' The header is the first row (scanning from the top) that carries a cell equal
#' to the name key — this locates the data-start row without assuming a fixed
#' layout. Returns `NA` when no such row is in the first rows scanned.
#'
#' @param grid Raw character grid (rows = file rows).
#' @param key Column name that anchors the header (default `scientificName`).
#' @param max_scan Rows to scan from the top.
#' @return Integer header-row index, or `NA_integer_`.
#' @noRd
detect_header_row <- function(grid, key = .ObservaBio_name_key(), max_scan = 30L) {
    n <- min(nrow(grid), max_scan)
    for (i in seq_len(n)) {
        cells <- trimws(as.character(unlist(grid[i, ], use.names = FALSE)))
        if (any(!is.na(cells) & cells == key)) {
            return(i)
        }
    }
    NA_integer_
}

#' Read a ObservaBio species table (.xlsx/.csv/.tsv/.txt), auto-detecting the header
#'
#' @param path Path to the species table.
#' @param key Column name that anchors header detection (default
#'   `scientificName`).
#' @return List with `records` (character data frame, one row per record),
#'   `model_cols` (column names from the detected header, in order) and
#'   `pt_labels` (the row directly above the header — the PT-BR labels of the
#'   double-header model — or `NULL` when the header is the first row).
#' @noRd
read_ObservaBio_table <- function(path, key = .ObservaBio_name_key()) {
    if (!file.exists(path)) {
        stop(sprintf("Spreadsheet not found: %s", path))
    }
    ext <- tolower(tools::file_ext(path))
    grid <- switch(
        ext,
        xlsx = read_xlsx_grid(path),
        xls  = read_xlsx_grid(path),
        csv = ,
        tsv = ,
        txt = read_text_grid(path),
        stop(sprintf(
            "Formato não suportado: '.%s'. Use .xlsx, .csv, .tsv ou .txt.", ext
        ))
    )

    hdr <- detect_header_row(grid, key = key)
    if (is.na(hdr)) {
        stop(sprintf(
            paste0("Não encontrei a linha de cabeçalho com a coluna '%s'. ",
                   "Confira se a planilha tem os nomes Darwin Core no cabeçalho."),
            key
        ))
    }

    header <- trimws(as.character(unlist(grid[hdr, ], use.names = FALSE)))
    pt_row <- if (hdr > 1L) as.character(unlist(grid[hdr - 1L, ], use.names = FALSE)) else NULL
    # Keep only named columns; align pt labels to the same columns.
    keep <- !is.na(header) & nzchar(header)
    header <- header[keep]
    if (!is.null(pt_row)) pt_row <- pt_row[keep]

    body <- if (hdr < nrow(grid)) {
        grid[(hdr + 1L):nrow(grid), keep, drop = FALSE]
    } else {
        grid[0L, keep, drop = FALSE]
    }
    records <- as.data.frame(body, stringsAsFactors = FALSE, check.names = FALSE)
    names(records) <- header
    records[] <- lapply(records, as.character)
    names(records) <- trimws(names(records))
    model_cols <- names(records)

    # A stable DwC pipeline works on character throughout; drop all-blank rows.
    if (nrow(records) > 0L) {
        mat <- as.matrix(records)
        keep_row <- apply(mat, 1L, function(r) any(!is.na(r) & nzchar(trimws(r))))
        records <- records[keep_row, , drop = FALSE]
        rownames(records) <- NULL
    }

    list(
        records = records,
        model_cols = model_cols,
        pt_labels = if (is.null(pt_row)) NULL else as.character(pt_row)
    )
}
