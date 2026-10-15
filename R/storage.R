# ---------------------------------------------------------------------------
# Storage: RSVPs, the community board, and the couple's planning tracker
#
# Default backend: Google Sheets (persists on shinyapps.io / Connect Cloud,
# where local files are wiped on restart). Set the STORAGE_BACKEND environment
# variable to "csv" to save to CSV files in data/ instead, e.g. for local
# development.
#
# Google Sheets setup (one time):
#   1. In Google Cloud Console, create a project, enable the Google Sheets API,
#      create a service account, and download its JSON key.
#   2. Create a Google Sheet and share it (Editor) with the service account's
#      email (the "client_email" field in the JSON key).
#   3. Set two environment variables (see .Renviron.example):
#        RSVP_SHEET_ID              - the long ID in the sheet's URL
#        GS4_SERVICE_ACCOUNT_JSON   - path to the JSON key file, OR the JSON
#                                     text itself (handy for hosts that only
#                                     offer env vars, like Connect Cloud)
#   RSVPs go in the first tab; its header row is written automatically on the
#   first RSVP if the sheet is empty. The board and planning tracker use their
#   own tabs ("board", "vendors", "budget"), created on first use.
# ---------------------------------------------------------------------------

storage_backend <- Sys.getenv("STORAGE_BACKEND", "gsheet")   # "gsheet" or "csv"
rsvp_tab        <- 1          # worksheet (tab) name or position within the sheet
data_dir        <- "data"
rsvp_file       <- file.path(data_dir, "rsvps.csv")

save_rsvp <- function(row) {
  stopifnot(is.data.frame(row), nrow(row) == 1)
  switch(storage_backend,
    csv    = csv_append(rsvp_file, row),
    gsheet = save_rsvp_gsheet(row),
    stop("Unknown STORAGE_BACKEND: ", storage_backend)
  )
}

# --- Tables for the board and planning tracker ------------------------------
# Every column is stored and read back as text; missing values come back "".
table_columns <- list(
  board   = c("id", "timestamp", "parent_id", "name", "message", "couple", "hidden"),
  vendors = c("id", "category", "vendor", "contact", "email", "phone", "status",
              "estimate", "cost", "paid", "due_date", "notes", "updated"),
  budget  = c("category", "amount")
)

empty_table <- function(name) {
  cols <- table_columns[[name]]
  as.data.frame(setNames(rep(list(character(0)), length(cols)), cols),
                stringsAsFactors = FALSE)
}

clean_table <- function(df, name) {
  cols <- table_columns[[name]]
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (col in setdiff(cols, names(df))) df[[col]] <- rep("", nrow(df))
  df <- df[cols]
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  rownames(df) <- NULL
  df
}

new_id <- function() paste(sample(c(0:9, letters[1:6]), 12, replace = TRUE), collapse = "")

# Reads are cached per server process so a busy board doesn't hit the Sheets
# API on every page view. Pass max_age = 0 to always read fresh.
table_cache <- new.env(parent = emptyenv())

read_table <- function(name, max_age = 30) {
  hit <- table_cache[[name]]
  if (!is.null(hit) && as.numeric(Sys.time()) - hit$at < max_age) return(hit$df)
  raw <- switch(storage_backend,
    csv    = csv_read(table_file(name)),
    gsheet = read_table_gsheet(name),
    stop("Unknown STORAGE_BACKEND: ", storage_backend)
  )
  df <- clean_table(raw, name)
  table_cache[[name]] <- list(df = df, at = as.numeric(Sys.time()))
  df
}

append_row <- function(name, row) {
  row <- clean_table(row, name)
  stopifnot(nrow(row) == 1)
  switch(storage_backend,
    csv    = csv_append(table_file(name), row),
    gsheet = append_row_gsheet(name, row),
    stop("Unknown STORAGE_BACKEND: ", storage_backend)
  )
  table_cache[[name]] <- NULL
  invisible(TRUE)
}

# Replaces the whole table (used for edits and deletes)
write_table <- function(name, df) {
  df <- clean_table(df, name)
  switch(storage_backend,
    csv    = csv_write(table_file(name), df),
    gsheet = write_table_gsheet(name, df),
    stop("Unknown STORAGE_BACKEND: ", storage_backend)
  )
  table_cache[[name]] <- list(df = df, at = as.numeric(Sys.time()))
  invisible(TRUE)
}

# --- CSV backend ------------------------------------------------------------
table_file <- function(name) file.path(data_dir, paste0(name, ".csv"))

csv_read <- function(path) {
  if (!file.exists(path)) return(data.frame())
  utils::read.csv(path, colClasses = "character", check.names = FALSE)
}

csv_append <- function(path, row) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  new_file <- !file.exists(path)
  utils::write.table(
    row, path, sep = ",", row.names = FALSE,
    col.names = new_file, append = !new_file, qmethod = "double"
  )
  invisible(TRUE)
}

csv_write <- function(path, df) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, path, row.names = FALSE)
  invisible(TRUE)
}

# --- Google Sheets backend --------------------------------------------------
gs_state <- new.env(parent = emptyenv())
gs_state$authed <- FALSE
gs_state$header_ok <- FALSE
gs_state$tabs_ok <- character(0)

gsheet_config_problems <- function() {
  c(
    if (!requireNamespace("googlesheets4", quietly = TRUE))
      "package 'googlesheets4' is not installed",
    if (!nzchar(Sys.getenv("RSVP_SHEET_ID")))
      "RSVP_SHEET_ID is not set",
    if (!nzchar(Sys.getenv("GS4_SERVICE_ACCOUNT_JSON")))
      "GS4_SERVICE_ACCOUNT_JSON is not set"
  )
}

gsheet_auth <- function() {
  if (gs_state$authed) return(invisible(TRUE))
  problems <- gsheet_config_problems()
  if (length(problems)) stop("Google Sheets not configured: ", paste(problems, collapse = "; "))
  # `path` accepts a file path or a JSON string
  googlesheets4::gs4_auth(path = Sys.getenv("GS4_SERVICE_ACCOUNT_JSON"))
  gs_state$authed <- TRUE
  invisible(TRUE)
}

gsheet_ss <- function() {
  gsheet_auth()
  googlesheets4::as_sheets_id(Sys.getenv("RSVP_SHEET_ID"))
}

# sheet_append() writes values by position, so make sure the sheet's header
# matches our columns. An empty sheet gets the header written for it.
gsheet_check_header <- function(ss, row) {
  if (gs_state$header_ok) return("ok")
  existing <- suppressMessages(googlesheets4::read_sheet(ss, sheet = rsvp_tab, n_max = 0))
  if (ncol(existing) == 0) {
    googlesheets4::sheet_write(row, ss, sheet = rsvp_tab)  # header + this row
    gs_state$header_ok <- TRUE
    return("written")
  }
  if (!identical(names(existing), names(row))) {
    stop("RSVP sheet header doesn't match. Expected: ",
         paste(names(row), collapse = ", "),
         " | Found: ", paste(names(existing), collapse = ", "))
  }
  gs_state$header_ok <- TRUE
  "ok"
}

save_rsvp_gsheet <- function(row) {
  ss <- gsheet_ss()
  if (gsheet_check_header(ss, row) == "written") return(invisible(TRUE))
  googlesheets4::sheet_append(ss, row, sheet = rsvp_tab)
  invisible(TRUE)
}

gsheet_write_header <- function(ss, name) {
  header <- as.data.frame(t(table_columns[[name]]))
  suppressMessages(googlesheets4::range_write(
    ss, header, sheet = name, range = "A1", col_names = FALSE, reformat = FALSE
  ))
}

# Create the tab (with its header row) if needed, and refuse to touch a tab
# whose header doesn't match what the app expects
gsheet_ensure_tab <- function(ss, name) {
  if (name %in% gs_state$tabs_ok) return(invisible(TRUE))
  if (!name %in% googlesheets4::sheet_names(ss)) {
    suppressMessages(googlesheets4::sheet_add(ss, name))
  }
  existing <- suppressMessages(googlesheets4::read_sheet(ss, sheet = name, n_max = 0))
  if (ncol(existing) == 0) {
    gsheet_write_header(ss, name)
  } else if (!identical(names(existing), table_columns[[name]])) {
    stop("'", name, "' tab header doesn't match. Expected: ",
         paste(table_columns[[name]], collapse = ", "),
         " | Found: ", paste(names(existing), collapse = ", "))
  }
  gs_state$tabs_ok <- c(gs_state$tabs_ok, name)
  invisible(TRUE)
}

read_table_gsheet <- function(name) {
  ss <- gsheet_ss()
  gsheet_ensure_tab(ss, name)
  suppressMessages(googlesheets4::read_sheet(ss, sheet = name, col_types = "c"))
}

append_row_gsheet <- function(name, row) {
  ss <- gsheet_ss()
  gsheet_ensure_tab(ss, name)
  suppressMessages(googlesheets4::sheet_append(ss, row, sheet = name))
}

write_table_gsheet <- function(name, df) {
  ss <- gsheet_ss()
  gsheet_ensure_tab(ss, name)
  if (nrow(df)) {
    suppressMessages(googlesheets4::sheet_write(df, ss, sheet = name))
  } else {
    suppressMessages(googlesheets4::range_clear(ss, sheet = name))
    gsheet_write_header(ss, name)
  }
}

# Warn in the server log at startup rather than on a guest's first submit
if (storage_backend == "gsheet" && length(gsheet_config_problems())) {
  warning("STORAGE_BACKEND is 'gsheet' but: ",
          paste(gsheet_config_problems(), collapse = "; "), call. = FALSE)
}
