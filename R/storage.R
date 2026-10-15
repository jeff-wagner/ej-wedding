# ---------------------------------------------------------------------------
# RSVP storage
#
# Default backend: Google Sheets (persists on shinyapps.io / Connect Cloud,
# where local files are wiped on restart). Set rsvp_backend <- "csv" to save
# to data/rsvps.csv instead, e.g. for local development.
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
#   The header row is written automatically on the first RSVP if the sheet
#   is empty.
# ---------------------------------------------------------------------------

rsvp_backend <- "gsheet"   # "gsheet" or "csv"
rsvp_tab     <- 1          # worksheet (tab) name or position within the sheet
rsvp_file    <- file.path("data", "rsvps.csv")

save_rsvp <- function(row) {
  stopifnot(is.data.frame(row), nrow(row) == 1)
  switch(rsvp_backend,
    csv    = save_rsvp_csv(row),
    gsheet = save_rsvp_gsheet(row),
    stop("Unknown rsvp_backend: ", rsvp_backend)
  )
}

# --- CSV backend ------------------------------------------------------------
save_rsvp_csv <- function(row) {
  dir.create(dirname(rsvp_file), showWarnings = FALSE, recursive = TRUE)
  new_file <- !file.exists(rsvp_file)
  utils::write.table(
    row, rsvp_file, sep = ",", row.names = FALSE,
    col.names = new_file, append = !new_file, qmethod = "double"
  )
  invisible(TRUE)
}

# --- Google Sheets backend --------------------------------------------------
gs_state <- new.env(parent = emptyenv())
gs_state$authed <- FALSE
gs_state$header_ok <- FALSE

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
  gsheet_auth()
  ss <- googlesheets4::as_sheets_id(Sys.getenv("RSVP_SHEET_ID"))
  if (gsheet_check_header(ss, row) == "written") return(invisible(TRUE))
  googlesheets4::sheet_append(ss, row, sheet = rsvp_tab)
  invisible(TRUE)
}

# Warn in the server log at startup rather than on a guest's first submit
if (rsvp_backend == "gsheet" && length(gsheet_config_problems())) {
  warning("RSVP backend is 'gsheet' but: ",
          paste(gsheet_config_problems(), collapse = "; "), call. = FALSE)
}
