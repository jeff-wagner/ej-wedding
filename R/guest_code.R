# ---------------------------------------------------------------------------
# RSVP guest code
#
# Guests must enter the code printed on their invitation before the RSVP form
# appears. Codes live in the RSVP_GUEST_CODE environment variable (see
# .Renviron.example), not in the code, so they stay out of the repository.
# Use one shared code, or several comma-separated codes (e.g. per household):
#   RSVP_GUEST_CODE=PHALAROPE
#   RSVP_GUEST_CODE=SMITH27,JONES27,LEE27
# Matching ignores case, spaces and dashes. If no code is set, the form stays
# locked for everyone.
# ---------------------------------------------------------------------------

max_code_attempts <- 5  # per browser session

normalize_code <- function(x) {
  if (is.null(x)) x <- ""
  toupper(gsub("[[:space:]-]", "", x))
}

guest_codes <- function() {
  codes <- normalize_code(strsplit(Sys.getenv("RSVP_GUEST_CODE"), ",", fixed = TRUE)[[1]])
  codes[nzchar(codes)]
}

# Returns the matched (normalized) code, or NULL if it doesn't match
check_guest_code <- function(entered) {
  entered <- normalize_code(entered)
  if (!nzchar(entered) || !entered %in% guest_codes()) return(NULL)
  entered
}

if (site$rsvp_open && !length(guest_codes())) {
  warning("RSVPs are open but RSVP_GUEST_CODE is not set; nobody can RSVP.", call. = FALSE)
}
