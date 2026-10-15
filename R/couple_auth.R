# ---------------------------------------------------------------------------
# Couple sign-in for the private planning area
# (Sourced after content.R, alphabetically, so `site` is available here.)
#
# Each of you has your own password, set in an environment variable named
# after the names in R/content.R (see .Renviron.example):
#   ADMIN_PASSWORD_EMILY=...
#   ADMIN_PASSWORD_JEFF=...
# To sign in, add ?couple to the end of the site's address. There's no link
# for guests to find, and the Planning tab only exists for a signed-in browser.
# Passwords shorter than 10 characters are ignored.
# ---------------------------------------------------------------------------

max_admin_attempts   <- 5        # per browser session
admin_global_limit   <- 20       # failed tries across all visitors...
admin_global_window  <- 15 * 60  # ...within this many seconds locks sign-in

admin_password_var <- function(name) {
  paste0("ADMIN_PASSWORD_", toupper(gsub("[^A-Za-z0-9]", "", name)))
}

admin_passwords <- function() {
  people <- c(site$partner1, site$partner2)
  pw <- vapply(people, function(p) Sys.getenv(admin_password_var(p)), "")
  pw[nchar(pw) >= 10]
}

admin_failures <- new.env(parent = emptyenv())
admin_failures$times <- numeric(0)

admin_locked_out <- function() {
  now <- as.numeric(Sys.time())
  admin_failures$times <- admin_failures$times[now - admin_failures$times < admin_global_window]
  length(admin_failures$times) >= admin_global_limit
}

# Returns the partner's name, or NULL
check_admin_password <- function(entered) {
  entered <- entered %||% ""
  pw <- admin_passwords()
  match <- names(pw)[pw == entered & nzchar(entered)]
  if (length(match) == 1) return(match)
  admin_failures$times <- c(admin_failures$times, as.numeric(Sys.time()))
  NULL
}

admin_login_modal <- function(error = NULL, locked = FALSE) {
  modalDialog(
    title = "Sign in",
    if (locked) {
      p("Too many tries. Please wait a while and reload the page.")
    } else {
      tagList(
        passwordInput("admin_password", "Password", width = "100%"),
        tags$script(HTML(
          "setTimeout(function() { $('#admin_password').focus(); }, 300);
           $('#admin_password').on('keyup', function(e) {
             if (e.key === 'Enter') $('#admin_sign_in').click();
           });"
        ))
      )
    },
    if (!is.null(error)) div(class = "alert alert-danger", error),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      if (!locked) actionButton("admin_sign_in", "Sign in", class = "btn btn-primary")
    )
  )
}

local({
  missing <- vapply(c(site$partner1, site$partner2), function(p) {
    nchar(Sys.getenv(admin_password_var(p))) < 10
  }, TRUE)
  if (any(missing)) {
    warning(
      "No planning password (10+ characters) set in: ",
      paste(vapply(names(missing)[missing], admin_password_var, ""), collapse = ", "),
      call. = FALSE
    )
  }
})
