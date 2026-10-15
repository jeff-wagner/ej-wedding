# Emily & Jeff -- wedding website
# Run with: shiny::runApp()
# Files in R/ are sourced automatically (content.R, storage.R, ui_helpers.R).

library(shiny)
library(bslib)
library(bsicons)

# ---- Theme -----------------------------------------------------------------
# Colors pulled from the phalarope illustration and the save-the-date
pal <- c(
  sage = "#4E5547", # save-the-date lettering
  rust = "#A0452B", # breeding-plumage neck
  slate = "#5E6266", # head / back
  water = "#8EA8AE", # water lines
  ochre = "#C69A3A", # wing highlights
  cream = "#FBF8F2", # belly
  ink = "#1F1F1F" # line work
)

theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = pal[["sage"]],
  primary = pal[["sage"]],
  secondary = pal[["water"]],
  success = pal[["sage"]],
  danger = pal[["rust"]],
  base_font = font_google(
    "Cormorant Garamond",
    wght = c(400, 500, 600),
    local = FALSE
  ),
  heading_font = font_google(
    "Cormorant Garamond",
    wght = c(500, 600),
    local = FALSE
  ),
  "font-size-base" = "1.15rem",
  "border-radius" = "2px",
  "navbar-bg" = "#FFFFFF"
)

# ---- Pages -----------------------------------------------------------------
home_page <- page_section(
  class = "home",
  img(
    src = "img/phalaropes.webp",
    class = "hero-art",
    alt = "Illustration of two red-necked phalaropes facing each other on the water, with a heart between their bills"
  ),
  div(class = "names script", paste(site$partner1, "&", site$partner2)),
  div(class = "date-display", site$date_display),
  div(class = "caps-sm", site$location),
  uiOutput("countdown"),
  if (site$rsvp_open) {
    actionButton("go_rsvp", "RSVP", class = "btn btn-outline-primary btn-rsvp")
  }
)

story_page <- page_section(
  section_title("Our", "little", "Story"),
  p(class = "lead-text", story$intro),
  div(
    class = "milestones",
    lapply(story$milestones, function(m) {
      div(
        class = "milestone",
        div(class = "milestone-when", m$when),
        h4(m$title),
        p(m$text)
      )
    })
  ),
  water_divider()
)

weekend_page <- page_section(
  section_title("The", "wedding", "Weekend"),
  p(
    class = "lead-text",
    site$date_long,
    br(),
    site$venue,
    br(),
    span(class = "muted", site$venue_address)
  ),
  {
    days <- unique(vapply(events, `[[`, "", "day"))
    lapply(days, function(d) {
      tagList(
        h3(class = "day-heading caps-sm", d),
        lapply(Filter(function(e) e$day == d, events), event_card)
      )
    })
  },
  water_divider()
)

travel_page <- page_section(
  section_title("Travel", "&", "Stay"),
  layout_column_wrap(
    width = "220px",
    fill = FALSE,
    class = "info-grid",
    !!!lapply(travel$getting_there, function(x) {
      info_card(x$icon, x$title, x$text)
    })
  ),
  h3(class = "day-heading caps-sm", "Where to stay"),
  div(
    class = "hotel-list",
    lapply(travel$hotels, function(h) {
      div(
        class = "hotel",
        tags$a(href = h$url, target = "_blank", rel = "noopener", h$name),
        p(h$detail)
      )
    })
  ),
  water_divider()
)

rsvp_page <- page_section(
  class = "rsvp",
  section_title("Kindly", "reply"),
  if (site$rsvp_open) {
    tagList(
      p(
        class = "lead-text",
        "Please respond by ",
        strong(format(site$rsvp_deadline, "%B %d, %Y"), .noWS = "after"),
        "."
      ),
      uiOutput("rsvp_body")
    )
  } else {
    p(class = "lead-text", "RSVPs will open with the formal invitation.")
  }
)

registry_page <- page_section(
  section_title("Gift", "registry"),
  p(
    class = "lead-text",
    "Your presence is the best gift. If you'd like to give something more, we've registered here:"
  ),
  div(
    class = "registry-links",
    lapply(registry, function(r) {
      tags$a(
        class = "btn btn-outline-primary",
        href = r$url,
        target = "_blank",
        rel = "noopener",
        r$name
      )
    })
  )
)

faq_page <- page_section(
  section_title("Questions", "&", "Answers"),
  accordion(
    open = FALSE,
    class = "faq",
    !!!lapply(faq, function(x) accordion_panel(x$q, p(x$a)))
  ),
  p(
    class = "muted center",
    "Anything else? Email us at ",
    tags$a(href = paste0("mailto:", site$contact_email), site$contact_email)
  )
)

# ---- UI --------------------------------------------------------------------
ui <- page_navbar(
  id = "nav",
  title = span(class = "brand script", site$initials),
  window_title = paste(site$partner1, "&", site$partner2),
  theme = theme,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "img/favicon.png"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Allura&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    )
  ),
  nav_panel("Home", value = "home", home_page),
  nav_panel("Our Story", value = "story", story_page),
  nav_panel("Weekend", value = "weekend", weekend_page),
  nav_panel("Travel", value = "travel", travel_page),
  nav_panel("RSVP", value = "rsvp", rsvp_page),
  nav_panel("Registry", value = "registry", registry_page),
  nav_panel("FAQ", value = "faq", faq_page),
  footer = div(
    class = "site-footer",
    span(class = "script", site$initials),
    " · ",
    site$date_display,
    " · ",
    site$location
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  # Countdown (refreshes hourly; days-level precision is plenty)
  output$countdown <- renderUI({
    invalidateLater(60 * 60 * 1000)
    days_left <- as.integer(site$date - Sys.Date())
    msg <- if (days_left > 1) {
      tagList(span(class = "count-num", days_left), " days to go")
    } else if (days_left == 1) {
      "Tomorrow!"
    } else if (days_left == 0) {
      "Today's the day!"
    } else {
      "Just married"
    }
    div(class = "countdown", msg)
  })

  observeEvent(input$go_rsvp, nav_select("nav", "rsvp"))

  # --- RSVP guest code ---
  code_used <- reactiveVal(NULL) # matched code once unlocked
  code_attempts <- reactiveVal(0)
  code_error <- reactiveVal(NULL)

  observeEvent(input$check_code, {
    if (code_attempts() >= max_code_attempts) {
      return()
    }
    code_attempts(code_attempts() + 1)
    matched <- check_guest_code(input$guest_code)
    if (!is.null(matched)) {
      code_used(matched)
      code_error(NULL)
    } else if (code_attempts() >= max_code_attempts) {
      code_error(paste0(
        "Too many attempts. Please email us at ",
        site$contact_email,
        " and we'll help you RSVP."
      ))
    } else {
      code_error(
        "That code doesn't match. Please check your invitation and try again."
      )
    }
  })

  output$code_error <- renderUI({
    req(code_error())
    div(class = "alert alert-danger", code_error())
  })

  # --- RSVP form ---
  submitted <- reactiveVal(FALSE)

  output$rsvp_body <- renderUI({
    if (is.null(code_used())) {
      locked <- code_attempts() >= max_code_attempts
      return(div(
        class = "rsvp-form rsvp-code",
        p("Please enter the guest code from your invitation."),
        textInput(
          "guest_code",
          NULL,
          width = "100%",
          placeholder = "Guest code"
        ),
        uiOutput("code_error"),
        if (!locked) {
          actionButton(
            "check_code",
            "Continue",
            class = "btn btn-primary btn-rsvp"
          )
        },
        # Let Enter submit the code
        tags$script(HTML(
          "$('#guest_code').on('keyup', function(e) {
             if (e.key === 'Enter') $('#check_code').click();
           });"
        ))
      ))
    }

    if (submitted()) {
      return(div(
        class = "rsvp-thanks",
        img(src = "img/phalaropes.webp", class = "thanks-art", alt = ""),
        h3(class = "script", "Thank you!"),
        p(
          if (identical(isolate(input$attending), "yes")) {
            "We can't wait to celebrate with you."
          } else {
            "We'll miss you, and thank you for letting us know."
          }
        )
      ))
    }

    div(
      class = "rsvp-form",
      textInput("name", "Your full name", width = "100%"),
      textInput("email", "Email", width = "100%"),
      radioButtons(
        "attending",
        "Will you be joining us?",
        choices = c("Joyfully accepts" = "yes", "Regretfully declines" = "no"),
        selected = character(0),
        inline = TRUE
      ),
      conditionalPanel(
        "input.attending == 'yes'",
        numericInput(
          "n_guests",
          "Number in your party (including you)",
          value = 1,
          min = 1,
          max = 6,
          step = 1,
          width = "200px"
        ),
        uiOutput("guest_names"),
        textAreaInput(
          "dietary",
          "Dietary restrictions or allergies",
          width = "100%",
          rows = 2
        ),
        textInput(
          "song",
          "A song that will get you on the dance floor",
          width = "100%"
        )
      ),
      textAreaInput(
        "message",
        "A note for the couple (optional)",
        width = "100%",
        rows = 3
      ),
      uiOutput("rsvp_error"),
      actionButton(
        "submit_rsvp",
        "Send RSVP",
        class = "btn btn-primary btn-rsvp"
      )
    )
  })

  # Name fields for additional guests; keep anything already typed
  output$guest_names <- renderUI({
    n <- suppressWarnings(as.integer(input$n_guests))
    if (is.na(n) || n < 2) {
      return(NULL)
    }
    n <- min(n, 6)
    lapply(seq_len(n - 1), function(i) {
      id <- paste0("guest_", i)
      textInput(
        id,
        paste("Guest", i + 1, "name"),
        value = isolate(input[[id]]) %||% "",
        width = "100%"
      )
    })
  })

  rsvp_error <- reactiveVal(NULL)
  output$rsvp_error <- renderUI({
    req(rsvp_error())
    div(class = "alert alert-danger", rsvp_error())
  })

  observeEvent(input$submit_rsvp, {
    # Server-side guard: never save without a valid code, whatever the browser sends
    req(code_used(), !submitted())
    name <- trimws(input$name %||% "")
    email <- trimws(input$email %||% "")
    attending <- input$attending

    n_guests <- if (identical(attending, "yes")) {
      suppressWarnings(as.integer(input$n_guests))
    } else {
      0L
    }
    guest_names <- if (isTRUE(n_guests > 1)) {
      vapply(
        seq_len(min(n_guests, 6) - 1),
        function(i) trimws(input[[paste0("guest_", i)]] %||% ""),
        ""
      )
    } else {
      character(0)
    }

    problems <- c(
      if (!nzchar(name)) "your name",
      if (!grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE)) {
        "a valid email"
      },
      if (is.null(attending) || !nzchar(attending)) "whether you'll attend",
      if (
        identical(attending, "yes") &&
          (is.na(n_guests) || n_guests < 1 || n_guests > 6)
      ) {
        "a party size between 1 and 6"
      },
      if (any(!nzchar(guest_names))) "each guest's name"
    )
    if (length(problems)) {
      if (length(problems) > 1) {
        problems <- c(head(problems, -1), paste("and", tail(problems, 1)))
      }
      rsvp_error(paste0(
        "Please include ",
        paste(problems, collapse = ", "),
        "."
      ))
      return()
    }

    row <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      guest_code = code_used(),
      name = name,
      email = email,
      attending = attending,
      n_guests = n_guests,
      guest_names = paste(guest_names, collapse = "; "),
      dietary = if (identical(attending, "yes")) {
        trimws(input$dietary %||% "")
      } else {
        ""
      },
      song = if (identical(attending, "yes")) {
        trimws(input$song %||% "")
      } else {
        ""
      },
      message = trimws(input$message %||% ""),
      stringsAsFactors = FALSE
    )

    ok <- tryCatch(
      {
        save_rsvp(row)
        TRUE
      },
      error = function(e) {
        message("RSVP save failed: ", conditionMessage(e))
        FALSE
      }
    )
    if (!ok) {
      rsvp_error(paste0(
        "Sorry, something went wrong saving your RSVP. Please email us at ",
        site$contact_email,
        "."
      ))
      return()
    }
    rsvp_error(NULL)
    submitted(TRUE)
  })
}

shinyApp(ui, server)
