# ---------------------------------------------------------------------------
# Community board
#
# Anyone can read the board. Guests post and reply after entering the guest
# code from their invitation (the same code that unlocks the RSVP form).
# When signed in to the planning area (see R/couple_auth.R), the couple posts with
# a "The couple" badge and can hide notes. Hidden notes stay in the "board"
# tab of the Sheet with hidden = "yes"; clear that cell to bring one back.
# ---------------------------------------------------------------------------

board_page_size   <- 20    # notes shown before "Show more"
board_max_name    <- 60
board_max_message <- 1000
board_min_gap     <- 15    # seconds between posts from one browser
board_max_posts   <- 30    # posts per browser session
board_refresh     <- 60    # seconds; picks up posts from other server processes

# Bumped on every post or hide, so every open browser on this server process
# redraws the board straight away
board_version <- shiny::reactiveVal(0)

board_add <- function(name, message, parent_id = "", couple = FALSE) {
  append_row("board", data.frame(
    id = new_id(),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    parent_id = parent_id,
    name = name,
    message = message,
    couple = if (couple) "yes" else "",
    hidden = "",
    stringsAsFactors = FALSE
  ))
  board_version(shiny::isolate(board_version()) + 1)
}

board_hide <- function(id) {
  df <- read_table("board", max_age = 0)
  df$hidden[df$id == id] <- "yes"
  write_table("board", df)
  board_version(shiny::isolate(board_version()) + 1)
}

board_date <- function(ts) {
  t <- as.POSIXct(ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  ifelse(is.na(t), "", format(t, "%b %e, %Y"))
}

board_entry <- function(x, ns, is_admin) {
  couple <- identical(x$couple, "yes")
  div(
    class = paste("board-entry", if (couple) "from-couple"),
    div(
      class = "board-meta",
      span(class = "board-name", x$name),
      if (couple) span(class = "board-badge", "The couple"),
      span(class = "board-date", board_date(x$timestamp)),
      if (is_admin) {
        tags$button(
          type = "button",
          class = "btn btn-link board-action",
          onclick = sprintf(
            "if (confirm('Hide this note from the board?')) Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("hide"), x$id
          ),
          "Hide"
        )
      }
    ),
    p(class = "board-message", x$message)
  )
}

board_post <- function(post, replies, ns, can_reply, is_admin) {
  div(
    class = "board-post",
    board_entry(post, ns, is_admin),
    if (nrow(replies)) {
      div(
        class = "board-replies",
        lapply(seq_len(nrow(replies)), function(j) board_entry(replies[j, ], ns, is_admin))
      )
    },
    if (can_reply) {
      tags$button(
        type = "button",
        class = "btn btn-link board-action board-reply",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
          ns("reply_to"), post$id
        ),
        bsicons::bs_icon("reply"), "Reply"
      )
    }
  )
}

board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "lead-text", board_text$intro),
    uiOutput(ns("composer")),
    uiOutput(ns("posts")),
    uiOutput(ns("more"))
  )
}

# guest_code: reactive, the matched guest code or NULL
# try_code:   function(entered) -> NULL on success, or an error message
# admin_user: reactive, the signed-in partner's name or NULL
board_server <- function(id, guest_code, try_code, admin_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    can_post <- reactive(!is.null(admin_user()) || !is.null(guest_code()))

    # --- Load (only redraws when the notes actually change) ---
    posts <- reactiveVal(NULL)
    load_failed <- reactiveVal(FALSE)
    observe({
      board_version()
      invalidateLater(board_refresh * 1000)
      df <- tryCatch(read_table("board", max_age = board_refresh), error = function(e) {
        message("Board load failed: ", conditionMessage(e))
        NULL
      })
      load_failed(is.null(df))
      if (!is.null(df)) posts(df[df$hidden != "yes", , drop = FALSE])
    })

    # --- Posting ---
    last_name <- reactiveVal("")
    post_times <- reactiveVal(numeric(0))

    # Server-side checks for posts and replies; returns NULL or an error
    submit <- function(name, message, parent_id = "") {
      is_admin <- !is.null(admin_user())
      if (!can_post()) return("Please enter your guest code first.")
      name <- if (is_admin) admin_user() else trimws(name %||% "")
      message <- trimws(message %||% "")
      if (!nzchar(name)) return("Please add your name.")
      if (nchar(name) > board_max_name) return("That name is a bit long.")
      if (!nzchar(message)) return("Please write a note.")
      if (nchar(message) > board_max_message) {
        return(paste0("Please keep notes under ", board_max_message, " characters."))
      }
      if (nzchar(parent_id) && !parent_id %in% posts()$id[posts()$parent_id == ""]) {
        return("That note isn't on the board any more.")
      }
      times <- post_times()
      if (!is_admin) {
        if (length(times) >= board_max_posts) {
          return("You've posted a lot today. Thank you! Please try again later.")
        }
        if (length(times) && as.numeric(Sys.time()) - max(times) < board_min_gap) {
          return("One moment, please. Try again in a few seconds.")
        }
      }
      ok <- tryCatch(
        {
          board_add(name, message, parent_id, couple = is_admin)
          TRUE
        },
        error = function(e) {
          message("Board post failed: ", conditionMessage(e))
          FALSE
        }
      )
      if (!ok) return("Sorry, your note didn't save. Please try again in a bit.")
      post_times(c(times, as.numeric(Sys.time())))
      if (!is_admin) last_name(name)
      NULL
    }

    code_error <- reactiveVal(NULL)
    post_error <- reactiveVal(NULL)

    output$composer <- renderUI({
      if (!can_post()) {
        return(div(
          class = "board-composer rsvp-code",
          p("Enter the guest code from your invitation to join the conversation."),
          textInput(ns("code"), NULL, width = "100%", placeholder = "Guest code"),
          uiOutput(ns("code_error")),
          actionButton(ns("check_code"), "Continue", class = "btn btn-primary btn-rsvp"),
          tags$script(HTML(sprintf(
            "$('#%s').on('keyup', function(e) { if (e.key === 'Enter') $('#%s').click(); });",
            ns("code"), ns("check_code")
          )))
        ))
      }
      admin <- admin_user()
      div(
        class = "board-composer",
        if (is.null(admin)) {
          textInput(ns("name"), "Your name", value = isolate(last_name()), width = "100%")
        } else {
          p(class = "board-meta", "Posting as ", span(class = "board-name", admin),
            span(class = "board-badge", "The couple"))
        },
        textAreaInput(
          ns("message"), if (is.null(admin)) "Your note" else NULL,
          width = "100%", rows = 3,
          placeholder = "Say hello, share travel plans, find a carpool..."
        ),
        uiOutput(ns("post_error")),
        actionButton(ns("post"), "Post", class = "btn btn-primary btn-rsvp")
      )
    })

    output$code_error <- renderUI({
      req(code_error())
      div(class = "alert alert-danger", code_error())
    })
    output$post_error <- renderUI({
      req(post_error())
      div(class = "alert alert-danger", post_error())
    })

    observeEvent(input$check_code, code_error(try_code(input$code)))

    observeEvent(input$post, {
      err <- submit(input$name, input$message)
      post_error(err)
      if (is.null(err)) updateTextAreaInput(session, "message", value = "")
    })

    # --- Replies (in a dialog, so a board refresh never wipes a half-typed reply) ---
    replying_to <- reactiveVal(NULL)
    reply_error <- reactiveVal(NULL)

    observeEvent(input$reply_to, {
      req(can_post())
      post <- posts()[posts()$id == input$reply_to, , drop = FALSE]
      req(nrow(post) == 1)
      replying_to(post$id)
      reply_error(NULL)
      showModal(modalDialog(
        title = paste("Reply to", post$name),
        div(class = "board-quote", post$message),
        if (is.null(admin_user())) {
          textInput(ns("reply_name"), "Your name", value = last_name(), width = "100%")
        },
        textAreaInput(ns("reply_message"), "Your reply", width = "100%", rows = 3),
        uiOutput(ns("reply_error")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("send_reply"), "Reply", class = "btn btn-primary")
        )
      ))
    })

    output$reply_error <- renderUI({
      req(reply_error())
      div(class = "alert alert-danger", reply_error())
    })

    observeEvent(input$send_reply, {
      req(replying_to())
      err <- submit(input$reply_name, input$reply_message, replying_to())
      reply_error(err)
      if (is.null(err)) {
        replying_to(NULL)
        removeModal()
      }
    })

    observeEvent(input$hide, {
      req(admin_user())
      tryCatch(board_hide(input$hide), error = function(e) {
        message("Board hide failed: ", conditionMessage(e))
        showNotification("Couldn't hide that note. Please try again.", type = "error")
      })
    })

    # --- The notes ---
    n_shown <- reactiveVal(board_page_size)
    observeEvent(input$show_more, n_shown(n_shown() + board_page_size))

    top_posts <- reactive({
      df <- posts()
      req(df)
      top <- df[df$parent_id == "", , drop = FALSE]
      top[order(top$timestamp, decreasing = TRUE), , drop = FALSE]
    })

    output$posts <- renderUI({
      if (is.null(posts())) {
        return(p(
          class = "muted center",
          if (load_failed()) "The board couldn't load right now. Please try again in a bit." else "Loading..."
        ))
      }
      top <- top_posts()
      if (!nrow(top)) return(p(class = "muted center board-empty", "No notes yet. Be the first!"))
      df <- posts()
      shown <- head(top, n_shown())
      is_admin <- !is.null(admin_user())
      div(
        class = "board-posts",
        lapply(seq_len(nrow(shown)), function(i) {
          post <- shown[i, ]
          replies <- df[df$parent_id == post$id, , drop = FALSE]
          replies <- replies[order(replies$timestamp), , drop = FALSE]
          board_post(post, replies, ns, can_post(), is_admin)
        })
      )
    })

    output$more <- renderUI({
      req(nrow(top_posts()) > n_shown())
      div(class = "center", actionButton(ns("show_more"), "Show more", class = "btn btn-outline-primary"))
    })
  })
}
