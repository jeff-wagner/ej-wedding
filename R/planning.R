# ---------------------------------------------------------------------------
# Planning tracker (couple only): vendors and budget
#
# Stored in the "vendors" and "budget" tabs of the Sheet. Each vendor's
# projected cost is its contracted cost once you have one, otherwise its
# estimate. Vendors marked "Not using" don't count toward totals.
# Only started for a signed-in browser (see R/couple_auth.R), and every output
# and action checks the sign-in again.
# ---------------------------------------------------------------------------

vendor_statuses <- c("Researching", "Contacted", "Booked", "Paid in full", "Not using")

default_categories <- c(
  "Venue", "Catering", "Bar", "Cake & desserts", "Photography", "Videography",
  "Florals", "Music / DJ", "Officiant", "Attire", "Hair & makeup", "Rings",
  "Rentals", "Stationery", "Transportation", "Lodging", "Favors & gifts", "Other"
)

as_money <- function(x) {
  x <- suppressWarnings(as.numeric(gsub("[$,[:space:]]", "", x)))
  x[is.na(x)] <- 0
  x
}

fmt_money <- function(x) {
  paste0(ifelse(x < 0, "-$", "$"), formatC(abs(x), format = "f", digits = 0, big.mark = ","))
}

# Vendors with numeric money columns plus projected cost and balance
vendor_numbers <- function(v) {
  v$estimate_n <- as_money(v$estimate)
  v$cost_n <- as_money(v$cost)
  v$paid_n <- as_money(v$paid)
  v$projected <- ifelse(v$status == "Not using", 0, ifelse(v$cost_n > 0, v$cost_n, v$estimate_n))
  v$balance <- pmax(v$projected - v$paid_n, 0)
  v
}

category_rollup <- function(v, b) {
  v <- vendor_numbers(v)
  cats <- unique(c(b$category, v$category[nzchar(v$category)]))
  out <- data.frame(category = cats, stringsAsFactors = FALSE)
  out$budget <- vapply(cats, function(cc) sum(as_money(b$amount[b$category == cc])), 0)
  out$projected <- vapply(cats, function(cc) sum(v$projected[v$category == cc]), 0)
  out$paid <- vapply(cats, function(cc) sum(v$paid_n[v$category == cc & v$status != "Not using"]), 0)
  out$left <- out$budget - out$projected
  out[order(-out$budget, -out$projected), , drop = FALSE]
}

plan_stat <- function(label, value, note = NULL, class = NULL) {
  div(
    class = paste("plan-stat", class),
    div(class = "plan-stat-label", label),
    div(class = "plan-stat-value", value),
    if (!is.null(note)) div(class = "plan-stat-note", note)
  )
}

planning_ui <- function(id) {
  ns <- NS(id)
  page_section(
    class = "planning",
    div(
      class = "planning-head",
      section_title("Planning"),
      div(
        class = "planning-user",
        "Signed in as ", textOutput(ns("who"), inline = TRUE), " · ",
        actionLink(ns("sign_out"), "Sign out")
      )
    ),
    uiOutput(ns("load_error")),
    navset_underline(
      id = ns("tabs"),
      nav_panel(
        "Overview",
        uiOutput(ns("summary")),
        h3(class = "day-heading caps-sm", "Budget by category"),
        uiOutput(ns("by_category")),
        h3(class = "day-heading caps-sm", "Upcoming payments"),
        uiOutput(ns("upcoming"))
      ),
      nav_panel(
        "Vendors",
        div(
          class = "planning-actions",
          actionButton(ns("add_vendor"), "Add vendor", icon = bsicons::bs_icon("plus-lg"), class = "btn-primary"),
          actionButton(ns("edit_vendor"), "Edit", icon = bsicons::bs_icon("pencil")),
          actionButton(ns("delete_vendor"), "Delete", icon = bsicons::bs_icon("trash")),
          span(class = "muted", "Select a row to edit it, or double-click it.")
        ),
        DT::DTOutput(ns("vendors_tbl"))
      ),
      nav_panel(
        "Budget",
        div(
          class = "planning-actions",
          actionButton(ns("add_category"), "Set a category budget", icon = bsicons::bs_icon("plus-lg"), class = "btn-primary"),
          actionButton(ns("delete_category"), "Remove budget", icon = bsicons::bs_icon("trash")),
          span(class = "muted", "Double-click a row to change its budget.")
        ),
        DT::DTOutput(ns("budget_tbl"))
      )
    )
  )
}

# admin_user: reactive, the signed-in partner's name or NULL
# on_sign_out: called when "Sign out" is clicked
planning_server <- function(id, admin_user, on_sign_out) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    signed_in <- reactive(!is.null(admin_user()))

    output$who <- renderText(admin_user())
    observeEvent(input$sign_out, on_sign_out())

    # --- Data ---
    vendors <- reactiveVal(empty_table("vendors"))
    budget <- reactiveVal(empty_table("budget"))
    load_error <- reactiveVal(NULL)

    # Always read fresh: the other one of you may have edited since
    reload <- function() {
      tryCatch(
        {
          vendors(read_table("vendors", max_age = 0))
          budget(read_table("budget", max_age = 0))
          load_error(NULL)
        },
        error = function(e) {
          message("Planning load failed: ", conditionMessage(e))
          load_error(conditionMessage(e))
        }
      )
    }
    observeEvent(admin_user(), reload(), ignoreNULL = TRUE)

    output$load_error <- renderUI({
      req(signed_in(), load_error())
      div(class = "alert alert-danger", "Couldn't load the planning sheets: ", load_error())
    })

    # Apply a change to the latest copy of a table, then save it
    save_change <- function(name, change) {
      ok <- tryCatch(
        {
          df <- change(read_table(name, max_age = 0))
          write_table(name, df)
          if (name == "vendors") vendors(df) else budget(df)
          TRUE
        },
        error = function(e) {
          message("Planning save failed: ", conditionMessage(e))
          showNotification(paste("Couldn't save:", conditionMessage(e)), type = "error", duration = 8)
          FALSE
        }
      )
      ok
    }

    categories <- reactive({
      unique(c(default_categories, budget()$category, vendors()$category[nzchar(vendors()$category)]))
    })

    # --- Overview ---
    output$summary <- renderUI({
      req(signed_in())
      v <- vendor_numbers(vendors())
      total <- sum(as_money(budget()$amount))
      projected <- sum(v$projected)
      paid <- sum(v$paid_n[v$status != "Not using"])
      left <- total - projected
      active <- v[v$status != "Not using", , drop = FALSE]
      booked <- sum(active$status %in% c("Booked", "Paid in full"))
      div(
        class = "plan-stats",
        plan_stat("Total budget", fmt_money(total),
                  if (total == 0) "Set category budgets on the Budget tab"),
        plan_stat("Projected spend", fmt_money(projected),
                  if (total > 0) paste0(round(100 * projected / total), "% of budget")),
        plan_stat("Paid so far", fmt_money(paid)),
        plan_stat("Still owed", fmt_money(sum(active$balance))),
        plan_stat(if (left < 0) "Over budget" else "Left in budget", fmt_money(abs(left)),
                  class = if (left < 0) "over"),
        plan_stat("Vendors booked", paste(booked, "of", nrow(active)))
      )
    })

    output$by_category <- renderUI({
      req(signed_in())
      r <- category_rollup(vendors(), budget())
      r <- r[r$budget > 0 | r$projected > 0, , drop = FALSE]
      if (!nrow(r)) return(p(class = "muted center", "Nothing yet. Add vendors or set a category budget."))
      tags$table(
        class = "table plan-table",
        tags$thead(tags$tr(
          tags$th("Category"), tags$th(class = "num", "Budget"), tags$th(class = "num", "Projected"),
          tags$th(class = "num", "Paid"), tags$th(class = "num", "Left"), tags$th(class = "bar-col", "")
        )),
        tags$tbody(lapply(seq_len(nrow(r)), function(i) {
          x <- r[i, ]
          pct <- if (x$budget > 0) min(x$projected / x$budget, 1) else 1
          over <- x$projected > x$budget
          tags$tr(
            tags$td(x$category),
            tags$td(class = "num", fmt_money(x$budget)),
            tags$td(class = "num", fmt_money(x$projected)),
            tags$td(class = "num", fmt_money(x$paid)),
            tags$td(class = paste("num", if (over) "over"), fmt_money(x$left)),
            tags$td(class = "bar-col", div(
              class = paste("plan-bar", if (over) "over"),
              div(style = sprintf("width: %.0f%%", 100 * pct))
            ))
          )
        }))
      )
    })

    output$upcoming <- renderUI({
      req(signed_in())
      v <- vendor_numbers(vendors())
      v <- v[nzchar(v$due_date) & v$balance > 0 & v$status != "Not using", , drop = FALSE]
      if (!nrow(v)) return(p(class = "muted center", "No payments with due dates."))
      v <- v[order(v$due_date), , drop = FALSE]
      due <- as.Date(v$due_date)
      tags$ul(
        class = "plan-upcoming",
        lapply(seq_len(nrow(v)), function(i) {
          late <- !is.na(due[i]) && due[i] < Sys.Date()
          tags$li(
            class = if (late) "over",
            span(class = "plan-due", format(due[i], "%b %e, %Y"), if (late) " (past due)"),
            span(class = "plan-due-vendor", v$vendor[i], span(class = "muted", paste0(" · ", v$category[i]))),
            span(class = "num", fmt_money(v$balance[i]), span(class = "muted", " left"))
          )
        })
      )
    })

    # --- Vendors table ---
    output$vendors_tbl <- DT::renderDT({
      req(signed_in())
      v <- vendor_numbers(vendors())
      contact <- trimws(paste(v$contact, v$email, v$phone))
      shown <- data.frame(
        Vendor = v$vendor, Category = v$category, Status = v$status,
        Projected = v$projected, Paid = v$paid_n, Balance = v$balance,
        `Next due` = v$due_date, Contact = contact, Notes = v$notes,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::formatCurrency(
        DT::datatable(
          shown,
          rownames = FALSE,
          selection = "single",
          class = "compact hover",
          options = list(pageLength = 25, dom = "ftip", autoWidth = FALSE,
                         language = list(emptyTable = "No vendors yet. Click Add vendor to start.")),
          callback = DT::JS(sprintf(
            "table.on('dblclick', 'tbody tr', function() {
               var i = table.row(this).index();
               if (i !== undefined) Shiny.setInputValue('%s', i + 1, {priority: 'event'});
             });", ns("vendor_dblclick")
          ))
        ),
        c("Projected", "Paid", "Balance"), digits = 0
      )
    })

    editing_id <- reactiveVal(NULL)

    # The due date is a native browser date field (typing works, and phones get
    # their own picker). It gets a fresh id each time the dialog opens so a
    # date from a previous dialog can never carry over.
    due_field <- reactiveVal(list(id = "v_due_0", start = ""))
    dialogs_opened <- 0

    vendor_modal <- function(v = NULL) {
      val <- function(col, default = "") if (is.null(v)) default else v[[col]]
      money_val <- function(col) {
        x <- val(col)
        if (nzchar(x)) as_money(x) else NA
      }
      modalDialog(
        title = if (is.null(v)) "Add vendor" else paste("Edit", v$vendor),
        size = "l",
        div(
          class = "plan-form",
          textInput(ns("v_vendor"), "Vendor", val("vendor"), width = "100%"),
          selectizeInput(ns("v_category"), "Category", categories(), selected = val("category", "Venue"),
                         options = list(create = TRUE), width = "100%"),
          selectInput(ns("v_status"), "Status", vendor_statuses, selected = val("status", "Researching"), width = "100%"),
          textInput(ns("v_contact"), "Contact person", val("contact"), width = "100%"),
          textInput(ns("v_email"), "Email", val("email"), width = "100%"),
          textInput(ns("v_phone"), "Phone", val("phone"), width = "100%"),
          numericInput(ns("v_estimate"), "Estimate ($)", money_val("estimate"), min = 0, width = "100%"),
          numericInput(ns("v_cost"), "Contracted cost ($)", money_val("cost"), min = 0, width = "100%"),
          numericInput(ns("v_paid"), "Paid so far ($)", money_val("paid"), min = 0, width = "100%"),
          div(
            class = "form-group shiny-input-container w-100",
            tags$label(class = "control-label", `for` = ns(due_field()$id), "Next payment due"),
            tags$input(
              id = ns(due_field()$id), type = "date", class = "form-control", value = val("due_date"),
              onchange = "Shiny.setInputValue(this.id, this.value);"
            )
          )
        ),
        textAreaInput(ns("v_notes"), "Notes", val("notes"), width = "100%", rows = 3),
        uiOutput(ns("vendor_error")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_vendor"), "Save", class = "btn btn-primary")
        )
      )
    }

    vendor_error <- reactiveVal(NULL)
    output$vendor_error <- renderUI({
      req(vendor_error())
      div(class = "alert alert-danger", vendor_error())
    })

    open_vendor <- function(row) {
      req(signed_in())
      v <- if (is.null(row)) NULL else vendors()[row, ]
      editing_id(if (is.null(v)) "" else v$id)
      dialogs_opened <<- dialogs_opened + 1
      due_field(list(id = paste0("v_due_", dialogs_opened), start = if (is.null(v)) "" else v$due_date))
      vendor_error(NULL)
      showModal(vendor_modal(v))
    }

    selected_vendor <- reactive(input$vendors_tbl_rows_selected)

    observeEvent(input$add_vendor, open_vendor(NULL))
    observeEvent(input$vendor_dblclick, open_vendor(input$vendor_dblclick))
    observeEvent(input$edit_vendor, {
      if (!length(selected_vendor())) {
        showNotification("Select a vendor in the table first.")
        return()
      }
      open_vendor(selected_vendor())
    })

    observeEvent(input$save_vendor, {
      req(signed_in(), !is.null(editing_id()))
      name <- trimws(input$v_vendor %||% "")
      category <- trimws(input$v_category %||% "")
      if (!nzchar(name) || !nzchar(category)) {
        vendor_error("Please add the vendor's name and a category.")
        return()
      }
      money_in <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
      due <- input[[due_field()$id]] %||% due_field()$start
      if (nzchar(due) && is.na(as.Date(due, optional = TRUE))) {
        vendor_error("Please pick a valid due date, or leave it blank.")
        return()
      }
      id <- if (nzchar(editing_id())) editing_id() else new_id()
      row <- data.frame(
        id = id, category = category, vendor = name,
        contact = trimws(input$v_contact %||% ""), email = trimws(input$v_email %||% ""),
        phone = trimws(input$v_phone %||% ""), status = input$v_status,
        estimate = money_in(input$v_estimate), cost = money_in(input$v_cost), paid = money_in(input$v_paid),
        due_date = due,
        notes = trimws(input$v_notes %||% ""),
        updated = paste(format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"), "UTC by", admin_user()),
        stringsAsFactors = FALSE
      )
      ok <- save_change("vendors", function(df) {
        if (id %in% df$id) {
          df[df$id == id, ] <- row
          df
        } else {
          rbind(df, row)
        }
      })
      if (ok) {
        editing_id(NULL)
        removeModal()
      }
    })

    observeEvent(input$delete_vendor, {
      req(signed_in())
      if (!length(selected_vendor())) {
        showNotification("Select a vendor in the table first.")
        return()
      }
      v <- vendors()[selected_vendor(), ]
      editing_id(v$id)
      showModal(modalDialog(
        title = "Delete vendor?",
        p("Delete ", strong(v$vendor), " and everything you've recorded for it? This can't be undone here."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_vendor"), "Delete", class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete_vendor, {
      req(signed_in(), editing_id())
      id <- editing_id()
      if (save_change("vendors", function(df) df[df$id != id, , drop = FALSE])) {
        editing_id(NULL)
        removeModal()
      }
    })

    # --- Budget ---
    output$budget_tbl <- DT::renderDT({
      req(signed_in())
      r <- category_rollup(vendors(), budget())
      shown <- data.frame(
        Category = r$category, Budget = r$budget, Projected = r$projected,
        Paid = r$paid, Left = r$left, stringsAsFactors = FALSE
      )
      DT::formatStyle(
        DT::formatCurrency(
          DT::datatable(
            shown,
            rownames = FALSE,
            selection = "single",
            class = "compact hover",
            options = list(pageLength = 50, dom = "t", ordering = FALSE,
                           language = list(emptyTable = "No budget yet. Click Set a category budget to start.")),
            callback = DT::JS(sprintf(
              "table.on('dblclick', 'tbody tr', function() {
                 var i = table.row(this).index();
                 if (i !== undefined) Shiny.setInputValue('%s', i + 1, {priority: 'event'});
               });", ns("budget_dblclick")
            ))
          ),
          c("Budget", "Projected", "Paid", "Left"), digits = 0
        ),
        "Left", color = DT::styleInterval(-0.5, c("#A0452B", "inherit"))
      )
    })

    budget_rows <- reactive(category_rollup(vendors(), budget()))

    category_modal <- function(category = NULL) {
      current <- if (is.null(category)) NA else sum(as_money(budget()$amount[budget()$category == category]))
      modalDialog(
        title = if (is.null(category)) "Set a category budget" else paste("Budget for", category),
        if (is.null(category)) {
          selectizeInput(ns("b_category"), "Category", categories(),
                         options = list(create = TRUE, placeholder = "Pick or type a category",
                                        onInitialize = I("function() { this.setValue(''); }")),
                         width = "100%")
        } else {
          div(class = "d-none", textInput(ns("b_category"), NULL, category))
        },
        numericInput(ns("b_amount"), "Budget ($)", current, min = 0, width = "100%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_category"), "Save", class = "btn btn-primary")
        )
      )
    }

    observeEvent(input$add_category, {
      req(signed_in())
      showModal(category_modal())
    })
    observeEvent(input$budget_dblclick, {
      req(signed_in())
      showModal(category_modal(budget_rows()$category[input$budget_dblclick]))
    })

    observeEvent(input$save_category, {
      req(signed_in())
      category <- trimws(input$b_category %||% "")
      amount <- input$b_amount
      if (!nzchar(category) || is.null(amount) || is.na(amount) || amount < 0) {
        showNotification("Please pick a category and enter an amount.", type = "warning")
        return()
      }
      ok <- save_change("budget", function(df) {
        df <- df[df$category != category, , drop = FALSE]
        rbind(df, data.frame(category = category, amount = as.character(amount), stringsAsFactors = FALSE))
      })
      if (ok) removeModal()
    })

    observeEvent(input$delete_category, {
      req(signed_in())
      i <- input$budget_tbl_rows_selected
      if (!length(i)) {
        showNotification("Select a category in the table first.")
        return()
      }
      category <- budget_rows()$category[i]
      if (!category %in% budget()$category) {
        showNotification("That category has no budget set. Its vendors are listed on the Vendors tab.")
        return()
      }
      save_change("budget", function(df) df[df$category != category, , drop = FALSE])
    })
  })
}
