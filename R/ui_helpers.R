# Small UI building blocks shared across pages

# Null-coalescing helper (base R only has this from 4.4.0)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Hand-drawn-looking water lines, echoing the illustration
water_divider <- function() {
  HTML('
<svg class="water-divider" viewBox="0 0 600 40" preserveAspectRatio="none" aria-hidden="true">
  <path d="M10 14 C 80 10, 160 18, 250 13 S 420 9, 590 15" />
  <path d="M60 26 C 140 22, 220 30, 300 25 S 470 22, 560 27" />
  <path d="M150 36 C 210 33, 290 38, 360 35" />
</svg>')
}

# Section heading in the save-the-date style: wide-tracked caps with an
# optional script word in the middle ("SAVE the DATE")
section_title <- function(left, script = NULL, right = NULL) {
  tags$h2(
    class = "section-title",
    span(class = "caps", left),
    if (!is.null(script)) span(class = "script", script),
    if (!is.null(right)) span(class = "caps", right)
  )
}

page_section <- function(..., class = NULL) {
  div(class = paste("page-section", class), ...)
}

event_card <- function(ev) {
  div(
    class = "event-card",
    div(class = "event-time", ev$time),
    div(
      class = "event-body",
      h4(ev$title),
      div(class = "event-place", bsicons::bs_icon("geo-alt"), ev$place),
      p(ev$note)
    )
  )
}

info_card <- function(icon, title, text) {
  div(
    class = "info-card",
    div(class = "info-icon", bsicons::bs_icon(icon)),
    h4(title),
    p(text)
  )
}
