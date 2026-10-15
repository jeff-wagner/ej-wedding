# ---------------------------------------------------------------------------
# Site content -- edit this file to update text without touching UI/server code.
# Anything in [brackets] is a placeholder to fill in.
# ---------------------------------------------------------------------------

site <- list(
  partner1      = "Emily",
  partner2      = "Jeff",
  initials      = "E + J",
  date          = as.Date("2027-09-18"),
  date_display  = "09.18.27",
  date_long     = "Saturday, September 18, 2027",
  location      = "New Buffalo, MI",
  venue         = "[Venue name]",
  venue_address = "[Street address], New Buffalo, MI 49117",
  rsvp_open     = FALSE,  # flip to TRUE when formal invitations go out
  rsvp_deadline = as.Date("2027-07-31"),
  contact_email = "[your-email@example.com]"
)

story <- list(
  intro = "[A short paragraph about how you met. Where were you? Who noticed whom first?]",
  milestones = list(
    list(when = "[Year]", title = "We met",       text = "[One or two sentences.]"),
    list(when = "[Year]", title = "First trip",   text = "[One or two sentences.]"),
    list(when = "[Year]", title = "The proposal", text = "[One or two sentences.]")
  )
)

# Events for the weekend timeline
events <- list(
  list(day = "Friday, Sept 17", time = "[7:00 PM]", title = "Welcome Drinks",
       place = "[Location]", note = "Casual. Come say hello after your travels."),
  list(day = "Saturday, Sept 18", time = "[4:00 PM]", title = "Ceremony",
       place = site$venue, note = "[Attire: e.g., garden formal]"),
  list(day = "Saturday, Sept 18", time = "[5:00 PM]", title = "Cocktails & Reception",
       place = site$venue, note = "Dinner, dancing, and a sunset over the lake."),
  list(day = "Sunday, Sept 19", time = "[10:00 AM]", title = "Farewell Brunch",
       place = "[Location]", note = "Drop in before you head home.")
)

travel <- list(
  getting_there = list(
    list(icon = "airplane", title = "By air",
         text = "Chicago O'Hare (ORD) and Midway (MDW) are the largest nearby airports. South Bend (SBN) is closer but has fewer flights. [Confirm drive times.]"),
    list(icon = "train-front", title = "By train",
         text = "Amtrak serves New Buffalo directly. [Confirm route and station details.]"),
    list(icon = "car-front", title = "By car",
         text = "New Buffalo sits right off I-94, about 90 minutes from downtown Chicago without traffic.")
  ),
  hotels = list(
    list(name = "[Hotel one]", detail = "[Room block code / deadline]", url = "#"),
    list(name = "[Hotel two]", detail = "[Room block code / deadline]", url = "#"),
    list(name = "Vacation rentals", detail = "There are plenty of cottages near the beach. Book early, since September weekends fill up.", url = "#")
  )
)

registry <- list(
  list(name = "[Registry one]", url = "#"),
  list(name = "[Registry two]", url = "#"),
  list(name = "Honeymoon fund", url = "#")
)

faq <- list(
  list(q = "When should I RSVP by?",
       a = if (site$rsvp_open) {
         paste0("Please RSVP by ", format(site$rsvp_deadline, "%B %d, %Y"), ".")
       } else {
         "RSVPs will open when formal invitations go out. We'll let you know!"
       }),
  list(q = "Can I bring a plus-one?",
       a = "If your invitation includes a guest, their name will be on it. [Edit as needed.]"),
  list(q = "Are kids welcome?",
       a = "[Your policy here.]"),
  list(q = "What should I wear?",
       a = "[Dress code.] The ceremony is [outdoors/indoors], so plan for a lakeside breeze in September."),
  list(q = "Is there parking at the venue?",
       a = "[Parking details.]"),
  list(q = "Why phalaropes?",
       a = "Phalaropes are shorebirds with reversed roles: the more colorful females court the males. They spin on the water to stir up food. We think they're a pretty good match for us.")
)
