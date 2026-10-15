# ej-wedding: Emily & Jeff wedding website (Shiny)

A Shiny + bslib site styled after the phalarope illustration and save-the-date.

## Run locally

```r
install.packages(c("shiny", "bslib", "bsicons", "googlesheets4", "DT"))
shiny::runApp("ej-wedding")
```

## Structure

| File | Purpose |
|---|---|
| `app.R` | Theme, page layout, server logic (countdown, RSVP, couple sign-in) |
| `R/content.R` | **All editable text**: names, dates, events, travel, FAQ, registry links. Placeholders are in `[brackets]`. |
| `R/storage.R` | Storage for RSVPs, the board and the planning tracker: Google Sheets by default, local CSV with `STORAGE_BACKEND=csv`. |
| `R/board.R` | Community board |
| `R/couple_auth.R` | Couple-only sign-in |
| `R/planning.R` | Private vendor and budget tracker |
| `R/ui_helpers.R` | Small reusable UI pieces (section titles, cards, water-line divider) |
| `www/styles.css` | Palette, typography, layout |
| `www/img/` | Illustration (transparent background, pencil notes removed), save-the-date, favicon |

## RSVP storage: Google Sheets

1. In [Google Cloud Console](https://console.cloud.google.com/): create a project, enable the **Google Sheets API**, create a **service account**, and download a JSON key for it.
2. Create a blank Google Sheet. Share it with the service account's `client_email` (from the JSON key) as **Editor**.
3. Copy `.Renviron.example` to `.Renviron` and fill in:
   - `RSVP_SHEET_ID`: the ID from the sheet URL (`docs.google.com/spreadsheets/d/<ID>/edit`)
   - `GS4_SERVICE_ACCOUNT_JSON`: the path to the key file (e.g. `secrets/rsvp-service-account.json`), or the JSON text itself
4. The header row is written on the first RSVP. If you add your own header, it must match the app's columns exactly. The app refuses to append to a sheet whose header doesn't match.

**Deploying:**
- **shinyapps.io:** deploy `.Renviron` and the key file with the app. They're git-ignored but not deploy-ignored.
- **Posit Connect Cloud:** set both values as environment variables, and paste the full JSON text into `GS4_SERVICE_ACCOUNT_JSON`.

For local testing without Sheets, set `STORAGE_BACKEND=csv` (saves to `data/*.csv`, which is git-ignored).

## RSVP guest code

Guests enter the code from their invitation before the form appears. Set it in `.Renviron` (or your host's environment variables):

```
RSVP_GUEST_CODE=PHALAROPE            # one shared code
RSVP_GUEST_CODE=SMITH27,JONES27      # or several, e.g. one per household
```

Matching ignores case, spaces and dashes. Each RSVP row records which code was used (`guest_code`). After 5 wrong tries, a browser session is locked and the guest is asked to email you. If no code is set, nobody can RSVP and the server log shows a warning.

## Community board

The **Community** tab is a message board. Anyone can read it. Guests post and reply after entering their invitation's guest code, the same code that unlocks the RSVP form. Each browser can post once every 15 seconds and 30 times per visit. Notes are stored in a `board` tab of the RSVP Sheet, created on first use.

When you're signed in (see below), your posts get a **The couple** badge, and each note has a **Hide** link. A hidden note stays in the Sheet with `hidden` set to `yes`. Clear that cell to bring the note back. Other people's new notes show up within about a minute.

## Planning tracker (just the two of you)

Add `?couple` to the end of the site address (for example `https://…/?couple`) to get a sign-in box. Each of you has your own password, set in `.Renviron` or your host's environment variables. Passwords must be at least 10 characters:

```
ADMIN_PASSWORD_EMILY=...
ADMIN_PASSWORD_JEFF=...
```

The variable names come from the names in `R/content.R`. After you sign in, a **Planning** tab appears with three views:

- **Overview:** total budget, projected spend, paid, still owed, and how much is left, plus spending by category and upcoming payments.
- **Vendors:** add, edit (double-click a row) or delete vendors, with status, estimate, contracted cost, paid so far, next due date, contacts and notes. Projected cost is the contracted cost once there is one, otherwise the estimate. Vendors marked "Not using" don't count toward totals.
- **Budget:** set a budget for each category.

The data lives in the `vendors` and `budget` tabs of the same Sheet. That's also the place to look at the full history or make bulk edits. Guests never get the Planning tab: the server only builds it after a correct password. Wrong guesses are limited to 5 per visit and 20 per 15 minutes across all visitors. You stay signed in until you sign out or close the tab.

**The Sheet itself is not private to the site.** Anyone you share the Google Sheet with can see the planning tabs, so don't share it with guests.

## Before deploying

- Replace every `[placeholder]` in `R/content.R`. Confirm the travel details: airport, Amtrak, and drive times.
