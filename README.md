# ej-wedding: Emily & Jeff wedding website (Shiny)

A Shiny + bslib site styled after the phalarope illustration and save-the-date.

## Run locally

```r
install.packages(c("shiny", "bslib", "bsicons", "googlesheets4"))
shiny::runApp("ej-wedding")
```

## Structure

| File | Purpose |
|---|---|
| `app.R` | Theme, page layout, server logic (countdown, RSVP) |
| `R/content.R` | **All editable text**: names, dates, events, travel, FAQ, registry links. Placeholders are in `[brackets]`. |
| `R/storage.R` | RSVP storage: Google Sheets by default, local CSV optional (`rsvp_backend`). |
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

For local testing without Sheets, set `rsvp_backend <- "csv"` in `R/storage.R`.

## RSVP guest code

Guests enter the code from their invitation before the form appears. Set it in `.Renviron` (or your host's environment variables):

```
RSVP_GUEST_CODE=PHALAROPE            # one shared code
RSVP_GUEST_CODE=SMITH27,JONES27      # or several, e.g. one per household
```

Matching ignores case, spaces and dashes. Each RSVP row records which code was used (`guest_code`). After 5 wrong tries, a browser session is locked and the guest is asked to email you. If no code is set, nobody can RSVP and the server log shows a warning.

## Before deploying

- Replace every `[placeholder]` in `R/content.R`. Confirm the travel details: airport, Amtrak, and drive times.
