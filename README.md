# TMS — Transport Management System

A logistics ERP for a road-freight carrier, built in R/Shiny. Bookings,
consignments, fleet, GST compliance, accounts, HRMS and self-service portals for
customers and vendors.

> ### You are on the `minimal-setup` branch
>
> Same application, every feature, but seeded for a clean start rather than a
> showcase: **two logins and one sample record per entity**, instead of the
> 148 accounts and 342 bookings on `main`.
>
> | | `main` | `minimal-setup` |
> |---|---|---|
> | Logins | 148 across 7 roles | **2** — Super Admin, Operations |
> | Bookings | 342 | 2 (one delivered & paid, one in transit) |
> | Customers | 214 | 2 |
> | Vehicles / drivers | 62 / 48 | 2 / 2 |
>
> Use this branch to hand someone a working system they can put their own data
> into. Use `main` to see the app under realistic volume.
>
> The switch is `SEED_PROFILE` in `global.R` (`"minimal"` here, `"demo"` on
> `main`), overridable with `TMS_SEED_PROFILE`. Both profiles generate the same
> tables with the same columns, so nothing downstream knows the difference.

**▶ Live demo: <https://sendwave.shinyapps.io/tms-transport/>** — sign in with a
**Quick preview** chip on the login screen to explore any role. *(That
deployment runs the `demo` profile from `main`.)*

**New here? Start with the [User Handbook](HANDBOOK.html)** — a screenshot-led
walkthrough of every screen, the order you actually do things in, and what each
error message means. Source: [HANDBOOK.Rmd](HANDBOOK.Rmd).

> **About the live demo.** shinyapps.io gives each instance an ephemeral
> filesystem, so anything you create there — a booking, a receipt, an approved
> leave request — works for your session but is discarded when the instance
> restarts, and is not shared with other visitors. The demo also ships a fixed
> snapshot of seed data, so relative figures ("bookings today", GPS "updated N
> minutes ago") drift as the snapshot ages. Run `Rscript R/seed.R` locally for a
> dataset anchored to today.

Built from a 30-screen UI design deck. The review of that deck — every ambiguity
found and the decision taken for each — is in **[DESIGN-REVIEW.md](DESIGN-REVIEW.md)**.

---

## Quick start

```bash
Rscript install_packages.R
```

```bash
Rscript -e "shiny::runApp('.', port = 8788, launch.browser = TRUE)"
```

The app seeds itself on first run — no database, no configuration. Sign in with
any account below, or use the **Quick preview as** chips on the login screen to
jump straight into a role.

| Login | Email | Password | Reaches |
|---|---|---|---|
| **Super Admin** | `admin@amardiptms.in` | `tms@2026` | Everything — all 30 screens, all branches |
| **Operations** | `operations@amardiptms.in` | `tms@2026` | Bookings, allocation, consignments, tracking, POD, fleet, complaints — 16 screens |

Sign-in also accepts the local part alone — `admin` works.

The Operations login is the standard **Operations Manager** role: it does the
bookings and follows them to delivery, but cannot reach Branches, Users,
Invoices, Payments, Security or Settings. Those are Super Admin's.

The other eight roles still exist in the permission matrix under **System →
Security & Roles** — Super Admin can create accounts against any of them at any
time. This branch simply ships with two.

The demo password is shared across all seeded accounts. It is stored
bcrypt-hashed, and this is public demo data — set `TMS_DEMO_LOGIN=false` to hide
the role chips and require real credentials.

---

## What it does

**Operations chain** — the spine of the system, working end to end:

```
Booking → Vehicle Allocation → Consignment (LR) → E-Way Bill
        → Dispatch → GPS Tracking → POD → Invoice → Payment
```

Allocating a vehicle from the Cargo Moto board writes the trip, the consignment
and its first timeline events in one step, and refuses loads that exceed the
vehicle's capacity or ride on expired documents.

**31 screens**

- **Administration** — Dashboard, Branches, Users
- **Relationships** — Clients, Vendors
- **Operations** — Cargo Bookings, Cargo Moto (allocation kanban), Consignments
  (LR), Multiple Consignment (split & consolidation), Trip Book, E-Way Bill,
  POD, Shipment Tracking, Live GPS
- **Fleet & People** — Vehicles, Drivers, Pin Code Mapping
- **Support** — Complaints (SLA-tracked kanban)
- **Accounts** — Invoices & Billing, Payments & Ledger
- **HRMS** — Employees, Attendance & Leave, Payroll
- **Insights** — Reports & Analytics (six report groups, XLSX export)
- **Portals** — Branch, Customer and Vendor dashboards
- **System** — Security & Roles, Settings

Trip Book is not in the original deck — it was added because trips are
referenced by payroll, vendor settlement, consolidation and GPS with no screen
to manage them. See DESIGN-REVIEW.md, gap 3.

## Rules the app actually enforces

Not just displayed — these are constraints, checked server-side at write time:

- **No invoice without a POD.** A consignment must carry a verified or approved
  proof of delivery before it can be billed.
- **GST follows the client.** Reverse-charge freight collects no tax (the
  recipient discharges it); forward-charge does. Configured per client, copied
  onto the booking, never typed by the clerk.
- **Capacity and compliance gate allocation.** Overweight loads, expired
  licences and lapsed vehicle documents block or warn at the point of assignment.
- **Branch scoping and portal isolation** are applied to the data, not by hiding
  UI. A Customer session cannot read another client's consignments.
- **Payroll pulls from the Trip Book.** Driver allowances and advances are summed
  from trip records; payroll staff never re-key them.
- **Every mutation is permission-checked and audit-logged.**

## Roles

Ten access levels with a permission matrix editable at module × action level
(view / create / edit / delete / approve / export):

Super Admin · Branch Admin · Operations Manager · Booking Executive ·
Dispatcher · Accountant · HR Manager · Driver · Vendor · Customer

Super Admin is deliberately not editable — removing its own access would lock
the screen that grants access.

---

## Project layout

```
app.R              entry point: auth gate, router, module wiring
global.R           config, domain vocabulary, formatting helpers
HANDBOOK.Rmd       user handbook source; renders to HANDBOOK.html
docs/img/          handbook screenshots, captured from the running app
tools/             capture-screenshots.js, build-pincodes.R
R/
  store.R          storage layer — the only code that touches files
  geo.R            India PIN code / city master, lane distance and transit
  ewb.R            e-way bill validity and automatic Part-B re-entry
  seed.R           synthetic data generator
  rbac.R           roles, permission matrix, branch & owner scoping
  nav.R            sidebar tree, breadcrumbs, landing pages
  theme.R          Bootstrap tokens matched to the design deck
  ui_helpers.R     stat cards, pills, kanban, timelines, tables
  mod_*.R          one module per screen
www/styles.css     shell, cards, kanban, pills, tables
tests/smoke.R      300 data and invariant checks
tests/functional.R 174 checks that drive the module servers
data/*.csv         seeded data
data/reference/    India PIN code master — read-only, never written to
```

## Data

Flat CSV files under `data/`, read and written through a narrow API in
`R/store.R`. Nothing else in the app touches a file path.

Flat files were chosen for zero-setup portability: clone and run, no database.
The trade-off is real — no transactions, no referential integrity — so the
storage API is deliberately database-shaped (a table name, a filter, a named
list of values) rather than file-shaped. Moving to SQLite or Postgres is a
rewrite of that one file, not of 31 screen modules.

Regenerate the seed at any time — this resets `data/` to a clean state:

```bash
Rscript R/seed.R
```

To load the full demo dataset on this branch instead, set the profile:

```bash
TMS_SEED_PROFILE=demo Rscript R/seed.R
```

**All data is fictional.** Names, GSTINs, PANs, bank accounts, Aadhaar fragments
and phone numbers are generated from fixed patterns and belong to no real person
or company. GSTINs are structurally shaped but carry an invalid checksum on
purpose so they cannot be mistaken for live registrations.

### Reference geography

`data/reference/` holds the India PIN code master — 19,238 PIN codes, 630
district-level cities across all 36 states and union territories, and the
aliases people actually type (Bangalore, Noida, Gurugram). It is **reference
data, not application state**: nothing in the app writes to it, so it bypasses
`R/store.R` and is read once per process by `R/geo.R`.

Rebuild it from a fresh GeoNames export with:

```bash
Rscript tools/build-pincodes.R path/to/IN.txt
```

> **Attribution.** The PIN code master is derived from the
> [GeoNames](https://www.geonames.org/) postal-code export for India, used
> under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Ladakh is
> restored as its own union territory and Delhi's nine revenue districts are
> collapsed to one city; both corrections are made in the build script.
>
> About a quarter of Indian PIN codes carry a city-level fallback coordinate
> rather than a real fix, concentrated in the metros. The app carries that
> accuracy grade through and declines to estimate a distance for a local lane
> whose two ends share a point, rather than quoting a figure it cannot
> support. See DESIGN-REVIEW.md.


## Branding

Everything the operator's identity touches — the page title, the sidebar, the
login card, printed lorry receipts and invoices, generated email addresses and
the support contact — reads from the `BRAND` block at the top of `global.R`.
Rebranding is that block plus one image; there is nothing to chase through the
screen modules.

```r
BRAND <- list(
  company = "MoveWing Logistics Pvt. Ltd.",
  short   = "MoveWing",
  unit    = "Logistics Pvt. Ltd.",
  product = "Transport Management System",
  domain  = "movewinglogistics.in",
  logo    = "logo.png",
  navy    = "#12275C",
  orange  = "#E9631A"
)
```

**The logo.** `www/movewing_logo.png` — a square, transparent-background
lock-up. It appears on the login card, on printed lorry receipts and as the
browser tab icon. Point `BRAND$logo` at a different filename to swap it; if the
file is missing the app draws an "MW" monogram in the brand colours instead, so
a clone without the artwork never shows a broken image.

The navy sidebar keeps the monogram either way: the supplied lock-up is a wide
mark on a white ground, and dropping it into a dark rail would put a white slab
down the side of every screen.

## Tests

```bash
Rscript tests/smoke.R
Rscript tests/functional.R
```

**300 + 174 = 474 checks.** `smoke.R` covers module wiring, seed referential
integrity, the domain rules above, RBAC denials for every role, branch and
owner scoping, the storage round-trip, the PIN code master and lane estimation,
datetime coercion, e-way bill validity, and the formatting helpers. `functional.R` drives the module servers through
`shiny::testServer` and asserts on what actually landed in the store — it
exists because render-only tests once let an unsubmittable booking form ship.

## Integrations

Six connectors are modelled — WhatsApp, transactional email, SMS, GST/e-way-bill
GSP, GPS telematics, payment gateway — and **all ship disconnected. No credential
is stored in this repository.** A real deployment supplies them as environment
variables (`.Renviron` locally, platform secrets when hosted); the Settings
screen names the variables but never offers a key field.

Where a feature depends on a connector the UI says so rather than pretending to
have sent something. E-way bill generation issues a local reference and states
the GSP is not connected; notification events are recorded in-app with a warning
that delivery needs a gateway.

---

## Status

A working prototype: every screen navigable, real reactive data, CRUD on the
master tables, and the operations chain functioning end to end.

Not a production ERP. Not built: fuel and expense management, maintenance
scheduling, a driver mobile app, GSTR filing, multi-currency, and any live
integration.

## Requirements

R ≥ 4.3. Dependencies: shiny, bslib, dplyr, tidyr, purrr, stringr, readr,
lubridate, scales, DT, plotly, leaflet, shinyWidgets, bcrypt, uuid, jsonlite,
openxlsx, fontawesome, htmltools.

`leaflet` backs one screen (Live GPS) and is loaded defensively — the app runs
without it, that screen degrades to a notice.

## Licence

MIT — see [LICENSE](LICENSE).
