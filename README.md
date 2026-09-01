# TMS — Transport Management System

A logistics ERP for a road-freight carrier, built in R/Shiny. Bookings,
consignments, fleet, GST compliance, accounts, HRMS and self-service portals for
customers and vendors.

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

| Role | Email | Password |
|---|---|---|
| Super Admin | `amardip.singh@amardiptms.in` | `tms@2026` |
| Branch Admin | `vikram.chauhan@amardiptms.in` | `tms@2026` |
| Accountant | `ganesh.malhotra@amardiptms.in` | `tms@2026` |
| HR Manager | `arif.kamble@amardiptms.in` | `tms@2026` |

Sign-in also accepts the local part alone — `amardip.singh` works.

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
R/
  store.R          storage layer — the only code that touches files
  seed.R           synthetic data generator
  rbac.R           roles, permission matrix, branch & owner scoping
  nav.R            sidebar tree, breadcrumbs, landing pages
  theme.R          Bootstrap tokens matched to the design deck
  ui_helpers.R     stat cards, pills, kanban, timelines, tables
  mod_*.R          one module per screen
www/styles.css     shell, cards, kanban, pills, tables
tests/smoke.R      143 checks
data/*.csv         seeded data
```

## Data

Flat CSV files under `data/`, read and written through a narrow API in
`R/store.R`. Nothing else in the app touches a file path.

Flat files were chosen for zero-setup portability: clone and run, no database.
The trade-off is real — no transactions, no referential integrity — so the
storage API is deliberately database-shaped (a table name, a filter, a named
list of values) rather than file-shaped. Moving to SQLite or Postgres is a
rewrite of that one file, not of 31 screen modules.

Regenerate the seed at any time:

```bash
Rscript R/seed.R
```

**All data is fictional.** Names, GSTINs, PANs, bank accounts, Aadhaar fragments
and phone numbers are generated from fixed patterns and belong to no real person
or company. GSTINs are structurally shaped but carry an invalid checksum on
purpose so they cannot be mistaken for live registrations.

## Tests

```bash
Rscript tests/smoke.R
```

143 checks covering module wiring, seed referential integrity, the domain rules
above, RBAC denials for every role, branch and owner scoping, the storage
round-trip, and the formatting helpers.

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
