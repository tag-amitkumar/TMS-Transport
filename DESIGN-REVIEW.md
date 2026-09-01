# Design review — TMS-UI-Design-V2

A review of the 30-screen design deck, the ambiguities found in it, and the
decision taken for each. Every decision below is implemented in the code; where
a decision deviates from the deck it is called out explicitly.

## What the deck covers

Thirty screens, and the sidebar has no item without one:

| Group | Screens |
|---|---|
| Auth | Login (5 role presets) |
| Administration | Dashboard, Branches, Users |
| Relationships | Clients, Vendors |
| Operations | Cargo Bookings (list + new), Cargo Moto, Consignments (LR), Multiple Consignment, E-Way Bill, POD, Shipment Tracking, Live GPS |
| Fleet & People | Vehicles, Drivers, Pin Code Mapping |
| Support | Complaints |
| Accounts | Invoices & Billing, Payments & Ledger |
| HRMS | Employees, Attendance & Leave, Payroll |
| Insights | Reports & Analytics |
| Portals | Branch, Customer, Vendor dashboards |
| System | Security & Roles, Settings |

The domain spine is coherent and well thought through:

```
Booking → Vehicle Allocation → Consignment (LR) → E-Way Bill
        → Dispatch → GPS Tracking → POD → Invoice → Payment
```

with HRMS payroll drawing driver trip allowances off the same trip records that
drive vendor settlement.

---

## Gaps, and what was decided

### 1. "Cargo Moto" is never defined

The screen is a kanban over bookings grouped by allocation stage, but it uses a
different number series (`BKG-4482`) from the Cargo Bookings list (`BK-30241`),
and its count (13) cannot be reconciled with the list's 342.

**Decision:** treated as a *view* over the same bookings, grouped by how far each
has progressed through allocation — not a separate booking type. The count
mismatch was a mockup artifact. Implemented in `R/mod_cargo_moto.R`.

### 2. Booking numbers are inconsistent across five screens

`BK-30241`, `BKG-4482`, `BKG-4471`, `BKG-4490` all appear for the same entity.

**Decision:** standardised on the `BKG-` series (`PREFIX$booking`), allocated on
save — which is what the New Booking form's own subtitle ("auto-numbered on
save") promises.

### 3. The Trip Book has no screen — the largest structural gap

Trips (`TRP-2260`, `TRP-2214`) are referenced as an existing concept on five
screens: Live GPS shows a trip id, the driver 360° links to one, load
consolidation groups by one, vendor settlement pays per one, and the payroll run
states it pulls allowances "from the Trip Book, zero re-entry". No screen
manages them.

**Decision — deviates from the deck:** added a Trip Book screen under Operations
(`R/mod_trips.R`). Without it the payroll and vendor-payment figures would have
no visible provenance. A trip is one vehicle + one driver between two points,
carrying the allowances and advances that flow into payroll, and it is the
grouping key for consolidated loads.

### 4. Two identifiers per consignment

`CN-90841` and `LR-8841` sit on the same row.

**Decision:** both kept, and they mean different things. `CN-` is the internal
record id; `LR-` is the number printed on the physical lorry receipt the driver
carries and the consignee signs. The LR series is a statutory document series;
CN is free to be renumbered.

### 5. "Clients" vs "Customer"

The sidebar says Clients; every screen says Customer; the dashboard card says
"Total Customers 214" against a Client Master of 214.

**Decision:** one entity. Stored as `clients`, the rail label is kept, and
customer-facing screens say Customer.

### 6. Driver is both an Employee and a Driver

48 drivers in the driver master, and the Employees filter also shows "Drivers
48". The driver panel's own note says "ONE RECORD, EVERYWHERE".

**Decision:** `drivers` is not a second people table — it is the
licence/depot/performance extension of an employee record, joined on
`driver_id == employee_id`. Adding a driver requires an existing employee; the
UI enforces this.

### 7. Vendor owners share names with drivers

"Rajesh Yadav (owner)" and "Suresh Khan (owner)" appear in vendor payments while
the same names appear in the driver master.

**Decision:** treated as coincidence in the mockup, not identity. Vendors are
external parties with their own master; a vendor owner is not an employee. The
seed keeps the name pools separate.

### 8. Counts contradict each other across screens

156 vs 62 vs 52 vehicles; 18 vs 9 branches; 81 employees but "Drivers 142/150"
in HR reports; 148 users but role chips summing to ~600.

**Decision:** one coherent dataset, every count derived from it. 18 branches (9
of them routing depots, which is what Pin Code Mapping counts), 62 vehicles, 48
drivers, 81 employees, 214 clients, 36 vendors, 148 internal users. The
Security screen's Driver/Vendor/Customer chips count the corresponding masters
rather than internal logins, which is what those larger numbers represent.

### 9. Company domain is inconsistent

`amardiptms.in` on the login and support address, `amardiprc.in` on user emails.

**Decision:** `amardiptms.in` throughout.

### 10. GST treatment is unstated

RCM 5%, FCM 12% and FCM 18% all appear, with only the hint "RCM flagged per
client configuration".

**Decision:** GST mode and rate are stored **per client** and copied onto the
booking, never typed by the booking clerk. Under reverse charge the carrier
collects no tax — the recipient discharges it — so an RCM invoice's tax line is
zero and its total equals the freight. Getting this wrong would overstate
revenue by 5% across most of the book. Ancillary charges (detention, demurrage)
are a service rather than GTA freight and carry forward charge at 18%
regardless of the client's freight treatment.

### 11. Screens referenced but not drawn

Route Planning (button on Cargo Moto), Audit Log (button on Security), Fuel
Reports, Maintenance scheduling, a Notifications centre, and a driver mobile app.

**Decision:** Route Planning and Audit Log are implemented as modals off their
existing buttons, both reading real data. Fuel and Maintenance scheduling are
**not built** — they need a cost model the deck does not specify. The driver
mobile app is out of scope for a Shiny web app; POD upload is available to the
Vendor portal and to internal staff instead.

### 12. Six integrations cannot be real without credentials

WhatsApp Business API (Gupshup), SendGrid, MSG91, GST/e-Way Bill GSP, GPS
telematics, Razorpay.

**Decision:** every connector ships in a `Not configured` state and **no
credential is stored anywhere in this repository**. The Settings screen records
which providers are intended and names the environment variables a real
deployment would supply; it never offers a key field. Where a feature depends on
a connector, the UI says so plainly rather than pretending to have sent
something:

- E-way bill generation issues a locally-generated reference and says the GSP is
  not connected.
- Notification events are recorded in-app; delivery warns that a gateway is
  required.
- GPS positions come from the `gps_pings` table. The refresh loop, deviation
  detection and alerting all work against that table, so wiring a real feed
  means writing into it on a schedule — no screen code changes.

---

## Decisions taken during the build

Things the deck could not have anticipated, resolved while making the data real.

**Month-to-date cards read zero on the 1st.** Every "MTD" KPI in the deck is
empty on the first day of a month, which reads as a broken feed rather than a
new period. Financial and operational cards use a rolling 30-day window
(`since_30d()`); genuinely monthly things — the payroll run, the
deliveries-per-month chart, the attendance Month tab — still use calendar months.

**Payroll runs for the month that has closed.** Attendance must be finalised and
trip allowances totalled before anything can be paid, so the seeded run is the
previous complete month and the screen defaults to the latest period present in
the data.

**A trip advance tracks the allowance it funds.** Advances are the float a driver
draws for tolls, fuel and food on a trip. Sampled independently they exceeded a
month's pay and put two-thirds of the fleet on hold; they are now proportional
to the per-trip allowance, which is the relationship the deck's own payroll row
shows (₹19,400 allowance against ₹22,150 advances).

**Freight is not linear in distance.** A flat rate-per-km with a floor collapsed
most short hauls onto the floor and made every invoice the same number. Freight
is now a fixed cost to put a truck on the road, plus a per-tonne handling
component, plus a per-tonne-km line haul.

**E-way bills exist only for consignments on the road.** A delivered
consignment's bill is spent; keeping it in the register filled the screen with
hundreds of dead "Expired" rows and buried the handful that genuinely needed
renewing before a check-post stopped a vehicle.

**Basemap tiles.** The deck's pale Positron basemap is now behind an API key at
both CARTO and Stamen. OpenStreetMap's keyless tiles are used and desaturated in
CSS to get the same muted ground without a credential.

---

## What is enforced, not just displayed

Rules the deck states in passing that are implemented as actual constraints:

- **The POD gate.** "Invoices generate automatically from delivered consignments
  (POD required)" — an invoice cannot be raised against a consignment without a
  verified or approved POD. Re-checked at write time, not just in the queue view.
- **Capacity.** A vehicle cannot be allocated a load heavier than its rated
  capacity; the allocation dialog refuses it and explains why.
- **Licence and document expiry.** Allocation warns on an expiring licence and
  blocks on expired vehicle documents.
- **Branch scoping.** Applied at the data layer (`scope_branch()`), so a Branch
  Admin's dashboard, tables and charts all show one branch's slice.
- **Portal isolation.** A Customer session can only read its own consignments,
  a Vendor session only its own trips (`scope_owner()`), enforced on the data
  rather than by hiding UI.
- **Server-side permission checks.** Every mutating handler calls
  `require_perm()` before touching the store. Hiding a button is defence in
  depth, not the defence.
- **Receipt reconciliation.** A receipt updates the invoice's paid amount and
  flips its status, so every outstanding figure in the app derives from one
  calculation.

---

## What this is, and is not

This is a **working prototype**: all 31 screens navigable, real reactive data,
CRUD on the master tables, and the booking → allocation → LR → POD → invoice →
payment chain actually functioning end to end. 143 automated checks cover
referential integrity, the domain rules above, RBAC and scoping.

It is **not** a production ERP. Not built: fuel and expense management,
maintenance scheduling, a driver mobile app, GSTR filing, multi-currency, and
any live integration. Flat-file storage (chosen for zero-setup portability) has
no transactions and no referential integrity — the storage layer in
`R/store.R` is deliberately database-shaped so that swapping in SQLite or
Postgres is a rewrite of that one file rather than of 31 screen modules.
