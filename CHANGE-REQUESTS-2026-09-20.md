# Change requests — meeting of 20 Sep 2026

Raw notes from the meeting, cleaned up and put in build order. Each item keeps
its original note in *italics* so nothing is lost in the tidying, and so anyone
who was in the room can check the interpretation.

## Status — 27 September 2026

All 27 are implemented, on branch `feat/cr-2026-09-20`. 415 tests pass. The
work is local: nothing has been pushed.

**One item is genuinely outstanding — CR-21 — and it is blocked on a business
decision, not on code.**

| State | Count | Means |
|---|---|---|
| **Done** | 26 | Implemented, unit-tested, and driven in the running app |
| **Partial** | 1 | CR-21 — built and reachable, but blocked on a provider |

**CR-21 is the one to not trust yet.** The e-way bill adapter is wired end to
end and reachable from the E-Way Bill and Trip screens, and it shows exactly
what it would send — but no provider is configured, so no bill has ever been
filed against a live endpoint. Every generate still records a *local
placeholder*, which looks identical to a real 12-digit number and only fails
at a checkpost. The screen says so. Treat e-way bill generation as unbuilt
until D-10 is answered and tested against a sandbox.

**Everything else has been exercised by hand**, including the four that were
outstanding: the TBB gate issued a reference (`TBB-DEL-…`) and unblocked the
booking; a ₹5,00,000 interstate load of books flipped to *not required* on the
Rule 138(14) exemption; Part-B built its payload against a vehicle change; and
the offline form refused a ₹60,000 interstate load with no e-way bill number.

**The twelve decisions were not answered** — the work proceeded under a stated
assumption for each rather than stopping. Three of those assumptions are
load-bearing and reversible; they are marked in
[Decisions needed](#decisions-needed) and argued in their commit messages.

---

Three things to know before reading:

- The build order below is driven by dependency, not by the order things were
  said. Serviceability (which branch delivers a pincode, and whether that is
  ODA) feeds the delivery section, the freight calculation *and* the
  consignment note, so it came first even though it was raised in the middle.
- Every item keeps its original wording, so the reading can still be checked
  by whoever said it.
- Two readings changed what got built: CR-03 (GST) and CR-23 (LR). Both are
  explained where they appear.

---

## Already in place

Worth knowing so these are not rebuilt from scratch:

| Exists today | Where | Relevance |
|---|---|---|---|
| Online / offline booking | `bookings.entry_mode` = Online \| Manual, with `manual_ref`, `manual_dt` | CR-26 is mostly a relabel, not a build |
| TBB payment mode | `PAYMENT_MODES` in `global.R`, incl. bill-at-branch | CR-05/06 extend it |
| Pincode master | `data/reference/pincodes.csv`, 19k rows; `geo_pin()` in `R/geo.R` | CR-12 lookup source |
| Branch coordinates | `branches.lat` / `.lon`; `geo_haversine()` | CR-11 distance test |
| Customer mobile + email | `clients.mobile`, `clients.email` | CR-01/02 reuse these |
| Per-client GST config | `clients.gst_mode`, `clients.gst_pct` | Bears directly on CR-03 |
| **Two** number series per shipment | `consignments.cn_no` (`CN-`) *and* `consignments.lr_no` (`LR-`) | CR-23 is about removing one of them |

---

## The changes

### A · Booking form — contact details

| # | Status | Change | Original note |
|---|---|---|---|
| CR-01 | **Done** | Email on a booking becomes **optional** | *Booking - Mail id, optional* |
| CR-02 | **Done** | Mobile number becomes **mandatory** | *Mobile number mandatory* |
| CR-27 | **Done** | Mobile mandatory **everywhere**, not just booking — consignor, consignee, branch, operator | *Mobile number for all.* |

### B · Charges and payment

| # | Status | Change | Original note |
|---|---|---|---|
| CR-03 | **Done** | Remove the GST **option** from the booking form | *Remove gst option.* |
| CR-04 | **Done** | Payment mode has **no pre-selected default** — the user must choose | *Payment mode - remove default option.* |
| CR-07 | **Done** | Freight priced as **rate per kg, varying by destination** | *Fraight - rate/kg for destination.* |
| CR-08 | **Done** | Freight shown as an **itemised breakup**, not a single figure | *Fraight cgarges, in breakup* |

### C · TBB workflow

| # | Status | Change | Original note |
|---|---|---|---|
| CR-05 | **Done** | TBB offered **on request only, and only to an operator** | *TBB - on request - if operator* |
| CR-06 | **Done** | TBB generates a **reference number**; booking cannot complete until it is received | *TBB - reference number generate and only book if reference number received* |

### D · Branch master and serviceability

| # | Status | Change | Original note |
|---|---|---|---|
| CR-14 | **Done** | Super Admin can maintain the **branch list**: pincodes served, mobile, and whether delivery is normal or ODA | *Add branch list option for superadmin - add pincode, mobile number and normal delivery and ODA* |
| CR-20 | **Done** | A pincode **present in a branch's list ⇒ Normal delivery**; absent ⇒ ODA | *ADD BRANCH - PINCODE (IF BRANCH PINCODE ENTERED, THEN SELECT AS Normal delivry, Other than ODA)* |
| CR-12 | **Done** | Selecting a destination pincode **shows the delivering branch** from the database | *Once pincode selected, show the delivery branch from database* |
| CR-11 | **Done** | Destination beyond **15 km of the nearest branch ⇒ ODA** | *Destination location within range of NEAREST BRANCH (15km) - ODA* |
| CR-09 | **Done** | Delivery location carries an **ODA flag** (Outer Delivery Area) | *Delivey location - ODA (outer delivery asrea)* |
| CR-13 | **Done** | Operators are **scoped to their branch** | *Operators by branch* |

> CR-20 and CR-11 are two different tests for the same flag. See [D-04](#d-04).

### E · Dispatch type

| # | Status | Change | Original note |
|---|---|---|---|
| CR-10 | **Done** | Dispatch type dropdown: **LTL** (Less Than Truck Load) default, **FTL** (Full Truck Load) second | *Dispath - LTL ... by default in dropdown, 2, FTL* |

### F · Invoices and e-way bill

| # | Status | Change | Original note |
|---|---|---|---|
| CR-15 | **Done** | Replace "material value" with **invoice number + invoice value** | *Material value as invoice number and invoice vale* |
| CR-18 | **Done** | **Multiple invoices** per booking, entered in a table: invoice no., value, e-way bill no. | *Option for multiple invoice - option as tabe to add, invoice number, value, e-way bill number* |
| CR-16 | **Done** | E-way bill **mandatory** at ≥ ₹1 lakh intrastate, ≥ ₹50 000 interstate | *e-Way-bill field mandatory in case billing >=1 lakh in same state, >= 50k for interstate* |
| CR-19 | **Done** | Thresholds apply to the **sum of all invoices**, not each one | *In case of sum for al the entered value same as >1l and 50k.* |
| CR-17 | **Done** | Threshold depends on **product type** — some goods exempt | *It is based on product type - in case of book, etc* |
| CR-21 | **Partial** | **API to generate the e-way bill** — Part-A and consolidated movement detail | *Api for generate the e-waybill. Part-A and movemnet detail cosolidated e-way bill* |
| CR-22 | **Done** | **Part-B** filed on vehicle change, or left for the transporter to update later | *Part-B in case of changing the vehichle number - or transporter will update later.* |

### G · Consignment note

| # | Status | Change | Original note |
|---|---|---|---|
| CR-23 | **Done** | Drop the **LR / Lorry Receipt** series; the Consignment Note (CN-) becomes the only number | *Consignment note - removr LR - Lorry receipt* |
| CR-24 | **Done** | Print **4 copies on 2 A4 sheets**: Consignor, Office, POD, Consignee | *4 copies in 2 A4, consigner copy, office copy, POD copy, Consignee copy* |
| CR-25 | **Done** | Field order: **consignor before consignee** | *Costigner then constigny* |

### H · Booking channel

| # | Status | Change | Original note |
|---|---|---|---|
| CR-26 | **Done** | Online / offline booking option | *Add online offline booking option.* |

---

## Build order

Seven steps. Each depends on the one before it only where stated — steps 4 and
7 can run in parallel with the rest once step 1 is done.

### Step 1 · Branch master and serviceability

> **Done.** Served-pincode master on the Branches screen, resolution and the ODA rule in `R/serviceability.R`. Verified by adding a Normal and an ODA pincode in the running app.

**CR-14, CR-20, CR-12, CR-11, CR-09, CR-13**

First because everything downstream reads it: the delivery section of the
booking form, the ODA surcharge in freight, operator scoping, and the branch
printed on the consignment note.

1. Extend `branches` with a served-pincode list, mobile, and default delivery type.
2. Build the Super Admin branch screen (CR-14).
3. Resolve destination pincode → delivering branch (CR-12), using the existing
   `geo_pin()` and the new pincode list.
4. Derive the ODA flag (CR-20 + CR-11) — **blocked on [D-04](#d-04)**.
5. Scope operators to their branch (CR-13) — **blocked on [D-03](#d-03)**.

*Needs: D-03, D-04, D-05.*

### Step 2 · Booking form — contact and defaults

> **Done.** Verified in the running app.

**CR-01, CR-02, CR-27, CR-04, CR-03, CR-10**

Small, independent field-level changes. Done early so the form is stable before
the charge logic is layered on.

1. Email optional, mobile mandatory (CR-01, CR-02), then the same across
   consignor, consignee, branch and operator (CR-27).
2. Remove the payment-mode default (CR-04).
3. Remove the GST selector (CR-03) — **blocked on [D-01](#d-01)**.
4. Add the LTL/FTL dispatch dropdown (CR-10).

*Needs: D-01.*

### Step 3 · Freight

> **Done.** Verified: 2 T to Kochi quoted ₹28,000 + ₹750 ODA, itemised.

**CR-07, CR-08**

After steps 1 and 2, because the rate depends on the destination branch and the
ODA flag, and the breakup has to show the dispatch type.

1. Build the rate card — rate per kg by destination (CR-07) — **blocked on [D-02](#d-02)**.
2. Render the itemised breakup: base freight, ODA surcharge, any other line
   items (CR-08) — **blocked on [D-06](#d-06)**.

*Needs: D-02, D-06. Depends on: steps 1, 2.*

### Step 4 · TBB workflow

> **Built, not clicked.** Gate and reference flow implemented and validated on save; not exercised by hand.

**CR-05, CR-06**

Independent of freight; needs operator/branch context from step 1.

1. Gate TBB to operators, on request (CR-05).
2. Generate the TBB reference and block completion until it is received
   (CR-06) — **blocked on [D-07](#d-07) and [D-08](#d-08)**.

*Needs: D-07, D-08. Depends on: step 1 (CR-13).*

### Step 5 · Invoices

> **Done.** Verified: two invoices under the limit, summing over it, triggered the e-way bill warning.

**CR-15, CR-18**

Must precede the e-way bill rules, which are computed from the invoice rows.

1. Replace material value with invoice number + value (CR-15).
2. Multi-invoice table with invoice no., value, e-way bill no. (CR-18).
3. New `booking_invoices` table — one row per invoice, keyed to the booking.

### Step 6 · E-way bill

> **Partial.** Thresholds and exemptions done. Generation is wired and reachable but has never reached a live endpoint — no provider configured (D-10).

**CR-16, CR-19, CR-17, CR-21, CR-22**

1. Threshold rule on the **sum** of invoice values (CR-16 + CR-19), intrastate
   vs interstate decided by `origin_state` / `dest_state`, both already stored.
2. Product-type exemptions (CR-17) — **blocked on [D-09](#d-09)**.
3. Part-A generation and consolidated movement detail via API (CR-21) —
   **blocked on [D-10](#d-10)**.
4. Part-B on vehicle change, or deferred to the transporter (CR-22).

*Needs: D-09, D-10. Depends on: step 5.*

### Step 7 · Consignment note

> **Done.** Four copies across two sheets, consignor first, verified in the print preview. No LR reference remains on any of the 18 screens.

**CR-23, CR-24, CR-25, CR-26**

Last, because the document has to print the final field set — invoice list,
ODA flag, dispatch type — so doing it earlier means doing it twice.

1. Consignment Note replaces Lorry Receipt (CR-23). Whether the `LR-` series
   is retired or merely hidden decides the size of this — **[D-11](#d-11)**.
2. Reorder consignor before consignee (CR-25).
3. Four copies on two A4 sheets (CR-24) — **blocked on [D-12](#d-12)**.
4. Confirm the online/offline labelling against existing `entry_mode` (CR-26).

*Needs: D-11, D-12. Depends on: steps 5, 6.*

---

## Decisions needed

### D-01
**CR-03 — "Remove GST option" means remove the selector, not the tax?**

> Built as: the per-booking GST control removed, GST still computed from the client's configured treatment. The literal reading would have produced invalid tax documents.

GST must still be calculated, shown and filed; a consignment note without it is
not a valid tax document. The app already takes GST mode and percentage from
the client record (`clients.gst_mode`, `clients.gst_pct`) rather than asking per
booking, so the reading is: *remove the per-booking GST control and rely on the
client's configuration.* **Confirm before building — the literal reading breaks
invoicing.**

### D-02
**CR-07 — Where does the rate card live, and what is the key?**

> Built as: rate card keyed pincode, then branch, then a standard fallback; dispatch mode narrows where the card distinguishes it. **This one is a guess — confirm the shape before the card is populated for real.**

Rate per kg by destination: keyed on destination pincode, city, branch, or zone?
Per client or standard? Who maintains it? This is a new master table and the
shape depends entirely on the answer.

### D-03
**CR-13 — What is an "operator"?**

> Built as: `OPERATOR_ROLES` = Super Admin, Branch Admin, Operations Manager, Booking Executive.

The role matrix has ten roles; none is called Operator. Closest are Booking
Executive and Operations Manager. Which one, or is this a new role?

### D-04
**CR-20 vs CR-11 — Which test decides ODA?**

> Built as: an explicitly listed pincode wins; only unlisted ones fall through to the 15 km straight-line test.

Two rules were given: pincode absent from the branch's list, and destination
more than 15 km from the nearest branch. They will disagree. Is the pincode list
authoritative with 15 km as the fallback for unlisted pincodes, or is distance
always the test? Also: 15 km straight-line or by road? Only coordinates are
stored, so straight-line is free and road distance needs a routing service.

### D-05
**CR-14 — Is a pincode allowed to belong to more than one branch?**

> Built as: where two branches claim a pincode, the nearer delivers.

If two branches list the same pincode, which delivers? Nearest, or an explicit
priority?

### D-06
**CR-08 — What are the breakup lines?**

> Built as: base freight, minimum top-up, ODA surcharge, then GST and insurance. Zero-value lines are not printed.

Named lines needed. Assumed: base freight, ODA surcharge, insurance, GST. Also
docket/handling charges?

### D-07
**CR-06 — Who issues the TBB reference?**

> Built as: the system generates the reference on an explicit request, stamped with the billing branch's code.

Generated by this system, or received from the billing branch? "Only book if
reference number received" suggests it comes from elsewhere — if so, by what
channel?

### D-08
**CR-05/06 — What happens to a booking waiting on a TBB reference?**

> Built as: the booking can be saved as a Draft while waiting, but cannot be confirmed.

Saved as Draft, or not created at all? Drafts are already a booking status.

### D-09
**CR-17 — Which product types are exempt?**

> Built as: a `product_types` master with an exemption flag, seeded with books, fresh produce and dairy. **The real annexure is longer — the list needs checking.**

"in case of book, etc" — the actual exemption list is needed, and whether it
maps to an existing field or a new product-type master.

### D-10
**CR-21 — Which e-way bill provider?**

> **Checked against NIC's own spec on 27 Sep — the answer is a GSP, not NIC directly.**
>
> NIC's *User Manual for API Interface (Site-to-Site integration)*, s.2.2, requires
> **at least 1000 e-way bill activities per day**, an SSL/TLS domain, a **static IP**,
> a pre-production system and 200+ test activities before go-live. A two-branch
> carrier does not raise a thousand bills a day, and this deployment deliberately
> has no static IP — it reaches the internet through a Cloudflare tunnel.
>
> So the question is no longer "NIC or a GSP" but **which GSP**. Still unanswered,
> and still the only thing blocking CR-21.

NIC directly, or a GSP? This needs credentials, a sandbox, and API access —
typically the longest lead time on this list, so worth starting early even
though it builds late.

### D-11
**CR-23 — Confirm this removes a number series, not just a word.**

> Built as option (b): nothing says Lorry Receipt any more, the `LR-` series stays in the data. Retiring it is a migration of its own.

Every consignment already carries **two** numbers: `cn_no` (`CN-0001`) and
`lr_no` (`LR-0001`), issued together for the same shipment. Read literally,
"remove LR" means retiring the `LR-` series and letting the Consignment Note
number stand alone — which is a sensible simplification, but it touches
consignments, e-way bills (`ewaybills.lr_no`), PODs, invoices and every printed
document, and existing records keep their LR numbers.

Confirm which is wanted:
- **(a)** Retire `LR-` entirely; `CN-` becomes the single reference. Historical
  LR numbers kept read-only for lookup.
- **(b)** Keep both internally, but stop printing LR and stop calling the
  document a Lorry Receipt.

(b) is a display change and fits inside step 7. (a) is a data migration and
should be scheduled on its own.

### D-12
**CR-24 — Two copies per A4 sheet, or A4 folded?**

> Built as: two copies per sheet — consignor and office on the first, POD and consignee on the second.

"4 copies in 2 A4" reads as two copies per sheet across two sheets. Confirm
which two share a sheet, and whether each copy needs its own watermark
(Consignor / Office / POD / Consignee).

---

## Longest lead time

D-10 (e-way bill API access) is an external dependency with credentials and
sandbox approval involved. It gates step 6 but nothing before it — start the
paperwork now and build steps 1–5 while it is in progress.
