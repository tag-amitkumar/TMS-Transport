# ==================================================================
# WhatsApp message templates, one per shipment stage.
#
# WHY THESE ARE DATA AND NOT STRINGS IN THE CODE
# ----------------------------------------------
# A WhatsApp template is not ours to edit freely. Every business-initiated
# message outside the 24-hour service window has to use a template that Meta
# has reviewed and approved, and the approved text is what gets sent — change
# a word and it needs re-approval. So each one carries the name Meta knows it
# by, its category, its language, and its approval state, and the body here is
# a copy of what was submitted rather than the source of truth.
#
# Variables are positional — {{1}}, {{2}} — because that is what the API
# takes. The `vars` field records what each position means, since "{{3}}" in a
# support ticket six months from now is otherwise unreadable.
#
# WRITING THEM
# ------------
# Utility templates are approved on the basis that they are expected,
# transactional and useful. Each of these is tied to something the customer
# asked for by handing over a consignment, says what happened and what it
# means for them, and carries the one number they would otherwise ring the
# branch to ask for. None of them sells anything — a marketing line in a
# utility template is the usual reason for rejection.
#
# Keep them short. These are read on a phone, often by a driver or a
# storekeeper, frequently in sunlight.
# ==================================================================

#' The stages a customer hears about.
#'
#' Deliberately not every status the system tracks. "In Prep" and "At Hub" are
#' internal: a customer who gets a message every time a parcel moves between racks
#' stops reading them, and the one that mattered goes unread with the rest.
NOTIFY_STAGES <- c(
  "booking_confirmed",
  "vehicle_allocated",
  "dispatched",
  "out_for_delivery",
  "delivered",
  "pod_uploaded",
  "delivery_delayed"
)

#' Seed the template master. Shared by both seed profiles.
#'
#' `status` is Meta's, not ours: Draft until submitted, Pending while under
#' review, Approved once it may be sent. Nothing sends on a template that is
#' not Approved — see notify_send() — because attempting it is an API error
#' and, repeated, a quality-rating problem for the whole number.
seed_message_templates <- function() {
  tibble::tribble(
    ~template_id, ~stage, ~meta_name, ~category, ~language, ~status, ~vars, ~body,

    "TPL-001", "booking_confirmed", "tms_booking_confirmed", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=origin|4=destination|5=expected delivery date",
    # Rejected by Meta first time round as INCORRECT_CATEGORY. The offending
    # line was "Track it any time with your consignment number and delivery
    # PIN code" — an invitation to engage, which reads as marketing however
    # useful it is. A utility template states what happened to a transaction
    # the customer is already party to and stops there. The company name went
    # with it: the sender is already identified, and repeating it is brand
    # presence, not information.
    paste0("Hello {{1}}, consignment {{2}} has been booked.\n\n",
           "From: {{3}}\nTo: {{4}}\nExpected delivery: {{5}}"),

    "TPL-002", "vehicle_allocated", "tms_vehicle_allocated", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=vehicle number|4=driver name|5=driver mobile",
    paste0("Hello {{1}}, a vehicle has been allotted to consignment {{2}}.\n\n",
           "Vehicle: {{3}}\nDriver: {{4}} ({{5}})\n\n",
           "You will hear from us again when it leaves our branch."),

    "TPL-003", "dispatched", "tms_dispatched", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=destination|4=expected delivery date|5=vehicle number",
    paste0("Hello {{1}}, consignment {{2}} has left our branch and is on its way to {{3}}.\n\n",
           "Expected delivery: {{4}}\nVehicle: {{5}}\n\n",
           "We will message you again before the vehicle reaches you."),

    "TPL-004", "out_for_delivery", "tms_out_for_delivery", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=driver name|4=driver mobile|5=number of packages",
    paste0("Hello {{1}}, consignment {{2}} is out for delivery today.\n\n",
           "Driver: {{3}} ({{4}})\nPackages: {{5}}\n\n",
           "Please keep someone available to receive and sign for the goods."),

    "TPL-005", "delivered", "tms_delivered", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=delivery date and time|4=received by",
    paste0("Hello {{1}}, consignment {{2}} was delivered on {{3}}.\n\n",
           "Received by: {{4}}\n\n",
           "Thank you for shipping with MoveWing Logistics."),

    "TPL-006", "pod_uploaded", "tms_pod_uploaded", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=delivery date",
    paste0("Hello {{1}}, the signed proof of delivery for consignment {{2}} ",
           "({{3}}) is now on file.\n\n",
           "Ask your branch for a copy if you need one for your records."),

    # The one message nobody wants to send and everybody wants to receive.
    # Saying nothing when a load is late is what turns a delay into a
    # complaint, so this is a stage in its own right rather than an exception.
    "TPL-007", "delivery_delayed", "tms_delivery_delayed", "UTILITY", "en", "Draft",
    "1=consignee name|2=consignment no|3=reason|4=revised delivery date|5=branch phone",
    paste0("Hello {{1}}, consignment {{2}} is running late.\n\n",
           "Reason: {{3}}\nRevised delivery: {{4}}\n\n",
           "We are sorry for the delay. Call us on {{5}} if this causes you a problem.")
  )
}

#' Fill a template body with its variables, for preview and for the outbox.
#'
#' Positional on purpose, matching the API: vars[1] replaces {{1}}. A missing
#' variable is left visible as its placeholder rather than blanked, because a
#' message reading "delivered on {{3}}" is obviously broken, while one reading
#' "delivered on " looks finished and is worse.
template_fill <- function(body, vars) {
  out <- as.character(body)
  if (!length(vars)) return(out)
  for (i in seq_along(vars)) {
    v <- as.character(vars[[i]] %||% "")
    if (nzchar(v)) out <- gsub(paste0("\\{\\{", i, "\\}\\}"), v, out)
  }
  out
}

#' The template for a stage, or NULL.
template_for <- function(stage) {
  t <- store_get("message_templates")
  if (!nrow(t)) return(NULL)
  hit <- t[t$stage == stage, , drop = FALSE]
  if (!nrow(hit)) NULL else hit[1, ]
}
