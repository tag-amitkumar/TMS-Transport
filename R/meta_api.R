# ==================================================================
# WhatsApp Business Platform (Meta Cloud API) adapter.
#
# WHAT THIS NEEDS, AND WHAT IT DOES NOT
# -------------------------------------
# Sending a WhatsApp message to a customer needs four things on OUR side:
#
#   1. A Meta Business Account with the business verified.
#   2. A WhatsApp Business Account (WABA) with a registered phone number -
#      a number that is NOT in use on the normal WhatsApp or Business app.
#   3. A permanent access token for a system user with whatsapp_business_
#      messaging permission.
#   4. Templates submitted and approved by Meta - see R/notify_templates.R.
#
# It needs exactly one thing from the customer: their mobile number, and
# their consent to be messaged on it.
#
# It does NOT need the customer to log in with Facebook, and Facebook Login
# would not help if they did. Facebook Login identifies a person on an app
# and does not hand over a WhatsApp number; the two systems are not joined
# that way. (Meta does have a "Facebook Login for Business" flow, but that is
# embedded signup for onboarding a BUSINESS's own WABA - it is how a carrier
# would onboard its clients' WhatsApp accounts, not how a carrier reaches a
# consignee.) The customer already has WhatsApp; we message the number they
# gave us on the consignment note.
#
# THE 24-HOUR WINDOW
# ------------------
# Outside 24 hours since the customer's last message to us, only an approved
# template may be sent. Every stage update here is business-initiated and so
# is always a template send. Free-form replies are possible inside the window
# and are not implemented - a shipment update does not need them.
#
# COST AND QUALITY
# ----------------
# Meta charges per conversation, and a number whose messages are blocked or
# reported loses quality rating and eventually throughput. Both are reasons
# the opt-in gate in R/notify.R is a gate and not a formality.
#
# WHAT IS PROVEN HERE
# -------------------
# Payload construction and the send flow are written to the Cloud API
# specification and covered by tests. Nothing has been sent to a live
# endpoint: no WABA exists yet. ewb_api.R carries the same caveat for the
# same reason, and the boundary is kept to one function for the same reason.
# ==================================================================

META_API_VERSION <- "v21.0"

#' Credentials and ids. Never from a file in the repo.
#'
#' Two sources, environment first. The environment is how an operator who has
#' copied ids out of Business Manager configures this; the credentials file is
#' what Embedded Signup writes when they connect through Facebook instead
#' (see R/meta_signup.R). Environment wins so that a deliberately set variable
#' is never silently overridden by an old stored connection.
meta_config <- function() {
  env <- list(
    phone_number_id = Sys.getenv("TMS_WA_PHONE_NUMBER_ID", ""),
    waba_id         = Sys.getenv("TMS_WA_BUSINESS_ACCOUNT_ID", ""),
    token           = Sys.getenv("TMS_WA_ACCESS_TOKEN", "")
  )
  saved <- if (exists("meta_saved_credentials", mode = "function"))
    tryCatch(meta_saved_credentials(), error = function(e) NULL) else NULL

  pick <- function(field) {
    if (nzchar(env[[field]])) return(env[[field]])
    v <- saved[[field]] %||% ""
    if (length(v) == 0) "" else as.character(v)[1]
  }
  list(
    phone_number_id = pick("phone_number_id"),
    waba_id         = pick("waba_id"),
    token           = pick("token"),
    api_version     = Sys.getenv("TMS_WA_API_VERSION", META_API_VERSION)
  )
}

#' TRUE only when there is a number to send from and a token to sign with.
meta_configured <- function() {
  cfg <- meta_config()
  nzchar(cfg$phone_number_id) && nzchar(cfg$token)
}

#' E.164 without the plus, which is what the Cloud API wants.
#'
#' A ten-digit Indian number is assumed to be Indian and gets 91. Guessing a
#' country code is only safe because this carrier operates in one country;
#' the moment that stops being true this has to take the code explicitly.
wa_msisdn <- function(mobile) {
  v <- gsub("[^0-9]", "", as.character(mobile %||% ""))
  if (!nzchar(v)) return("")
  if (nchar(v) == 10) return(paste0("91", v))
  if (nchar(v) == 12 && substr(v, 1, 2) == "91") return(v)
  if (nchar(v) == 11 && substr(v, 1, 1) == "0") return(paste0("91", substring(v, 2)))
  v
}

#' The body of a template send.
meta_template_payload <- function(to, template_name, language, vars = character()) {
  params <- lapply(vars, function(v) list(type = "text", text = as.character(v %||% "")))
  comp <- if (length(params))
    list(list(type = "body", parameters = params)) else list()
  list(
    messaging_product = "whatsapp",
    recipient_type    = "individual",
    to                = wa_msisdn(to),
    type              = "template",
    template = list(
      name = template_name,
      language = list(code = language %||% "en"),
      components = comp
    )
  )
}

.meta_not_configured <- function() {
  list(ok = FALSE, configured = FALSE, id = "", payload = NULL,
       message = paste(
         "No WhatsApp sender configured. Set TMS_WA_PHONE_NUMBER_ID and",
         "TMS_WA_ACCESS_TOKEN, and have the templates approved in Meta",
         "Business Manager, before anything can be sent."))
}

#' The only function that talks to Meta.
#'
#' Every failure is returned rather than thrown. A messaging gateway being
#' down must never take a dispatch screen with it: the load still moved, and
#' the person on the screen has a truck to deal with.
meta_send <- function(payload, cfg = meta_config()) {
  if (!meta_configured()) { r <- .meta_not_configured(); r$payload <- payload; return(r) }
  url <- sprintf("https://graph.facebook.com/%s/%s/messages",
                 cfg$api_version, cfg$phone_number_id)
  tryCatch({
    resp <- httr::POST(
      url,
      httr::add_headers("Authorization" = paste("Bearer", cfg$token),
                        "Content-Type"  = "application/json"),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      encode = "raw", httr::timeout(30))
    txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
    body <- tryCatch(jsonlite::fromJSON(txt), error = function(e) list(raw = txt))
    okay <- httr::status_code(resp) < 400
    list(ok = okay, configured = TRUE, status = httr::status_code(resp),
         id = if (okay && !is.null(body$messages)) body$messages$id[1] else "",
         body = body, payload = payload,
         message = if (okay) "Accepted by Meta"
                   else paste("Meta rejected the message:",
                              body$error$error_user_msg %||% body$error$message %||% txt))
  }, error = function(e) {
    list(ok = FALSE, configured = TRUE, status = NA, id = "", body = NULL,
         payload = payload,
         message = paste("Could not reach Meta:", conditionMessage(e)))
  })
}
