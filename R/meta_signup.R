# ==================================================================
# Meta Embedded Signup — connecting the business's WhatsApp account.
#
# WHAT THIS IS, AND WHAT IT IS NOT
# --------------------------------
# This is Facebook Login used the one way it genuinely applies here: the
# BUSINESS signs in with Facebook and grants this app permission to send on
# its WhatsApp Business Account. Meta calls it Embedded Signup, and it is how
# every platform onboards a WABA without the operator hand-copying ids and
# tokens out of Business Manager.
#
# It is still not a way to reach customers. A consignee does not sign in to
# anything; they are messaged on the number written on the consignment note,
# with consent recorded. The two uses of "Facebook login" are easy to conflate
# and only one of them exists.
#
# THE FLOW
# --------
#   1. The operator clicks Connect. The Facebook JS SDK opens Meta's own
#      dialog, where they pick or create a WhatsApp Business Account and a
#      sender number. We never see their Facebook password.
#   2. The dialog returns an authorisation CODE to the browser, plus the
#      waba_id and phone_number_id it provisioned.
#   3. The code is exchanged for a business access token SERVER-SIDE, because
#      the exchange needs the app secret and a secret that reaches a browser
#      is not a secret.
#   4. The app subscribes to the WABA's webhooks and registers the number.
#
# WHAT IT NEEDS FIRST
# -------------------
# A Meta App of type Business with the WhatsApp product added, and an
# Embedded Signup configuration. From those: APP_ID, APP_SECRET, CONFIG_ID.
# The app must also list this deployment's domain, and for production use it
# needs App Review for whatsapp_business_management and
# whatsapp_business_messaging.
#
# WHERE THE TOKEN GOES
# --------------------
# Not into data/. That directory is committed and baked into the image, and a
# token in a commit is compromised the moment it is pushed. It goes to a file
# outside the repo with owner-only permissions, which meta_config() reads
# after the environment. Nothing ever renders it.
# ==================================================================

#' Where the obtained credentials live. Outside the repo, deliberately.
meta_cred_path <- function() {
  Sys.getenv("TMS_WA_CREDENTIALS", "~/.tms/wa_credentials.json")
}

#' App-level configuration — what is needed before anyone can click Connect.
meta_app_config <- function() {
  list(
    app_id      = Sys.getenv("TMS_META_APP_ID", ""),
    app_secret  = Sys.getenv("TMS_META_APP_SECRET", ""),
    config_id   = Sys.getenv("TMS_META_CONFIG_ID", ""),
    api_version = Sys.getenv("TMS_WA_API_VERSION", META_API_VERSION)
  )
}

#' TRUE when Embedded Signup can actually be offered.
#'
#' The app secret is required because the code exchange happens here, not in
#' the browser. Without it the button would open a dialog that cannot be
#' completed, which is worse than not offering it.
meta_signup_available <- function() {
  a <- meta_app_config()
  nzchar(a$app_id) && nzchar(a$app_secret) && nzchar(a$config_id)
}

#' Credentials obtained through signup, if any.
meta_saved_credentials <- function() {
  p <- path.expand(meta_cred_path())
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
}

#' Persist them with owner-only permissions.
meta_save_credentials <- function(waba_id, phone_number_id, token,
                                  display_number = "", business_name = "") {
  p <- path.expand(meta_cred_path())
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(
    waba_id = waba_id, phone_number_id = phone_number_id, token = token,
    display_number = display_number, business_name = business_name,
    connected_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ), p, auto_unbox = TRUE)
  # 0600 before anything else can read it. The default umask would leave a
  # long-lived token world-readable on a shared machine.
  Sys.chmod(p, "0600")
  invisible(TRUE)
}

#' Forget them. Used by Disconnect, and after a token is revoked at Meta.
meta_forget_credentials <- function() {
  p <- path.expand(meta_cred_path())
  if (file.exists(p)) unlink(p)
  invisible(TRUE)
}

#' Exchange the authorisation code for a business access token.
#'
#' Server-side only — the app secret must never reach a browser.
meta_exchange_code <- function(code, cfg = meta_app_config()) {
  if (!nzchar(code %||% "")) return(list(ok = FALSE, message = "No authorisation code returned."))
  if (!meta_signup_available())
    return(list(ok = FALSE, message = "Meta app is not configured on this deployment."))
  url <- sprintf("https://graph.facebook.com/%s/oauth/access_token", cfg$api_version)
  tryCatch({
    resp <- httr::GET(url, query = list(
      client_id = cfg$app_id, client_secret = cfg$app_secret, code = code),
      httr::timeout(30))
    body <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (httr::status_code(resp) >= 400 || is.null(body$access_token)) {
      return(list(ok = FALSE,
                  message = paste("Meta refused the code exchange:",
                                  body$error$message %||% "no token returned")))
    }
    list(ok = TRUE, token = body$access_token, message = "Token obtained")
  }, error = function(e)
    list(ok = FALSE, message = paste("Could not reach Meta:", conditionMessage(e))))
}

#' Subscribe this app to the WABA so delivery statuses arrive.
meta_subscribe_waba <- function(waba_id, token, cfg = meta_app_config()) {
  url <- sprintf("https://graph.facebook.com/%s/%s/subscribed_apps",
                 cfg$api_version, waba_id)
  tryCatch({
    resp <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", token)),
                       httr::timeout(30))
    list(ok = httr::status_code(resp) < 400,
         message = httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
}

#' What the Settings screen shows about the connection.
#'
#' Never the token. Only whether one is held, and what it is for.
meta_connection_summary <- function() {
  cred <- meta_saved_credentials()
  env_configured <- meta_configured()
  list(
    connected      = env_configured,
    via            = if (!is.null(cred)) "Facebook (Embedded Signup)"
                     else if (env_configured) "Environment variables" else "",
    waba_id        = cred$waba_id %||% Sys.getenv("TMS_WA_BUSINESS_ACCOUNT_ID", ""),
    phone_number_id= cred$phone_number_id %||% Sys.getenv("TMS_WA_PHONE_NUMBER_ID", ""),
    display_number = cred$display_number %||% "",
    business_name  = cred$business_name %||% "",
    connected_dt   = cred$connected_dt %||% "",
    signup_offered = meta_signup_available()
  )
}
