# ==================================================================
# Customer notifications — the decision layer.
#
# meta_api.R knows how to send. This knows whether to, to whom, with which
# template, and what to record. Those are separate because the first is a
# vendor's API and the second is this carrier's policy.
#
# THE OPT-IN GATE
# ---------------
# Nothing is sent to a number that has not consented. That is not politeness:
# under Meta's rules a business must hold opt-in obtained outside WhatsApp,
# and messages to people who did not ask get reported, which costs the
# sending number its quality rating and eventually its throughput. One
# consignee marking a message as spam degrades delivery for every customer.
#
# So consent is stored per number with when and where it was taken, and
# notify_send() refuses without it. The refusal is recorded in the outbox
# rather than silently dropped, because "why did my customer not get the
# message" is the question this table exists to answer.
#
# WHY AN OUTBOX
# -------------
# Sends are queued and recorded, never fired blindly from a screen handler.
# A dispatch clerk marking a load as gone should not wait on Meta's API, and
# should certainly not see a dispatch fail because a messaging gateway is
# down. Every attempt lands in notification_outbox with its state, so a
# message can be retried, audited, or explained.
# ==================================================================

#' Has this number consented to WhatsApp updates?
#'
#' Keyed on the number rather than the customer: a consignee is often not the
#' account holder, and it is the person holding the phone who consented.
wa_optin_state <- function(mobile) {
  n <- wa_msisdn(mobile)
  if (!nzchar(n)) return("unknown")
  o <- store_get("wa_optin")
  if (!nrow(o)) return("never_asked")
  row <- o[o$msisdn == n, , drop = FALSE]
  if (!nrow(row)) return("never_asked")
  # Latest record wins: consent can be given and withdrawn and given again,
  # and the most recent instruction is the one to obey.
  row$state[nrow(row)]
}

wa_opted_in <- function(mobile) identical(wa_optin_state(mobile), "opted_in")

#' Record consent, or its withdrawal.
#'
#' `source` says where it was taken — the consignment note, the portal, a
#' counter form. Meta expects a business to be able to show this, and a
#' stop request has to be honoured immediately and permanently.
wa_set_optin <- function(mobile, state = c("opted_in", "opted_out"),
                         source = "", client_id = "", user_id = "system") {
  state <- match.arg(state)
  n <- wa_msisdn(mobile)
  if (!nzchar(n)) return(invisible(FALSE))
  store_insert("wa_optin", list(
    msisdn = n, state = state, source = source, client_id = client_id,
    recorded_by = user_id, recorded_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  invisible(TRUE)
}

#' Queue a stage update for a consignment.
#'
#' Returns the outbox row id. Always succeeds from the caller's point of
#' view — a blocked or failed message is a recorded state, not an error
#' thrown back at whoever was marking a load dispatched.
notify_stage <- function(stage, cn_no, to_mobile, to_name, vars = character(),
                         user_id = "system") {
  tpl <- template_for(stage)
  id  <- next_id(store_get("notification_outbox")$outbox_id, "MSG-", 5)

  # Every reason, not the first one found.
  #
  # Reporting only the first means fixing it reveals the next, and whoever is
  # trying to get notifications working discovers the obstacles one release at
  # a time. Consent leads because it is the one that must never be worked
  # around; the rest are setup.
  reasons <- character()

  st <- wa_optin_state(to_mobile)
  if (identical(st, "opted_out")) {
    # Materially different from never having been asked. A stop request has
    # been given and honouring it is not optional, so it is said plainly
    # rather than folded in with "no consent on file".
    reasons <- c(reasons, "Number has opted OUT — do not message it")
  } else if (!identical(st, "opted_in")) {
    reasons <- c(reasons, "No consent on file for this number")
  }

  if (!nzchar(wa_msisdn(to_mobile))) reasons <- c(reasons, "No usable mobile number")
  if (is.null(tpl)) {
    reasons <- c(reasons, paste0("No template for stage '", stage, "'"))
  } else if (!identical(tpl$status, "Approved")) {
    reasons <- c(reasons, paste0("Template ", tpl$meta_name, " is ",
                                 tolower(tpl$status), ", not approved by Meta"))
  }
  if (!meta_configured()) reasons <- c(reasons, "No WhatsApp sender configured")

  blocked <- if (length(reasons)) paste(reasons, collapse = "; ") else NULL

  body <- if (is.null(tpl)) "" else template_fill(tpl$body, vars)

  store_insert("notification_outbox", list(
    outbox_id = id, stage = stage, cn_no = cn_no %||% "",
    msisdn = wa_msisdn(to_mobile), to_name = to_name %||% "",
    template = if (is.null(tpl)) "" else tpl$meta_name,
    body = body,
    state = if (is.null(blocked)) "queued" else "blocked",
    detail = blocked %||% "",
    queued_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    sent_dt = "", message_id = "", queued_by = user_id))
  invisible(id)
}

#' Send everything queued. Safe to call repeatedly.
#'
#' Blocked rows are left alone — they are blocked for a reason that resending
#' will not change. Clearing the reason (approving the template, recording
#' consent) and requeueing is the path back.
notify_flush <- function(limit = 50) {
  ob <- store_get("notification_outbox")
  if (!nrow(ob)) return(invisible(0L))
  due <- which(ob$state == "queued")
  if (!length(due)) return(invisible(0L))
  due <- head(due, limit)

  sent <- 0L
  for (i in due) {
    tpl <- store_get("message_templates")
    t   <- tpl[tpl$meta_name == ob$template[i], , drop = FALSE]
    lang <- if (nrow(t)) t$language[1] else "en"
    # The body was rendered at queue time; the API takes the variables, so
    # they are recovered from the stored row rather than re-derived — the
    # shipment may have moved on since, and the message must say what was
    # true when it was queued.
    vars <- notify_vars_from_body(if (nrow(t)) t$body[1] else "", ob$body[i])
    res <- meta_send(meta_template_payload(ob$msisdn[i], ob$template[i], lang, vars))
    store_update("notification_outbox", list(outbox_id = ob$outbox_id[i]),
      if (isTRUE(res$ok))
        list(state = "sent", sent_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             message_id = res$id %||% "", detail = "")
      else
        list(state = "failed", detail = substr(res$message, 1, 300)))
    if (isTRUE(res$ok)) sent <- sent + 1L
  }
  invisible(sent)
}

#' Recover the variable values by diffing a filled body against its template.
#'
#' Storing the rendered body and the variables separately would let them drift;
#' storing only the body means the values have to come back out of it. The
#' template is a fixed pattern, so the gaps between its literal parts are the
#' values.
notify_vars_from_body <- function(template_body, filled) {
  if (!nzchar(template_body %||% "") || !nzchar(filled %||% "")) return(character())
  parts <- strsplit(template_body, "\\{\\{[0-9]+\\}\\}")[[1]]
  rest <- filled; out <- character()
  for (k in seq_along(parts)) {
    lit <- parts[k]
    if (nzchar(lit)) {
      pos <- regexpr(lit, rest, fixed = TRUE)
      if (pos < 0) return(out)
      if (k > 1) out <- c(out, substr(rest, 1, pos - 1))
      rest <- substring(rest, pos + nchar(lit))
    } else if (k > 1) {
      out <- c(out, rest); rest <- ""
    }
  }
  if (nzchar(rest)) out <- c(out, rest)
  out
}

#' Everything queued or sent for one consignment, newest first.
notify_history <- function(cn_no) {
  ob <- store_get("notification_outbox")
  if (!nrow(ob)) return(ob)
  h <- ob[ob$cn_no == (cn_no %||% ""), , drop = FALSE]
  h[order(h$queued_dt, decreasing = TRUE), , drop = FALSE]
}
