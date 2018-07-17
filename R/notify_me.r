#' notify_me
#'
#' Send an email with a given subject and body text from R. Useful for notifying you when a long simulation has completed, or to notify participants in a study.
#' @param email_to email address to send to
#' @param email_subject subject
#' @param email_body_text body text
#' @examples 
#' notify_me(email_to = "test@test.com")

notify_me <- function(email_to, email_subject = "Notification from R", email_body_text = "Script has finished running."){
  # dependencies
  library(shiny)
  library(gmailr)
  
  # construct message
  email_body_unformatted <- mime() %>%
    to(c(email_to)) %>%
    subject(email_subject) %>%
    html_body(email_body_text)
  
  email_body_formatted <- sub("Content-Disposition: inline\r\n--",
                              "Content-Disposition: inline\r\n\r\n--", 
                              as.character(email_body_unformatted))
  
  # send message
  send_message(email_body_formatted)
}