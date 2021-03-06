% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notify_me.r
\name{notify_me}
\alias{notify_me}
\title{notify_me}
\usage{
notify_me(email_to, email_subject = "Notification from R",
  email_body_text = "Script has finished running.")
}
\arguments{
\item{email_to}{email address to send to}

\item{email_subject}{subject}

\item{email_body_text}{body text}
}
\description{
Send an email with a given subject and body text from R. Useful for notifying you when a long simulation has completed, or to notify participants in a study.
}
\examples{
notify_me(email_to = "test@test.com")
}
