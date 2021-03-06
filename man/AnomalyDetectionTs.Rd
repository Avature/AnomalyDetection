% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ts_anom_detection.R
\docType{data}
\name{AnomalyDetectionTs}
\alias{AnomalyDetectionTs}
\title{Anomaly Detection Using Seasonal Hybrid ESD Test}
\usage{
AnomalyDetectionTs(x, max_anoms = 0.1, direction = "both", alpha = 0.05,
  only_last = NULL, upper_threshold = NULL, lower_threshold = NULL, e_value = FALSE,
  longterm = FALSE, piecewise_median_period_weeks = 2, plot = FALSE,
  y_log = FALSE, xlabel = "", ylabel = "count", title = NULL,
  verbose = FALSE)
}
\arguments{
\item{x}{Time series as a two column data frame where the first column consists of the
timestamps and the second column consists of the observations.}

\item{max_anoms}{Maximum number of anomalies that S-H-ESD will detect as a percentage of the
data.}

\item{direction}{Directionality of the anomalies to be detected. Options are:
\code{'pos' | 'neg' | 'both'}.}

\item{alpha}{The level of statistical significance with which to accept or reject anomalies.}

\item{only_last}{Find and report anomalies only within the last day or hr in the time series.
\code{NULL | 'day' | 'hr'}.}

\item{upper_threshold}{Only report positive going anoms above the threshold specified.
It must be a function specifing an operation over analyzed distribution to set the value.}

\item{lower_threshold}{Only report negative going anoms below the threshold specified.
It must be a function specifing an operation over analyzed distribution to set the value.}

\item{e_value}{Add an additional column to the anoms output containing the expected value.}

\item{longterm}{Increase anom detection efficacy for time series that are greater than a month.
See Details below.}

\item{piecewise_median_period_weeks}{The piecewise median time window as described in Vallis, Hochenbaum, and Kejariwal (2014).
Defaults to 2.}

\item{plot}{A flag indicating if a plot with both the time series and the estimated anoms,
indicated by circles, should also be returned.}

\item{y_log}{Apply log scaling to the y-axis. This helps with viewing plots that have extremely
large positive anomalies relative to the rest of the data.}

\item{xlabel}{X-axis label to be added to the output plot.}

\item{ylabel}{Y-axis label to be added to the output plot.}

\item{title}{Title for the output plot.}

\item{verbose}{Enable debug messages}
}
\value{
The returned value is a list with the following components.

\item{anoms}{Data frame containing timestamps, values, and optionally expected values.}

\item{plot}{A graphical object if plotting was requested by the user. The plot contains
the estimated anomalies annotated on the input time series.}

One can save \code{anoms} to a file in the following fashion:
\code{write.csv(<return list name>[["anoms"]], file=<filename>)}

One can save \code{plot} to a file in the following fashion:
\code{ggsave(<filename>, plot=<return list name>[["plot"]])}
}
\description{
A technique for detecting anomalies in seasonal univariate time series where the input is a
series of <timestamp, count> pairs.
}
\details{
\code{longterm} This option should be set when the input time series is longer than a month.
The option enables the approach described in Vallis, Hochenbaum, and Kejariwal (2014).\cr\cr
\code{upper_threshold} Filter all anomalies whose magnitude is smaller than the output of the given function.
\code{lower_threshold} Filter all anomalies whose magnitude is greater than the output of the given function.
}
\examples{
data(raw_data)
AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
# To detect only the anomalies on the last day, run the following:
AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
}
\references{
Vallis, O., Hochenbaum, J. and Kejariwal, A., (2014) "A Novel Technique for
Long-Term Anomaly Detection in the Cloud", 6th USENIX, Philadelphia, PA.

Rosner, B., (May 1983), "Percentage Points for a Generalized ESD Many-Outlier Procedure"
, Technometrics, 25(2), pp. 165-172.
}
\seealso{
\code{\link{AnomalyDetectionVec}}
}
\keyword{datasets}
