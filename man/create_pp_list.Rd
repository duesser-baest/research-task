% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_pp_list}
\alias{create_pp_list}
\title{Helper function used in both building models and predicting on them. The result is passed within the model. The output maps the order of factor input features depending on y to their internally used number}
\usage{
create_pp_list(X, y)
}
\arguments{
\item{X}{independent variables}

\item{y}{dependent variable vector}
}
\value{
preprocessing list
}
\description{
Helper function used in both building models and predicting on them. The result is passed within the model. The output maps the order of factor input features depending on y to their internally used number
}
