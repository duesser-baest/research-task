% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{sl_df}
\alias{sl_df}
\title{Creates a data frame storing name split and loss in a node}
\usage{
sl_df(node, min_leaf_size, measure, restr_la_var)
}
\arguments{
\item{node}{node}

\item{min_leaf_size}{minimal allowed leaf size}

\item{measure}{measure}

\item{restr_la_var}{whether or not the dataframe should be restricted to carry only one split per variable}
}
\value{
df with name, value and loss
}
\description{
Creates a data frame storing name split and loss in a node
}
