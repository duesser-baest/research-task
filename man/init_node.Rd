% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{init_node}
\alias{init_node}
\title{Initializes new node for model}
\usage{
init_node(X, y, name, generation, parent, parent_split, loss)
}
\arguments{
\item{X}{independent data}

\item{y}{dependent variable}

\item{name}{name stores heritage}

\item{generation}{generation (kept to track max_tree_depth)}

\item{parent}{parent}

\item{parent_split}{parent split}

\item{loss}{loss of node}
}
\value{
node
}
\description{
Initializes new node for model
}
