\name{mangalSearch}
\alias{mangalSearch}
\title{Search for resources}
\usage{
mangalSearch(api, type, filters)
}
\arguments{
  \item{api}{the api to connect to}

  \item{type}{the type of resource to filter}

  \item{filters}{a list of filters (see ?makefilter)}
}
\description{
Search for resources with a set of queries
}
\details{
This function will list all resources of type \code{type},
that correspond to the series of \code{filters}. Filters
are a series of lists, each with three elements: the field
on which to search, the type of search, and the target. See
\code{?makefilter} for more informations.
}

