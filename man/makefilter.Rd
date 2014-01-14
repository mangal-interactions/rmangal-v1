\name{makefilter}
\alias{makefilter}
\title{Construct a filtering query}
\usage{
makefilter(api, type, filter)
}
\arguments{
  \item{api}{the api to connect to}

  \item{type}{the type of resource to filter}

  \item{filter}{a list with objects field, relation,
  target}
}
\description{
Returns a string formatted as a filtering query
}
\details{
This functions takes a list and returns a formatted
filtering query. It is used internally by
\code{mangalSearch}. The next paragraphs detail the role of
each element of the \code{filter} list.

\code{field} is the name of the field on which to search. A
list of fields is available with \code{whatIs}.

\code{relation} is a string, giving the type of search to
perform. It can takes the values \code{startswith},
\code{endswith}, \code{contains}, \code{exact}, \code{in}
(check if \code{target} is one of the values of for
multi-values fields), \code{gte}, \code{lte}, \code{gt},
\code{lt}, and finally \code{range}.

\code{target} is the value to search on. If \code{relation}
is \code{range}, it must be given as a vector of two values
}

