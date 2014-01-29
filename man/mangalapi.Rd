\name{mangalapi}
\alias{mangalapi}
\title{Initialise a mangal API object}
\usage{
mangalapi(url = "http://mangal.uqar.ca", v = "v1", usr = NULL,
  key = NULL)
}
\arguments{
  \item{url}{The URL to the server}

  \item{v}{The API version}

  \item{usr}{The username}

  \item{pwd}{The API key}
}
\description{
Initialise a mangal API obect, with direct reference to the
resources
}
\details{
This function establishes a connection to the mangal API
located at the given url. If the author has an username /
API key pair, it can be stated here. Note that if this
information is in the `mangal_usr` and `mangal_key`
options, it will be filled \textbf{unless} a usr and key
are supplied.
}

