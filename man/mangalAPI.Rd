\name{mangalAPI}
\alias{mangalAPI}
\title{Handles connection to the database}
\usage{
  mangalAPI(api = "http://localhost:8000/api", user = NULL,
    pwd = NULL)
}
\arguments{
  \item{api}{URL of the API -- default to the UQAR API}

  \item{user}{Username on the API server}

  \item{pwd}{Password}
}
\description{
  This function returns an object of class
  \code{mangalAPI}, that has the URL of the API and
  possibly the username / password informations.
}

