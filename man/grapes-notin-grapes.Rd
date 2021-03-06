% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guf.R
\name{\%notin\%}
\alias{\%notin\%}
\title{The inverse of \%in\%}
\usage{
x \%notin\% y
}
\arguments{
\item{x}{A vector}

\item{y}{A vector}
}
\value{
A vector of of booleans with the length of vector x. \code{TRUE} indicates that the particular element of x is not in y, \code{FALSE} indicates that the element of x is in y.
}
\description{
\code{\%notin\%} tells you which values of vector x are not in vector y.
}
\details{
Although this function can be called as \code{`\%notin\%`(x, y)}, the intended use is like the function \code{\%in\%}: similar to \code{x \%in\% y}, this function's supposed use is \code{x \%notin\% y}.
Vector types (more formally: modes) don't need to match (see last example).
}
\examples{
c(1,2,3) \%notin\% c(0,2,4) # returns TRUE FALSE TRUE
# compare to:
c(1,2,3) \%in\% c(0,2,4) # returns FALSE TRUE FALSE

c(1,2) \%notin\% c(0,2,4) # returns TRUE FALSE
# versus (vectors changed place, hence, result adapts to length of x):
c(0,2,4) \%notin\% c(1,2) # returns TRUE FALSE TRUE

c('Hello', 'world') \%notin\% unlist(strsplit('The world is not enough', ' ')) # TRUE FALSE
c('Hello', 'world') \%notin\% c(1,2,3,4,5) # returns TRUE TRUE. Vector types don't need to match.
}
