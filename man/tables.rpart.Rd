\name{tables.rpart}
\alias{tables.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tables.rpart(object, digits = 4, minlength = 1L, pretty, collapse = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{minlength}{
%%     ~~Describe \code{minlength} here~~
}
  \item{pretty}{
%%     ~~Describe \code{pretty} here~~
}
  \item{collapse}{
%%     ~~Describe \code{collapse} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, digits = 4, minlength = 1L, pretty, collapse = TRUE, 
    ...) 
{
    if (missing(minlength) && !missing(pretty)) {
        minlength <- if (is.null(pretty)) 
            1L
        else if (is.logical(pretty)) {
            if (pretty) 
                4L
            else 0L
        }
        else 0L
    }
    ff <- object$frame
    n <- nrow(ff)
    if (n == 1L) 
        return("root")
    is.leaf <- (ff$var == "<leaf>")
    whichrow <- !is.leaf
    vnames <- ff$var[whichrow]
    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + (!is.leaf)))
    irow <- index[c(whichrow, FALSE)]
    ncat <- object$splits[irow, 2L]
    lsplit <- rsplit <- list()
    if (any(ncat < 2L)) {
        jrow <- irow[ncat < 2L]
        cutpoint <- object$splits[jrow, 4L]
        temp1 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
        temp2 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
        lsplit[ncat < 2L] <- paste0(temp1, cutpoint)
        rsplit[ncat < 2L] <- paste0(temp2, cutpoint)
    }
    if (any(ncat > 1L)) {
        xlevels <- attr(object, "xlevels")
        jrow <- seq_along(ncat)[ncat > 1L]
        crow <- object$splits[irow[ncat > 1L], 4L]
        cindex <- (match(vnames, names(xlevels)))[ncat > 1L]
        print(jrow)
        print(crow)
        print(cindex)
        lsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i], 
            ] == 1L])
        rsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i], 
            ] == 3L])
    }
    names(lsplit) <- vnames
    names(rsplit) <- vnames
    results <- list(left = lsplit, right = rsplit)
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
