\name{push.rules.rpart}
\alias{push.rules.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
push.rules.rpart(object, prefix = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{prefix}{
%%     ~~Describe \code{prefix} here~~
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
function (object, prefix = "") 
{
    require(RODBC)
    rules <- rules.table.rpart(crs$rpart)
    rules$Rule <- paste(prefix, rules$Rule, sep = ".")
    rules$Subrule <- paste(prefix, rules$Subrule, sep = ".")
    sqlSave(xj, rules, tablename = "AUTHCLASS_RULES", append = TRUE, 
        rownames = FALSE)
    subrules <- subrule.table.rpart(crs$rpart)
    subrules$Subrule <- paste(prefix, subrules$Subrule, sep = ".")
    sqlSave(xj, subrules, tablename = "AUTHCLASS_SUBRULES", append = TRUE, 
        rownames = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
