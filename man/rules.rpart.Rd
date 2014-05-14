\name{rules.rpart}
\alias{rules.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rules.rpart(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
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
function (object) 
{
    frame <- object$frame
    ruleNums <- as.numeric(row.names(frame))
    is.leaf <- (frame$var == "<leaf>")
    frame[!is.leaf, "order"] <- seq_along(which(!is.leaf))
    rules <- replicate(max(ruleNums), NULL)
    rules[1] <- "NULL"
    for (i in as.numeric(row.names(frame))[-1]) {
        if (i\%\%2 == 0) {
            rules[i] <- paste(rules[i/2], paste("L", frame[as.character(i/2), 
                "order"], sep = ""), sep = ",")
        }
        else {
            rules[i] <- paste(rules[(i - 1)/2], paste("R", frame[as.character((i - 
                1)/2), "order"], sep = ""), sep = ",")
        }
    }
    rules <- lapply(rules, function(x) gsub("NULL,", "", x))
    return(rules)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
