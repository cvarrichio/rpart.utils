\name{subrule.table.rpart}
\alias{subrule.table.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subrule.table.rpart(object)
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
    lists <- lists.rpart(object)
    leftCompares <- lapply(lists$L, function(x) attr(x, "compare"))
    rightCompares <- lapply(lists$R, function(x) attr(x, "compare"))
    leftRules <- lapply(seq_along(lists$L), function(i) setNames(data.frame(paste("L", 
        i, sep = ""), names(lists$L)[i], as.character(unlist(lists$L[i], 
        use.names = FALSE)), NA, NA), c("Subrule", "Variable", 
        "Value", "Less", "Greater")))
    rightRules <- lapply(seq_along(lists$R), function(i) setNames(data.frame(paste("R", 
        i, sep = ""), names(lists$R)[i], as.character(unlist(lists$R[i]), 
        use.names = FALSE), NA, NA), c("Subrule", "Variable", 
        "Value", "Less", "Greater")))
    reassign.columns <- function(object, compare) {
        if (grepl("<", compare)) 
            object$Less <- object$Value
        if (grepl(">", compare)) 
            object$Greater <- object$Value
        if (!grepl("=", compare)) 
            object$Value = NA
        return(object)
    }
    leftTable <- Reduce(rbind, Map(reassign.columns, leftRules, 
        leftCompares))
    rightTable <- Reduce(rbind, Map(reassign.columns, rightRules, 
        rightCompares))
    return(rbind(leftTable, rightTable))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
