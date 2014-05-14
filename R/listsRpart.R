rpart.lists <- function(object, digits = 4, minlength = 1L, pretty,
                        collapse = TRUE, ...)
{
  if (missing(minlength) && !missing(pretty)) {
    minlength <- if (is.null(pretty)) 1L
    else if (is.logical(pretty)) {
      if (pretty) 4L else 0L
    } else 0L
  }
  
  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L) return("root")            # special case of no splits
  
  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow] # the variable names for the primary splits
  
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
  irow <- index[c(whichrow, FALSE)] # we only care about the primary split
  ncat <- object$splits[irow, 2L]
  
  lsplit <- rsplit <- list()
  #   
  #   print ("Whichrow = ")
  #   print(whichrow)
  #   print("vnames = ")
  #   print(vnames)
  #   print("index = ")
  #   print(index)
  #   print("irow = ")
  #   print(irow)
  #   print("ncat=")
  #   print(ncat)
  
  ## Now to work: first create labels for the left and right splits,
  ##  but not for leaves of course
  ##
  #   
  
  
  if (any(ncat < 2L)) {               # any continuous vars ?
    jrow <- irow[ncat < 2L]
    cutpoint <- object$splits[jrow, 4L]
    temp1 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
    lsplit[ncat<2L] <- cutpoint
    #lsplit[ncat<2L] <- lapply(lsplit[ncat<2L],function (x) structure(x, 'numeric'=TRUE))
    
    rsplit[ncat<2L] <- cutpoint
    #rsplit[ncat<2L] <- lapply(rsplit[ncat<2L],function (x) structure(x, 'numeric'=TRUE))
    
  }
  
  
  if (any(ncat > 1L)) {               # any categorical variables ?
    xlevels <- attr(object, "xlevels")
    ##
    ## jrow will be the row numbers of factors within lsplit and rsplit
    ## crow the row number in "csplit"
    ## and cindex the index on the "xlevels" list
    ##
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L] #row number in csplit
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]
    
    #     print(jrow)
    #     print(crow)
    #     print(cindex)
    
    
    
    lsplit[jrow]<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==1L])
    rsplit[jrow]<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==3L])
    
    #     lsplit<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==1L] )
    #     names(lsplit)<-names(crow)
    #     rsplit<-lapply(seq_along(jrow),function (i) xlevels[[cindex[i]]][object$csplit[crow[i], ]==3L] )
    #     names(rsplit)<-names(crow)
    
    #     combined<-unlist(Map(function(...) list(...),lsplit,rsplit),recursive=FALSE)
    
    
  }

  
  lsplit<-lapply(seq_along(lsplit), function (i) structure(lsplit[[i]], "compare"=ifelse(ncat[i]<2L,ifelse(ncat[i]<0,"<",">="),"=")))
  rsplit<-lapply(seq_along(lsplit), function (i) structure(rsplit[[i]], "compare"=ifelse(ncat[i]<2L,ifelse(ncat[i]<0,">=","<"),"=")))
  
  
  names(lsplit)<-vnames
  names(rsplit)<-vnames
  
  results<-list("L"=lsplit,"R"=rsplit)  
  
  return(results)
}