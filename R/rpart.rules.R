#' Returns a list of strings summarizing the branch path to each node.
#'
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' rpart.rules(fit)
rpart.rules<-function(object)
{
  frame<-object$frame
  ruleNums<-as.numeric(row.names(frame))
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf,"order"]<-seq_along(which(!is.leaf))
  rules<-replicate(max(ruleNums),NULL)
  rules[1]<-"NULL"
  for (i in as.numeric(row.names(frame))[-1])
  {
    if(i%%2==0)
    {
      rules[i]<-paste(rules[i/2],paste('L',frame[as.character(i/2),"order"],sep=''),sep=',')
    }
    else
    {
        rules[i]<-paste(rules[(i-1)/2],paste('R',frame[as.character((i-1)/2),"order"],sep=''),sep=',')
    }
  }
  rules<-lapply(rules,function (x) gsub("NULL,",'',x))
  return(rules)
}

