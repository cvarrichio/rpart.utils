#' Returns an unpivoted table of branch paths (subrules) associated with each node.
#'
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' rpart.rules.table(fit)
rpart.rules.table<-function(object)
{
  rules<-rpart.rules(object)
  ff<-object$frame
 ff$rules<-unlist(rules[as.numeric(row.names(ff))])
 # print(Reduce(cbind,lapply(ff,identity)))
  ruleList<-lapply(row.names(ff),function (name) setNames(data.frame(name,
                                                                     (strsplit(ff[name,'rules'],split=',')),
                                                                     ff[name,'yval2'][5],
                                                                     ff[name,'var']=="<leaf>"
                                                                     ),
                                                          c("Rule","Subrule","Confidence","Leaf")))
  combinedRules<-Reduce(rbind,ruleList)
  
  return(combinedRules)
  
}