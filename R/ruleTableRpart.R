rules.table.rpart<-function(object)
{
  rules<-rules.rpart(object)
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