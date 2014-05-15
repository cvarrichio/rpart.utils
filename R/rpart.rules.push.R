#' Writes rule tables required to process rpart rules in SQL to an open RODBC connection.
#'
#'
#' @param object an rpart object
#' @param connection and open RODBC connection
#' @param prefix A character string to prefix the rules to allow for multiple rule sets
#' @export
rpart.rules.push<-function(object,connection,prefix='')
{
  require(RODBC)
  rules<-rpart.rules.table(object)
  rules$Rule<-paste(prefix,rules$Rule,sep='.')
  rules$Subrule<-paste(prefix,rules$Subrule,sep='.')
  sqlSave(connection,rules,tablename="AUTHCLASS_RULES",append=TRUE,rownames=FALSE)
  subrules<-rpart.subrules.table(object)
  subrules$Subrule<-paste(prefix,subrules$Subrule,sep='.')
  sqlSave(connection,subrules,tablename="AUTHCLASS_SUBRULES",append=TRUE,rownames=FALSE)
}