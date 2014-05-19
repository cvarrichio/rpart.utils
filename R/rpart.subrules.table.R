#' Returns an unpivoted table of variable values (factor levels) associated with each branch.
#'
#' WITH SOURCE AS
#' (
#'     SELECT
#'     ID,
#'     TYPE,
#'     VALUE
#'     FROM DATA
#'     UNPIVOT
#'     (
#'         VALUE FOR TYPE IN (FIELD1, FIELD2, FIELD3)
#'     )UNPVT
#' ),
#' MATCHES AS 
#' (
#'     SELECT
#'     ID
#'     ,Subrule
#'     ,Variable
#'     ,SR.Value
#'     ,Less
#'     ,Greater
#'     FROM
#'     SOURCE S
#'     LEFT JOIN SUBRULES SR
#'     ON
#'     TYPE = VARIABLE
#'     AND (
#'         S.value = SR.value
#'         OR S.value < SR.Less
#'         OR S.value > SR.Greater
#'         ) 
#'     ),
#' MATCHED_SUBRULES
#' AS (
#'     SELECT
#'     Subrule
#'     ,ID
#'     FROM
#'     MATCHES M
#'     GROUP BY
#'     Subrule
#'     ,ID
#'     ),
#' MATCHED_RULES
#' AS (
#'     SELECT
#'     R.[Rule]
#'     ,MS.*
#'     FROM
#'     RULES AS R
#'     LEFT JOIN MATCHED_SUBRULES MS
#'     ON R.SUBRULE=MS.SUBRULE AND Leaf='TRUE'
#'     )
#' ,
#' COUNTS AS
#' (
#'     SELECT
#'     [RULE]
#'     ,ID
#'     ,MATCH_COUNT=COUNT(DISTINCT SUBRULE)
#'     ,NEEDED_COUNT=(SELECT COUNT(DISTINCT SUBRULE) FROM RULES R WHERE R.[RULE]=MR.[RULE])
#'     FROM
#'     MATCHED_RULES MR
#'     GROUP BY
#'     [RULE]
#'     ,ID
#'     )
#' SELECT 
#' RULE
#' ,ID
#' FROM COUNTS 
#' WHERE 
#' MATCH_COUNT=NEEDED_COUNT 
#'
#' @param object an rpart object
#' @export
#' @examples
#' library(rpart)
#' fit<-rpart(Reliability~.,data=car90)
#' rpart.subrules.table(fit)
rpart.subrules.table<-function(object)  
{
  lists<-rpart.lists(object)
  leftCompares<-lapply(lists$L,function (x) attr(x,"compare"))
  rightCompares<-lapply(lists$R,function (x) attr(x,"compare"))
  leftRules<-lapply(seq_along(lists$L),function (i) setNames(data.frame(paste('L',i,sep=''),names(lists$L)[i],as.character(unlist(lists$L[i],use.names=FALSE)),NA,NA),c("Subrule","Variable","Value","Less","Greater")))
  rightRules<-lapply(seq_along(lists$R),function (i) setNames(data.frame(paste('R',i,sep=''),names(lists$R)[i],as.character(unlist(lists$R[i]),use.names=FALSE),NA,NA),c("Subrule","Variable","Value","Less","Greater")))
  
  reassign.columns<-function(object,compare)
  {
    if(grepl("<",compare))
      object$Less<-object$Value
    if(grepl(">",compare))
      object$Greater<-object$Value
    if(!grepl("=",compare))
      object$Value=NA
    return(object)
  }
  
  leftTable<-Reduce(rbind,Map(reassign.columns, leftRules, leftCompares))
  rightTable<-Reduce(rbind,Map(reassign.columns, rightRules, rightCompares))
  
  
  return(rbind(leftTable,rightTable))
}