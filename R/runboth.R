
#' @title Extreme value regression tests for range shifts
#'
#' @description
#' Approach used in Carlson 2019 bioRxiv forthcoming. Can be done with maximum point per year, maximum n points, or the OLE ('distance-to-edge' model).
#'
#' @param dataset A dataset frame containing columns "Year" and "Species" at a minimum as well as the response variable.
#' @param resp A variable within the dataset that is being tracked, such as elevation, or latitude. (If latitude is used, just note to convert it back to km manually, it's not currently automatic in here.)
#' @param n.pts Number of top-per-year points to use. Haven't added this functionality yet
#' @param OLE Distance-to-edge model, haven't added yet
#' @param byRegion The column to use as a random spatial effect to handle spatial heterogeneity (multiple gradients)
#' 
#' @import lmer
#' 
#' @export
#' 


run.all <- function(dataset, resp='elev', n.pts=10, OLE=FALSE,
                    byRegion=NULL) {
  
  tbl1 <- data.frame(evreg(dataset, resp, n.pts, byRegion=NULL)[[2]])
  tbl2 <- mw.2period(dataset, resp, 
                      pre.years=c(1900,1950), post.years=c(1970,2020), 
                      n.mw=n.pts)
  
  tbl2 <- tbl2[order(row.names(tbl2)), ]
  tbl.all <- dplyr::left_join(mutate(tbl1, Species=rownames(tbl1)),
                   mutate(tbl2, Species=rownames(tbl2)), 
                   by='Species')[,c(3,4,5,1,2)]
  tbl.all[,c(2,4)] <- round(tbl.all[,c(2,4)],2)
  tbl.all[,c(3,5)] <- round(tbl.all[,c(3,5)],3)
  colnames(tbl.all) <- c('Species','MWBeta','P','RegBeta','P')
  return(tbl.all)
}
