
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


evreg <- function(dataset, resp='elev', n.pts=10, OLE=FALSE,
                  byRegion=NULL) {
  
  colnames(dataset)[which(colnames(dataset)==resp)] <- 'resp'
  dataset$Year <- dataset$Year-min(dataset$Year)
  
  if(is.null(byRegion)) {
    
    small.max <- plyr::ddply(dataset, c("Species",'Year'), summarise,
                             max.resp=max(resp))
    
    model1 <- lm(max.resp ~ 0 + Species+Species:Year, data=small.max)
    
    n <- length(coef(model1))/2
    sp.max <- coef(model1)[1:n][order(coef(model1)[1:n])]
    spy.max <- coef(model1)[(n+1):(2*n)][order(coef(model1)[(n+1):(2*n)])]
    names(sp.max) <- gsub(':Year','',gsub('Species','',names(sp.max)))
    names(spy.max) <- gsub(':Year','',gsub('Species','',names(spy.max)))
     
    d1 <- dotplot(sp.max, xlab='Species baseline')
    d2 <- dotplot(spy.max, xlab='Species change (units/year)')
    grid.arrange(d1, d2, ncol = 2)
    
    print(summary(model1))
    return(model1)
    
    e <- summary(model1)
    e2 <- e$coefficients[grep('Year',rownames(e$coefficients)),]
    e2 <- e2[,c(1,4)]
    
  } else {
    
  small.max <- plyr::ddply(dataset, c("Species",'Year',byRegion), summarise,
                        max.resp=max(resp))
  
  model2 <- lmer(gsub('filler',byRegion,
                  'max.resp ~ 0 + Species + Species:Year+(1|filler)'),
                  data=small.max, REML=FALSE)
  
  # compare the two models
  
  if(!(byRegion==NULL)){rand.max <- ranef(model2)}
  n <- length(fixef(model2))/2
  sp.max <- fixef(model2)[1:n][order(fixef(model2)[1:n])]
  spy.max <- fixef(model2)[(n+1):(2*n)][order(fixef(model2)[(n+1):(2*n)])]
  names(sp.max) <- gsub(':Year','',gsub('Species','',names(sp.max)))
  names(spy.max) <- gsub(':Year','',gsub('Species','',names(spy.max)))
  
  if(!(byRegion==NULL)){dotplot(rand.max)}
  d1 <- dotplot(sp.max, xlab='Species baseline')
  d2 <- dotplot(spy.max, xlab='Species change (units/year)')
  grid.arrange(d1, d2, ncol = 2)
  
  print(summary(model2))
  suppressMessages(return(model2))
  }
}
