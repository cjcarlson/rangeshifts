
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
#' @import lme4
#' @import lmerTest
#' @import MuMIn
#'
#' @export
#'


# TO DO: ADD TRAILING EDGE USING NEGATIVE NUMBER IN TOP_N

evreg <- function(dataset, resp='elev', n.pts=1, OLE=FALSE,
                  byRegion=NULL, south=FALSE, latConv=FALSE,
                  units='units') {

  if(south==TRUE) {
    dataset[,resp] <- dataset[,resp] * -1
    print('Remember you turned on the latitude-reverser. + intercepts are actually -')
  }
  colnames(dataset)[which(colnames(dataset)==resp)] <- 'resp'
  dataset$Year <- dataset$Year-min(dataset$Year)

  if(is.null(byRegion)) {

    #small.max <- plyr::ddply(dataset, c("Species",'Year'), summarise,
    #                         max.resp=max(resp))

    small.max <- dataset %>% as_tibble() %>% group_by(Species,Year) %>%
                  unique() %>% dplyr::top_n(n=n.pts,wt=resp)
    small.max <- small.max[,c('Species','Year','resp')]

    model1 <- lm(resp ~ 0 + Species+Species:Year, data=small.max)

    n <- length(coef(model1))/2
    sp.max <- coef(model1)[1:n][order(coef(model1)[1:n])]
    spy.max <- coef(model1)[(n+1):(2*n)][order(coef(model1)[(n+1):(2*n)])]
    names(sp.max) <- gsub(':Year','',gsub('Species','',names(sp.max)))
    names(spy.max) <- gsub(':Year','',gsub('Species','',names(spy.max)))

    d1 <- dotplot(sp.max, xlab='Species baseline')


    if(latConv==TRUE){
      spy.max <- spy.max*111.2
      d2 <- dotplot(spy.max, xlab='Species change (kilometers/year)')
    } else {
      d2 <- dotplot(spy.max, xlab=paste('Species change (','/year)',sep=units))
    }

    grid.arrange(d1, d2, ncol = 2)

    print(summary(model1))
    #return(model1)

    e <- summary(model1)
    e2 <- e$coefficients[grep('Year',rownames(e$coefficients)),]
    e2 <- e2[,c(1,4)]
    rownames(e2) <- gsub(':Year','',gsub('Species','',rownames(e2)))

    if(latConv==TRUE){
      e2[,1] <- e2[,1] * 111.2
    }

    return(list(model1,e2))

  } else {

    colnames(dataset)[colnames(dataset)==byRegion] <- 'Region'
    small.max <- dataset %>% as_tibble() %>% group_by(Species,Year,Region) %>%
      unique() %>% dplyr::top_n(n=n.pts,wt=resp)
    small.max <- small.max[,c('Species','Year','Region','resp')]

  model2 <- lmerTest::lmer('resp ~ 0 + Species + Species:Year+(1|Region)',
                  data=small.max, REML=FALSE)
  print(MuMIn::r.squaredGLMM(model2))
  # compare the two models

  if(!(is.null(byRegion))){rand.max <- ranef(model2)}
  n <- length(fixef(model2))/2
  sp.max <- fixef(model2)[1:n][order(fixef(model2)[1:n])]
  spy.max <- fixef(model2)[(n+1):(2*n)][order(fixef(model2)[(n+1):(2*n)])]
  names(sp.max) <- gsub(':Year','',gsub('Species','',names(sp.max)))
  names(spy.max) <- gsub(':Year','',gsub('Species','',names(spy.max)))

  if(!(is.null(byRegion))){dotplot(rand.max)}
  d1 <- dotplot(sp.max, xlab='Species baseline')

  if(latConv==TRUE){
    spy.max <- spy.max*111.2
    d2 <- dotplot(spy.max, xlab='Species change (kilometers/year)')
  } else {
    d2 <- dotplot(spy.max, xlab=paste('Species change (','/year)',sep=units))
  }
  grid.arrange(d1, d2, ncol = 2)

  print(summary(model2))
  #suppressMessages(return(model2))

  e <- summary(model2)
  e2 <- e$coefficients[grep('Year',rownames(e$coefficients)),]
  e2 <- e2[,c(1,5)]
  rownames(e2) <- gsub(':Year','',gsub('Species','',rownames(e2)))

  if(latConv==TRUE){
   e2[,1] <- e2[,1] * 111.2
  }

  return(list(model2,e2))
  }
}
