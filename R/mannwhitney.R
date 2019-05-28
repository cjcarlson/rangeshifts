
#' @title Mann-Whitney resurvey test for range shifts
#'
#' @description
#' Use before-and-after comparisons to test for range shifts. Takes the top n unique points from specified variable (elevation, latitude, etc.) and does an unpaired Mann-Whitney test. Returns absolute difference in means (not scaled to time period) and the p-value from the MW test.
#'
#' @param dataset A dataset frame containing columns "Year" and "Species" at a minimum as well as the response variable.
#' @param resp A variable within the dataset that is being tracked, such as elevation, or latitude. (If latitude is used, just note to convert it back to km manually, it's not currently automatic in here.)
#' @param pre.years An interval entered as "c(year1,year2)"
#' @param post.years An interval entered as "c(year1,year2)"
#' @param n.mw The number of unique points to be used in the Mann-Whitney comparison. It defauls to 10. Remember that if you use negative you can do southern or lower elevations easy
#'
#'
#' @export
#'


mw.2period <- function(dataset, resp='elev', pre.years, post.years, n.mw=10) {
  suppressMessages((dataset %>% na.omit() -> dataset))
  pre <- dataset[ pre.years[1] <= dataset$Year & pre.years[2] >= dataset$Year, ]
  post <- dataset[ post.years[1] <= dataset$Year & post.years[2] >= dataset$Year, ]

  colnames(pre)[colnames(pre)==resp] <- 'response'
  pre[,c('response','Species')] %>% as_tibble() %>%
    group_by(Species) %>% unique() %>% dplyr::top_n(n=n.mw,wt=response) -> Pre.top

  colnames(post)[colnames(post)==resp] <- 'response'
  post[,c('response','Species')] %>% as_tibble() %>%
    group_by(Species) %>% unique() %>% dplyr::top_n(n=n.mw,wt=response) -> Post.top

  names <- unique(Pre.top$Species)
  result1 <- data.frame(t(data.frame(lapply(names,function(nam){
    Pre.top.i <- c(Pre.top[Pre.top$Species==nam,1])[[1]]
    Post.top.i <- c(Post.top[Post.top$Species==nam,1])[[1]]
    if(length(Pre.top.i)==10) {
      if(length(Post.top.i)==10) {
        suppressWarnings(w <- wilcox.test(Pre.top.i,Post.top.i))
        c(mean(Post.top.i)-mean(Pre.top.i),w$p.value)
      } else c(NA,NA)
    } else c(NA,NA)
  }))))

  if(nrow(na.omit(result1))==0){
    return("No species with enough data")
  } else {

  colnames(result1) <- c('Diff/year','P-value')
  rownames(result1) <- names
  result1$'Diff/year' <- result1$'Diff/year'/(mean(post.years)-mean(pre.years))

  if(length(rownames(result1[is.na(result1[,2]),]))>0){
  print("Not enough data for")
  print(rownames(result1[is.na(result1[,2]),]))
  }

  result1 <- na.omit(result1)
  result1$sig <- sapply(result1$'P-value', function(x) {if(x<0.05) {'*'} else {'n.s.'}})

  summ <- table(sign(result1$`Diff/year`),result1$sig)
  rownames(summ) <- gsub('-1','-',rownames(summ))
  rownames(summ) <- gsub('1','+',rownames(summ))
  print(summ)
  return(result1)
  detach()
  }
}

