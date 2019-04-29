# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


mw.2period <- function(data, resp='elev', pre.years, post.years, n.mw=10) {
  suppressMessages((data %>% na.omit() -> data))
  pre <- data[ pre.years[1] <= data$Year & pre.years[2] >= data$Year, ]
  post <- data[ post.years[1] <= data$Year & post.years[2] >= data$Year, ]

  pre[,c(resp,'Species')] %>% as_tibble() %>%
    group_by(Species) %>% unique() %>% dplyr::top_n(n=n.mw,wt=resp) -> Pre.top

  post[,c(resp,'Species')] %>% as_tibble() %>%
    group_by(Species) %>% unique() %>% dplyr::top_n(n=n.mw,wt=resp) -> Post.top

  names <- unique(Pre.top$Species)
  result1 <- data.frame(t(data.frame(lapply(names,function(nam){
    Pre.top.i <- c(Pre.top[Pre.top$Species==nam,1])[[1]]
    Post.top.i <- c(Post.top[Post.top$Species==nam,1])[[1]]
    w <- wilcox.test(Pre.top.i,Post.top.i)
    c(mean(Post.top.i)-mean(Pre.top.i),w$p.value)}))))
  colnames(result1) <- c('Diff/year','P-value')
  rownames(result1) <- names
  result1$'Diff/year' <- result1$'Diff/year'/(mean(post.years)-mean(pre.years))

  result1$sig <- sapply(result1$'P-value', function(x) {if(x<0.05) {'*'} else {''}})
  return(result1)
  detach()
}
