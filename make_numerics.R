# author: daniel hough
# filename: make_numerics.R
# date: 2018-04-13
# dependencies: dplyr, lazyeval

library(dplyr)

make_numerics <- function(df,col_name,target_name,output = 'rank'){
  ### convert character / factored columns to scaled numerics
  
  mydf <- df
  
  groupL <- lapply(col_name,as.symbol)
  
  convert <- mydf %>% ungroup() %>% group_by_(.dots=groupL) %>%
    summarise_(s = lazyeval::interp(~mean(a,na.rm = TRUE), a = as.name(target_name))) %>% 
    mutate(r = rank(s)/length(unique(mydf[,col_name]))) %>% arrange(r)
  
  foo_name_2 <- c(col_name)
  if(output == 'rank'){
  foo_2 <- lazyeval::interp(~r)
  select_names <- c(col_name,'r')
  mydf <- mydf %>% left_join(select_(convert, .dots = select_names), by = col_name) %>%
    mutate_(.dots=setNames(list(foo_2),foo_name_2)) %>% select(-r)
  } else {
    foo_2 <- lazyeval::interp(~s)
    select_names <- c(col_name,'s')
    mydf <- mydf %>% left_join(select_(convert, .dots = select_names), by = col_name) %>%
      mutate_(.dots=setNames(list(foo_2),foo_name_2)) %>% select(-s)
  }
  
  mydf
}

# example
md <- data.frame(result=rnorm(7),text_var = c('d','da','d','dan','dan','da','d'))

make_numerics(md,'text_var','result','value')

make_numerics(md,'text_var','result','rank')

