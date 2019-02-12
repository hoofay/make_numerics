
# dependencies: dplyr, lazyeval

library(dplyr)
library(lazyeval)

#' make_numerics.R
#' converts a character vector of a dataset into a scaled numeric
#'
#' @param df target dataframe
#' @param col_name column for conversion
#' @param target_name y variable used for scaling 
#' @param output 'rank' or 'value'. If rank, then scaled based on rank of target. If 'value' scaled based on the value of target variable.
#' @param convert_ind if TRUE the conversion is returned rather than the dataframe
#'
#' @return if convert_ind = FALSE then the dataframe is returned with col_name as a numeric.
#' if convert_ind = TRUE the conversion from categorical to numeric is returned, as a dataframe.
#' @export
#'
#' @examples
#' md <- data.frame(result=rnorm(8),text_var = c('d','da','d','dan','dan','da','d',NA)) 
#' make_numerics(md,'text_var','result','value') 
#' make_numerics(md,'text_var','result','rank')
make_numerics <- function(df,col_name,target_name,output = 'rank',convert_ind=FALSE){
  ### convert character / factored columns to scaled numerics
  
  mydf <- df
  
  groupL <- lapply(col_name,as.symbol)
  
  convert <- mydf %>% ungroup() %>% group_by_(.dots=groupL) %>%
    summarise_(s = lazyeval::interp(~mean(a,na.rm = TRUE), a = as.name(target_name))) %>% 
    mutate(r = (rank(s)-1)/(length(unique(mydf[,col_name]))-1)) %>% arrange(r)
  
  if(convert_ind==TRUE){
    
    return(convert)
  
    } else {
    
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
    
    return(mydf)
  }
}

