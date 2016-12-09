##the function checklist_split is used to take a column in a data frame and split it into
##multiple columns based on which strings appear when the text values are split by a 
##character

##this is most often used for checkbox-type data that may be returned by an application
##such as Google Forms
gather_factors_ <- function(splitted_list, na.rm=TRUE){
    if (!na.rm)
        return(unique(unlist(splitted_list)))
    else 
        return(na.omit(unique(unlist(splitted_list))))
}



checklist_split <- function(data, column, new_column_prefix='check_', sep=','){
    colname_ = deparse(substitute((column)))
    colname = substr(colname_, 2, nchar(colname_)-1)
    data[,colname] = as.character(data[,colname])
    splitted_list = strsplit(get(colname,data), sep)
    new_factors = sort(gather_factors_(splitted_list))
    for (new_factor in new_factors)
      data[,paste0(new_column_prefix, new_factor)] = 1*laply(splitted_list, 
           function(x,element=new_factor) new_factor %in% x)
    data[,colname] = NULL
    return(data) 
}

test_checkbox_parsing <-function(){
  df = data.frame(x = 1:5,
		  check=c('a,b,c','a,c,d','','e','a,b'),
		  val=rnorm(5))
		  
  return(checklist_split(df))
}
