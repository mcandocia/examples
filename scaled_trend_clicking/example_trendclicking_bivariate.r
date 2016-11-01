##This code is an example of how one could detect similarities 
##in time series data
##In particular, the goal is to be able to click points on a graph and show the top 9
##Most similar groups in terms of ratios between points (i.e., shape)
##A more robust implementation would not use R like this, as the recursive searching here 
##would use up a decent amount of memory

##used for data cleaning
require(dplyr)
##used for fast indexing
require(data.table)

##generate data
years = 2008:2013
set.seed(2016)
Letters = toupper(letters)
object_year_data = expand.grid(Letters,letters,Letters,years) %>% rowwise() %>% 
  transmute(name=paste0(Var1,Var2,Var3),year=Var4)
  
N = nrow(object_year_data)
dataset = object_year_data
##distribution is somewhat narrowly centered around 0.5, bounded by 0 and 1
dataset$tax_rate = rbeta(N, 9, 9)
dataset$growth_rate = rbeta(N, 12, 12) - 0.44
data_indexed = data.table(dataset)
setkeyv(data_indexed, c('name','year'))

company_summaries = data_indexed %>% group_by(name) %>%
  mutate(dtax = tax_rate[-1] - tax_rate[-6], 
  dgrowth = growth_rate[-1] - growth_rate[-6],
  logratio = log(abs(dgrowth/dtax))) %>% 
  dplyr::filter(year != 2013)##last year is recycled
  
##some platforms don't keep the data.table class when using summarise()
company_summaries = data.table(company_summaries)

##indexes (and sorts) all columns of data table	    
setkeyv(company_summaries, cols=colnames(company_summaries))	

cosine_similarity <- function(x_differences, dtax, dgrowth){
  return(
    (x_differences$dtax * dtax + x_differences$dgrowth * dgrowth)/
    sqrt(rowSums(x_differences^2) *( dtax^2 + dgrowth^2)))
}


##x is a 2-column matrix of points (or a data frame/table)
find_similar <- function(x, data=company_summaries, top_n = 9, raw_data = data_indexed,
			  angle_weight=1){
  x = as.data.frame(x)
  colnames(x) = c('tax_rate','growth_rate')
  x_differences = (x %>%
    dplyr::do(data.frame(dtax = .$tax_rate[-1] - .$tax_rate[-6], 
    dgrowth = .$growth_rate[-1] - .$growth_rate[-6])) %>%
    mutate(logratio = log(abs(dgrowth/dtax)))
  )
    
  similarities = company_summaries %>% group_by(name) %>% dplyr::mutate(
		cosine_similarity = cosine_similarity(x_differences[,c('dtax','dgrowth')], 
						      dtax, dgrowth),
		ratio_differences = x_differences$logratio - logratio
		)
  aggregate_similarities = similarities %>% group_by(name) %>%
    dplyr::summarise(cosine_penalty = sum((1-cosine_similarity)^2),
	      ratio_penalty = sum(abs(ratio_differences))) %>%
	mutate(final_penalty = scale(ratio_penalty) + angle_weight * scale(cosine_penalty))
	      
  cutoff_value = rev(-sort(-aggregate_similarities$final_penalty))[top_n]
  top_names= aggregate_similarities$name[aggregate_similarities$final_penalty <= cutoff_value]
  rows = data_indexed %>% dplyr::filter(name %in% top_names)
  return(rows)
}


##run this function after all of the code has been run to simulate this
try_simulation <- function(angle_weight=1){
  plot(NULL, xlim = c(0,1), ylim=c(-0.5,0.5),xlab="TAX", ylab="GROWTH",
    main='Tax Rates vs. Growth for 3-letter Companies')
  points_list = list()
  for (year in 2008:2013){
    abline(v=year,col='red',lty=2)
    pt = locator(1)
    points(pt[1],pt[2])
    points_list[[year-2007]] <- pt
    if (year > 2008)
      lines(c(pt[1],prevpt[1]),c(pt[2],prevpt[2]))
    prevpt = pt
  }
  point_matrix <<- rbindlist(points_list)
  rows = find_similar(point_matrix,angle_weight=angle_weight)
  unique_names = unique(rows$name)
  for (i in 1:9){
    color = rainbow(9)[i]
    subrows = data_indexed %>% dplyr::filter(name == unique_names[i])
    lines(subrows$tax_rate, subrows$growth_rate, col=color)#sorted
    points(subrows$tax_rate, subrows$growth_rate, col=color)
  }

  legend('topright',col=rainbow(9), legend = unique_names, lwd=3)
}


