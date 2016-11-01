##This code is an example of how one could detect similarities 
##between groups based on trends
##In particular, the goal is to be able to click points on a graph and show 
##the top 9 Most similar groups in terms of ratios between points (i.e., shape)
##A more robust implementation would not use R like this, 
##as the recursive searching here would use up a decent amount of memory

###NOTE: BIVARIATE METHOD NOT IMPLMENTED
##For bivariate data, slope ratios would not use the constant delta_year = 1,
##but whatever change in the variable exists. Then the two variables per 
##change in years would become angle and magnitude. Using the angle as an 
##additional constraint and the ratios of the magnitudes as they were before,
##a very similar ranking alorithm can be constructed

##used for data cleaning
require(dplyr)
##used for fast indexing
require(data.table)
##used to calculate euclidean distances between vector and matrix
require(pdist)

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
data_indexed = data.table(dataset)
setkeyv(data_indexed, c('name','year'))

company_summaries = data_indexed %>% group_by(name) %>%
  summarise(del1 = log(tax_rate[year==2009]/tax_rate[year==2008]),
	    del2 = log(tax_rate[year==2010]/tax_rate[year==2009]),
	    del3 = log(tax_rate[year==2011]/tax_rate[year==2010]),
	    del4 = log(tax_rate[year==2012]/tax_rate[year==2011]),
	    del5 = log(tax_rate[year==2013]/tax_rate[year==2012]))
	    
##some platforms don't keep the data.table class when using summarise()
company_summaries = data.table(company_summaries)

##indexes (and sorts) all columns of data table	    
setkeyv(company_summaries, cols=colnames(company_summaries))	

##calculates euclidean distances to find the top `number` points in `data` closest to `x`
get_most_similar <- function(x, data, number=9){
  deltas = log(c(x[2]/x[1],x[3]/x[2],x[4]/x[3],x[5]/x[4],x[6]/x[5]))
  dists = pdist(deltas, data %>% dplyr::select(del1,del2,del3,del4,del5))@dist 
  print(dim(data))
  return(data[order(dists)[1:number],])
}

##filters through data, recursively reducing number of results to
##avoid many distance calculations
##indexing via data table is not useful for these cuts (it actually slows them down)
##since data.table does not use indexing information for inequalities
##(and no index was built on an overly specific abs(del_i - X_i) functions)
##The code should run fairly quickly, though, so optimization here is premature
find_similar <- function(x, data=company_summaries, threshold=1.2){
  deltas = log(c(x[2]/x[1],x[3]/x[2],x[4]/x[3],x[5]/x[4],x[6]/x[5]))
  cuts = data %>% dplyr::filter(abs(del1-deltas[1]) < threshold,
				      abs(del2-deltas[2]) < threshold,
				      abs(del3-deltas[3]) < threshold,
				      abs(del4-deltas[4]) < threshold,
				      abs(del5-deltas[5]) < threshold)
  if (nrow(cuts) > 50){
    new_cuts = find_similar(x, cuts, threshold * 0.88)
    if (nrow(new_cuts) < 9)
      return(cuts)
    else
      return(new_cuts)
    }
  else
    return(cuts)
}

##returns the top 9 most similar trends
top9 <- function(x, data=company_summaries){
  return(get_most_similar(x,find_similar(x, data)))
}

##example
#x = c(0.3,0.4,0.5,0.6,0.1,0.1)
#y = top9(x)
#rows = data_indexed %>% dplyr::filter(name %in% y$name)

##run this function after all of the code has been run to simulate this
try_simulation <- function(){
  plot(NULL, xlim = c(2007,2014), ylim=c(0,1),xlab="YEAR", ylab="TAX",
    main='Plot of Tax Rates for 3-letter Companies')
  points_list = numeric(0)
  for (year in 2008:2013){
    abline(v=year,col='red',lty=2)
    pt = locator(1)
    pt[1] = year
    abline(v=year,col='white', lwd=4)
    points(pt[1],pt[2])
    points_list = c(points_list, pt$y)
    if (year > 2008)
      lines(c(pt[1],prevpt[1]),c(pt[2],prevpt[2]))
    prevpt = pt
  }

  similar = top9(points_list)
  rows = data_indexed %>% dplyr::filter(name %in% similar$name)

  for (i in 1:9){
    color = rainbow(9)[i]
    subrows = rows %>% dplyr::filter(name == similar$name[i])
    lines(2008:2013, subrows$tax_rate, col=color)#sorted
  }

  legend('topright',col=rainbow(9), legend = similar$name, lwd=3)
}


