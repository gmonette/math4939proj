#' 
#' # This is a heading
#' 
#' Note that headings are created with 'number signs' 
#' and the level of the heading is indicated with the 
#' number of 'number signs'
#' 
#' ## This is a subheading
#' 
#' This is a bullet list:  
#'   - item number 1   
#'   - item number 2  
#'
#' and this is a numbered list:  
#' 
#'   1. first time  
#'   1. second item (it doesn't matter what 
#'      number you use in the source code.)    
#'      1. a sub list with 2 additional spaces of indentation   #'      3. second sub list item  
#'   1. back to the main list  
#' and now for an 'r chunk'
head(cars)
dim(cars)
# this is an R comment, not a heading
plot(dist ~ speed, cars)
#' and we are back to rmarkdown code after the 'r chunk'.
#' 
#' 
#' ## More examples
#'
#' Here's a link to [MATH 4039](http://capstone.stats.yorku.ca)
#'
#' Here's an internal link to the ["This is a subheading" section](#this-is-a-subheading)
#'
#' Here's a bit of LaTeX $\int_0^\infty  {\frac{1}{{\sqrt {2\pi } }}{{\exp }^{ - {x^2}/2}}dx}$
#' 
#' You can do just about anything with [rmarkdown](http://rmarkdown.rstudio.com/)
#' 