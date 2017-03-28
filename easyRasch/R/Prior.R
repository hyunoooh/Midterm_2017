#' A function to create a prior 
#' 
#' Calculate the height of the normal curve for a proposed value of theta
#' 
#' @param theta A proposed value of beta
#' 
#' @author Hyunjoo Oh: \email{hyunjoo.oh@wustl.edu}
#' @seealso \code{\link{Probability}}, \code{\link{Likelihood}}
#' @examples 
#' 
#' Prior(theta = 1)
#' Prior(theta = 10)
#' Prior(theta = 5)
#' 
#' @aliases Prior,ANY-method
#' @rdname Prior
#' @export
setGeneric(name = "Prior",
           def = function(theta, ...){
             standardGeneric("Prior")})

#' @export
setMethod(f = "Prior",
          definition = function(theta, ...){
            prior <- dnorm(theta, mean=1, sd=3)  
            # Given a set of values dnorm() returns 
            # the height of the probability distribution at each point.
            # mean=0 and sd=3 are given.
            print(prior)
          }
)
