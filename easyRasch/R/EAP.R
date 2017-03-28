#' The expected a posteriori (EAP) value for theta
#' 
#' Calculate the expected a posteriori value for theta
#' 
#' @param raschObj An object of class Rasch
#' @param lower The lower limits of integration (default = -6)
#' @param upper The upper limits of integration (default = +6)
#' 
#' An object of the class 'Rasch', raschObj, contains:
#' \itemize{
#' \item \code{name} The name of the test taker 
#' \item \code{a} A numeric vector of question-item parameters on difficulty of questions
#' \item \code{y} A numeric vector of whether the respondent's answers are right(1) or wrong(0)
#' }
#' @author Hyunjoo Oh: \email{hyunjoo.oh@wustl.edu}
#' @note If the answer is correct, y=0; if the answer is incorrect, y=1. 
#' We should know all n of the difficulty parameters a. 
#' We assume that there is no missing data.
#' @seealso \code{\link{Probability}}, \code{\link{Likelihood}}, \code{\link{Prior}}
#' @examples 
#' a <- as.numeric(c((sample(seq(-3, 3), size = 10, replace = TRUE))))
#' Harry <- new("Rasch", name = 'Harry', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Ron <- new("Rasch", name = 'Ron', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Hermione <- new("Rasch", name = "Hermione", a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' EAP(Harry, lower=-6, upper=6)
#' EAP(Ron, lower=-3, upper=3)
#' EAP(Hermione)
#' 
#' @aliases EAP,ANY-method
#' @rdname EAP
#' @export
setGeneric(name = "EAP",
           def = function(raschObj, lower, upper,...){
             standardGeneric("EAP")})

#' @export
setMethod(f = "EAP",
          definition = function(raschObj, lower, upper,...){ 
            # we want to calculate the expected a posteriori value for theta,
            f <- function(x) Likelihood(raschObj, theta = x)*Prior(theta = x)
            eap <- integrate(f=f, lower=-6, upper=6) # default -6, +6 is given
            print(eap)
            }
          )
