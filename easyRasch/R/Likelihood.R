#' Likelihood of respondent's ability, theta 
#' 
#' Calculate the likelihood of respondent's ability to get the answer correct
#' 
#' @param raschObj An object of class Rasch
#' @param theta A proposed value of theta
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
#' @seealso \code{\link{Probability}}
#' @examples 
#' a <- as.numeric(c((sample(seq(-3, 3), size = 10, replace = TRUE))))
#' Harry <- new("Rasch", name = 'Harry', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Ron <- new("Rasch", name = 'Ron', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Hermione <- new("Rasch", name = "Hermione", a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Likelihood(Harry, theta = 1)
#' Likelihood(Ron, theta = 0.5)
#' Likelihood(Hermione, theta = 3)
#' 
#' @aliases Likelihood,ANY-method
#' @rdname Likelihood
#' @export
setGeneric(name = "Likelihood",
           def = function(raschObj, theta, ...){
             standardGeneric("Likelihood")})

#' @export
setMethod(f = "Likelihood",
          # Likelihood is the joint probability of all the observed responses given our guess of theta
          definition = function(raschObj, theta, ...){
            # the probability a test-taker get the right answer, P
            P <- exp(theta - raschObj@a)/(1+exp(theta - raschObj@a))
            # the probability a test-taker get the wrong answer, Q
            Q <- 1-P
            PQ <- ifelse((raschObj@y==1), P, Q) # if they got the correct answer, P
                                                # if they got the wrong answer, Q
            print(sum(PQ)) # Likelihood = sum of PQ
          }
)
