#' Probability of answering the question correctly
#' 
#' Calculate the probability of respondent's answer is correct
#' 
#' @param raschObj An object of class Rasch
#' @param theta A proposed value of theta
#' 
#' An object, raschObj, of the class 'Rasch' contains:
#' \itemize{
#' \item \code{name} The name of the test taker 
#' \item \code{a} A numeric vector of question-item parameters on difficulty of questions
#' \item \code{y} A numeric vector of whether the respondent's answers are right(1) or wrong(0)
#' }
#' @author Hyunjoo Oh: \email{hyunjoo.oh@wustl.edu}
#' @seealso \code{\link{Likelihood}}
#' @note If the answer is correct, y=0; if the answer is incorrect, y=1. 
#' We should know all the difficulty parameters, a. We assume that there is no missing data.
#' @examples 
#' a <- as.numeric(c((sample(seq(-3, 3), size = 10, replace = TRUE))))
#' Harry <- new("Rasch", name = 'Harry', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Ron <- new("Rasch", name = 'Ron', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Hermione <- new("Rasch", name = "Hermione", a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Probability(Harry, 1)
#' Probability(Ron, 0.5)
#' Probability(Hermione, 3)
#' 
#' @aliases Probability,ANY-method
#' @rdname Probability
#' @export
setGeneric(name = "Probability",
           def = function(raschObj, theta, ...){
           standardGeneric("Probability")})

#' @export
setMethod(f = "Probability",
          definition = function(raschObj, theta, ...){
                            Prob.table <- showRasch(raschObj)
                            P <- exp(theta - raschObj@a)/(1+exp(theta - raschObj@a))
                            Q <- 1-P
                            PQ <- ifelse((raschObj@y==1), P, Q)
                            Prob.table <- rbind(Prob.table, P, PQ)
                            row.names(Prob.table) <- c("a", "y", "P", "P or Q")
                            print(Prob.table)
                            }
          )

# Test:
# Probability(Harry, 0.1)
