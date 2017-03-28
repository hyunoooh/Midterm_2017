#' Plotting for the Rasch class
#' 
#' Create a plot for the Rasch class
#' 
#' @param raschObj An object of class Rasch
#' @param theta A proposed value of theta
#' 
#' An object of the class 'Rasch' contains:
#' \itemize{
#' \item \code{name} The name of the test taker 
#' \item \code{a} A numeric vector of question-item parameters on difficulty of questions
#' \item \code{y} A numeric vector of whether the respondent's answers are right(1) or wrong(0)
#' }
#' @author Hyunjoo Oh: \email{hyunjoo.oh@wustl.edu}
#' @seealso \code{\link{Probability}}, \code{\link{Likelihood}}, \code{\link{Prior}}
#' @note If the answer is correct, y=0; if the answer is incorrect, y=1. 
#' We should know all n of the difficulty parameters a. We assume that there is no missing data.
#' @examples 
#' a <- as.numeric(c((sample(seq(-3, 3), size = 10, replace = TRUE))))
#' Harry <- new("Rasch", name = 'Harry', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Ron <- new("Rasch", name = 'Ron', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Hermione <- new("Rasch", name = 'Hermione', a = a, y = as.numeric(c(sample(c(0,1), size=10, replace = TRUE))))
#' Plot(Harry, theta = 1)
#' Plot(Ron, theta = 0.5)
#' Plot(Hermione, theta = 3)
#' 
#' @aliases Plot,ANY-method
#' @rdname Plot
#' @export
setGeneric(name = "Plot",
           def = function(raschObj, ...){
             standardGeneric("Plot")})

#' @export
setMethod(f = "Plot",
          definition = function(raschObj, theta,...){
            output <- Probability(raschObj = raschObj, theta = theta)
            for(i in 1:ncol(output)) plot(output[2,i], xlim = c(theta-1, theta+1),
                                          ylab = "Probability of answering the question correctly",
                                          xlab = "possible theta",
                                          main = paste("Probability of answering the question correctly: ", raschObj@name, sep = ""))
          }
)

