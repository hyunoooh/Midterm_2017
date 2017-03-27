#' An object of a student who has completed an exam
#' 
#' Objects of class \code{Rasch} are created. 
#' 
#' 
#' An object of the class 'Rasch' has the following slots:
#' \itemize{
#' \item \code{name} The name of the test taker 
#' \item \code{a} A numeric vector of question-item parameters on difficulty of questions
#' \item \code{y} A numeric vector of whether the respondent's answers are right(1) or wrong(0)
#' }
#' 
#' @author Hyunjoo Oh: \email{hyunjoo.oh@wustl.edu}
#' @note We assume that we know the difficulty parameters, a; 
#' we have observed the answers of a respondent, y
#' @aliases Rasch-class initialize,Rasch-method showRasch,Rasch-method
#' @rdname Rasch
#' @export
setClass(Class = "Rasch", 
         # Define the slots
         representation = representation(name = 'character',
                                a = 'numeric',
                                y = 'numeric'),
         # Set the default values for the slots
         prototype = prototype(list(
                      name = c(),
                      a = c(),
                      y = c()))
         )

setMethod("initialize", "Rasch",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

## Make a function: validation method to test the object is structured correctly
# This function should have a single argument called object
# and should return TRUE if the object is valid.
# If not, it should return a character vector giving the reasons why it is not valid.
setValidity(Class = "Rasch",
            function(object){
              errors <- character() # create errors
              length.a <- length(object@a) # the length of "a"
              length.y <- length(object@y) # the length of "y"
              # the length of "a" and "y" should be the same
              if (length.a != length.y){
                msg <- paste("The length of a is ", length.a, 
                             ". And the length of y is ", length.y, 
                             ". They should be the same.", sep = '')
                errors <- c(errors, msg) # If the length of "a" and "y" is not equal, 
                # the error message appears
              }
              length.name <- length(object@name) # checking the length of "name" of test-taker
              if (length.name !=1){
                msg <- paste("The length of name is ", length.name, ". This sould be 1", sep = '')
                errors <- c(errors, msg) # If the length of "name" is not 1, the error message appears
              }
              # checking if the class "name" is character
              if (class(object@name) !="character"){
                msg <- paste("The class of name is ", class(object@name), ". This sould be character.", sep = '')
                errors <- c(errors, msg) # If the length of "name" is not 1, the error message appears
              }
              value.y <- object@y # checking the value of "y" which should be either 0 (wrong) or 1 (right)
              if (abs(max(value.y) - min(value.y)) > 1){
                msg <- paste("y includes the value which is neither 0 or 1. It should be either 0 or 1", sep = '')
                errors <- c(errors, msg) # If the value of "y" is neither 0 or 1, the error message appears
              }
              if (length(errors)==0) TRUE else errors
            }
)

# Test:
# new("Rasch")
# new("Rasch", name='Harry', a=c(-2, 0, 3, 1, 1), y=c(1, 0, 1, 0, 0))
# new("Rasch", name=2, a=c(-2, 0, 3, 1, 1), y=c(1, 0, 1, 0, 0))
# new("Rasch", name='Harry', a=c(-2, 0, 3, 1, 1), y=c(1, 0, 2))


#' @export
setGeneric(name = "showRasch", # we are going to define a new method, "showRasch"
          function(object = "Rasch"){
             standardGeneric("showRasch")
           }
)

setMethod("showRasch", "Rasch", # by using "showRasch" we will make a matrix of 
          function(object){
            raschObj <- rbind(object@a, object@y) # first row: difficulty parameter, a
                                                  # second row: whether object's answers are right
            rownames(raschObj) <- c('a', object@name) # adding row names
            return(raschObj) # matrix is created
          }
)

# Test:
# raschObj <- new("Rasch", name="Ron", a=c(0, 1, -1, -3, 2), y=c(1, 1, 1, 0, 0))
# showRasch(raschObj)
