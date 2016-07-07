#' comment80
#'
#' comment80 prints to the console a comment of 80 width with the text string centered and split evently between lines
#'
#' @param textString A comment text string
#'
#' @return A comment printed to the console
#' @export
#'
#' @examples
#' #comment80(textString="This is a short comment")
#' #comment80(textString="This is a longer text string that will end up having to be split between two different lines. It will be split such that the lines are approximately equal.")
comment80 <- function(textString) {
  lentS <- nchar(textString)
  cat(c(rep("#", 80), "\n"), sep = '')
  if (lentS <= 78) {
    eachSide <- (80 - lentS) / 2
    if (paste0(c(rep('#', eachSide), textString, rep('#', eachSide), '\n'), collapse =
               '') != 81) {
      cat(c(rep('#', floor(eachSide)), textString, rep('#', ceiling(eachSide)), '\n'), sep = '')
    } else {
      cat(c(rep('#', eachSide), textString, rep('#', eachSide), '\n'), sep = '')
    }
  }
  if (lentS > 78) {
    getStrings <- function(textString) {
      i = 1
      getCut <- function(i) {
        cut <- nchar(textString) / i <= 78
        if (cut)
          return(substring(
            textString,
            seq(1,nchar(textString),nchar(textString) /
                  i),
            seq(
              nchar(textString) / i,nchar(textString), nchar(textString) / i
            )
          ))
        if (!cut) {
          i <- i + 1
          getCut(i)
        }
      }
      cutStrings <- getCut(i)
      cutWords <- stri_split_fixed(cutStrings, ' ')
      for (i in 1:(length(cutStrings) - 1)) {
        if (cutWords[[i]][length(cutWords[[i]])] != " " &
            cutWords[[i + 1]][1] != " ") {
          if (nchar(paste0(
            paste0(cutWords[[i]], collapse = " "),
            cutWords[[i + 1]][1], collapse = ''
          )) <= 78) {
            cutStrings[i] <- paste0(paste0(cutWords[[i]], collapse = " "),
                                    cutWords[[i + 1]][1], collapse = '')
            cutStrings[i + 1] <-
              paste0(cutWords[[i + 1]][-1], collapse = " ")
            cutWords[[i + 1]] <- cutWords[[i + 1]][-1]
          } else if (nchar(paste0(
            cutWords[[i]][length(cutWords[[i]])],
            paste0(cutWords[[i + 1]], collapse = " "),
            collapse = ''
          )) <= 78) {
            cutStrings[i] <-
              paste0(cutWords[[i]][-length(cutWords[[i]])], collapse = " ")
            cutStrings[i + 1] <-
              paste0(cutWords[[i]][length(cutWords[[i]])],
                     paste0(cutWords[[i + 1]], collapse = " "),
                     collapse = '')
            cutWords[[i]] <- cutWords[[i]][-length(cutWords[[i]])]
          } else {
            cutStrings[i] <-
              paste0(substring(
                paste0(cutWords[[i]], collapse = " "), 1,
                nchar(paste0(cutWords[[i]], collapse = " ")) -
                  1
              ), "-")
            cutStrings[i + 1] <-
              paste0(substring(
                paste0(cutWords[[i]], collapse = " "),
                nchar(paste0(cutWords[[i]], collapse = " ")),
                nchar(paste0(cutWords[[i]], collapse = " "))
              ),
              paste0(cutWords[[i + 1]], collapse = " "))
          }
        }
      }
      for (j in 1:length(cutStrings)) {
        eachSide <- (80 - nchar(cutStrings[j])) / 2
        if (nchar(paste0(c(
          rep('#', floor(eachSide)),
          cutStrings[j],
          rep('#', ceiling(eachSide)),
          '\n'
        ), collapse = '')) == 81) {
          cat(c(
            rep('#',floor(eachSide)), cutStrings[j], rep('#', ceiling(eachSide)), '\n'
          ), sep = '')
        } else {
          cat(c(
            rep('#', eachSide), cutStrings[j], rep('#', eachSide), '\n'
          ), sep = '')
        }
      }
    }
    getStrings(textString)
  }
  cat(rep("#", 80), sep = '')
}
