# https://www.hackerrank.com/challenges/30-dictionaries-and-maps

l <- readLines(file("stdin"), warn = FALSE)
n <- as.integer(l[[1]])
spl <- strsplit(l[2:(n+1)], " ")

mylist <- lapply(spl, function (x) x[[2]])
names(mylist) <- lapply(spl, function (x) x[[1]])

for (i in (n+2):length(l))
{
  pn <- mylist[[l[[i]]]]
  if(is.null(pn))
  {
    cat('Not found\n')
  }
  else
  {
    cat (l[[i]])
    cat("=")
    cat (pn)
    cat("\n")
  }
}