headtail <- function(x, n=10, n.head=n, n.tail=n)
  rbind(head(x, n=n.head), tail(x, n=n.tail))
