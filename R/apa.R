#' APA formating number
#'
#' convert a numerical result to an apa norme restul
#' @param x a numric
#' @param oto a true of false statement (false by default) to indicate if the value x  of the nurmric value provided would matematicly constrict between -one to +one a numric
#' @param p a true of false  statement (false by default) to indicate if the value x is a p-value
#' @param d the number of digits needed
#' @return a formated number for apa normed document
#' @examples
#' apa.n(34.036, d=2) ;
#' apa.n(34.036, d=3)
#' apa.n(34.036, d=4)
#' apa.n(0.001, d=1)
#' apa.n(0.001, d=2)
#' apa.n(0.001, d=3)
#' apa.n(0.001, d=2,p=T)
#' apa.n(0.001, d=3,p=T)
#' apa.n(-0.10, d=2,oto=T)
#' apa.n(-0.10, d=3,oto=T)
#' @export




apa.n <- function(x, d = 2, oto = F, p = F) {
  res <- round(x,digits=d)

  res <- format(res, nsmall = d, digit=d,scientific=F,trim=T)

  if (oto == T | p == T){
    res <- strsplit(res,".",fixed = T)
    res[[1]][1] <- substr(res[[1]][1], 0, nchar(res[[1]][1])-1)
    res <- paste(res[[1]],collapse = ".")

    if (res == ".000") res <- ".001"
    return(res)
  }else{
    return(res)
  }
}




#' APA formating number 2
#'
#' @param n numeric value
#' @importFrom stringr str_count
#' @importFrom stringr str_sub
#'
#' @export
apa.p <- function(n, pval, d = 3){
  rez <- c()
  for(i in seq_along(n)){
    # cat(n[i],pval[i])
    p="   "
    if(pval[i] < 0.10) p ="†  "
    if(pval[i] < 0.05) p ="*  "
    if(pval[i] < 0.01) p ="** "
    if(pval[i] < 0.001) p ="***"
    tmp <- paste0(apa.n(n[i],d = d),p)
    rez <- c(rez, tmp)
  }
  t = min(str_count(rez," "))
  rez <- str_sub(rez, end = -(t+1))
  return(rez)
}


