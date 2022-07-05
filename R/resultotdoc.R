
#' Title
#'
#' @param filename type as.doc
#' @param design design is blocking technique you used in the experiment
#' @param model 0 for no test 1 for LSD 2 for Duncan 3 for HSD test
#' @param data data frame name
#' @param rep  replication factor
#' @param factA
#' @param factB
#' @param factC
#'
#' @return
#' @export
#'
#' @examples
resultdoc <- function(filename,design,model,data,rep, factA,factB,factC){
  library(doebioresearch)
  abc=data
  abcd = colnames(abc)
  d = design
  r=rep
  p=factA
  q=factB
  s=factC
  m= model
  f = as.string(filename)
  sink(f)
  for (x in abcd){
    print('####################### This is analysis of of the variable ---> ')
    print(x)
    print('#######################')
    if (d == "rcbd"){
      print(rcbd(abc[x],p,r,m))
    }
    else if (d == 'rcbd2') {
      print(frbd2fact(abc[x],r,p,q,m))
    }
    else if (d == 'rcbd3') {
      print(frbd3fact(abc[x],r,p,q,s,m))
    }
    else if (d == 'crd2') {
      print(fcrd2fact(abc[x],p,q,m))
    }
    else if (d == 'crd3') {
      print(fcrd3fact(abc[x],p,q,s,m))
    }
    else if (d == 'crd') {
      print(crd(abc[x],p,m))
    }
    else if (d == 'lsd') {
      print(lsd(abc[x],p,q,s,m))
    }
    else if (d == 'split') {
      print(splitplot(abc[x],r,p,q,m))
    }
    else if (d == 'strip') {
      print(stripplot(abc[x],r,p,q,m))
    }
  }
  sink()
}

