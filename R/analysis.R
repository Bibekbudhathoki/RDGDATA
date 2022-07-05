# Analysis
#
# This is an example function named 'Analysis'
# which prints 'For biological research'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' Prints result of the analysis data
#' For simple use always install "Doebioresearch" package using install.package('doebioresearch') after installing this package, Do analysis by just putting data in the recommemnded format.Code is as follos analysis(design, model, data, replication, factA, factB, factC), for design use ,  rcbd -->  for randomized complete block  design , rcbd2 --> for 2 factorial  rcbd , rcbd3 --> for 3 factorial rcbd, crd -- > for complete randmized design crd2 --> for 2 factorial crd, crd3 --> for 3 factorial crd, lsd --> for latin square design ,split -- > for split plot design, strip --> for strip plot design , for model  ,0 for no test ,1 for LSD test 2 for Duncan test,3 for HSD test for Data put Dataframe name ( as seen in global environment)for replication , Block for split plot, if block is not avalable put 0, for Factor A ,Put Treatment name incase of LSD , RCBD and CRD , factor B,Put Second factor Name otherwise put 0 if its absent .factor C,Put third factor Name otherwise put 0 if its absent,
#' @export

analysis <- function(design,model,data,rep, factA,factB,factC){
    library(doebioresearch)
    abc=data
    abcd = colnames(abc)
    d = design
    r=rep
    p=factA
    q=factB
    s=factC
    m= model
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
  }

