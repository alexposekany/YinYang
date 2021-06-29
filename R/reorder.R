#' ran
#'
#' derives the range of a vector.
#'
#' @param x vector with unknown range
#'
#' @return the range of the vector
#'
#' @examples
#' custom_vector = c(5,2,7,9,4)
#' ran(custom_vector)
#'
#' @export
ran<-function(x){
  rangex<-range(x)
  diff<-rangex[2]-rangex[1]

  diff
}

## derive ordering of the items in the list by their range
#' ran.list
#'
#' Computes the vector of indices according to decreasing order of the ranges within the vector. This is the basis for ordering a list according to the range of vectors in the list.
#'
#' @param beta_list a two dimensional vector of which the ordered range is of interest
#'
#' @return vector of ordered ranges
#'
#' @examples
#' #ran.list(list(x=runif(100,min=1,max=2),y=runif(100,min=1,max=2)))
#' @export
ran.list<-function(beta_list){
  ran_beta<-numeric(length(beta_list))
  for(j in 1:length(beta_list)){
    ran_beta[j]<-ran(beta_list[[j]])
  }
  indexvec<-order(ran_beta,decreasing=TRUE)

  indexvec
}

## derive ordering of the items in the list by their median locations
#' med.list
#'
#' Computes the increasing order of items based on median. This is the basis for ordering a list according to the range of vectors in the list.
#'
#' @param beta_list a two dimensional vector of which the ordered median is of interest
#'
#' @return vector of increasing medians of
#'
#' @examples
#' #med.list(list(x=runif(100,min=1,max=2),y=runif(100,min=1,max=2)))
#' @export
med.list<-function(beta_list){
  med_beta<-numeric(length(beta_list))
  for(j in 1:length(beta_list)){
    med_beta[j]<-median(beta_list[[j]])
  }
  indexvec<-order(abs(med_beta-median(med_beta)),decreasing=FALSE)

  indexvec
}
