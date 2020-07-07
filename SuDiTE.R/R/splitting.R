#' Random proportional train-holdout splitting of a dataset
#'
#' Takes a dataset and randomly assign observations to either train or holdout sets in a given proportion
#'
#' @param opts$TestPropotion defines the relative size of the test set w.r.t. the whole dataset.
#' @param opts$TestSize defines the absolute size of the test set
#' @param dbY is Y component of the dataset
#' @param dbTrt is Trt component of the dataset
#' @param dbX is X component of the dataset
#'
#' @return A list with indices of train and holdout set.
#' @export
#'
#' @examples
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
#' # Computing the train-holdout split
#' split = randomSplit(list(TestProportion=0.2), Y, Trt, X)
#' summary(split)
#' summary(X[split$Train,])
#' summary(X[split$Holdout,])
randomSplit = function(opts, dbY, dbTrt, dbX) {
  stopifnot(!is.na(opts$TestProportion))
  stopifnot(0 < opts$TestProportion && opts$TestProportion < 1)

  indsHoldout = sample(length(dbY),opts$TestProportion*length(dbY))
  indsTrain = (1:length(dbY))[-indsHoldout]
  return (list(Train=indsTrain,Holdout=indsHoldout))
}

#' Balansed proportional train-holdout splitting of a dataset
#'
#' The function realizes the procedure described in
#' Lipkovich I. et al. Subgroup identification based on differential effect search -- A recursive partitioning method for establishing response to treatment in patient subpopulations // Stat. Med. John Wiley & Sons, Ltd, 2011. Vol. 30, № 21. P. 2601–2621.
#'
#' @param opts$InitDataPropotion defines proportion of a dataset that is randomly assigned to train and holdout sets
#' @param opts$InitSplitPropotion defines proportion of initial split into train and holdout sets
#' @param opts$Deterministic is a logical flag defining if balancing assigniments should be deterministic
#' @param dbY is Y component of the dataset
#' @param dbTrt is Trt component of the dataset
#' @param dbX is X component of the dataset
#'
#' @return A list with indices of train and holdout set.
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
#' # Computing the train-holdout split
#' split = balansedSplit(list(InitSplitProportion=0.2), Y, Trt, X)
#' summary(split)
#' summary(X[split$Train,])
#' summary(X[split$Holdout,])
balansedSplit = function(opts, dbY, dbTrt, dbX) {
  if(is.null(opts$InitDataProportion)) {
    opts$InitDataProportion=0.5
  }
  if(is.null(opts$Deterministic) ) {
    opts$Deterministic=TRUE
  }
  stopifnot(!is.null(opts$InitSplitProportion))
  stopifnot(0 < opts$InitSplitProportion && opts$InitSplitProportion < 1)
  stopifnot(0 < opts$InitDataProportion && opts$InitDataProportion < 1)

  result = list(Train=NULL,Holdout=NULL)

  initInds = sample(length(dbY),opts$InitDataProportion * length(dbY))
  initSizeHoldout = length(initInds) * opts$InitSplitProportion
  result$Holdout = c(result$Holdout, initInds[1:initSizeHoldout])
  result$Train = c(result$Train, initInds[-(1:initSizeHoldout)])

  balanceVars = cbind(dbTrt,dbX)
  balanceInds = (1:length(dbY))[-initInds]
  balanceInds=sample(balanceInds)
  for( i in balanceInds ) {
   trainProb = balancedTrainProb(i, result, balanceVars)
    if(trainProb == 0.5) {
      # Always random assignment
      if(rbinom(1,1,opts$InitSplitProportion) == 1) {
        result$Holdout=c(result$Holdout, i)
      } else {
        result$Train=c(result$Train, i)
      }
    } else if(opts$Deterministic) {
      if(trainProb < 0.5) {
        result$Holdout=c(result$Holdout, i)
      } else {
        result$Train=c(result$Train, i)
      }
    } else {
      if( rbinom(1,size=1,prob=trainProb) == 0 ) {
        result$Holdout=c(result$Holdout, i)
      } else {
        result$Train=c(result$Train, i)
      }
    }
  }

  return( result )
}

#' Balanced probablity that an observation should be assigned to a certain set
#'
#' @param i is an index of the added object in X
#' @param split is the indices of observations from Train and Holdout sets
#' @param X the covariates
#'
#' @return A balansed probablity that the observation i should be in the train dataset
balancedTrainProb = function(i, split, X) {
  trainPosEff=0
  holdoutPosEff=0

  for(col in X) {
    # unsupported column type
    stopifnot(is.factor(col) || is.numeric(col) || is.logical(col))

    tr=NULL
    ho=NULL
    iVal=NULL
    if(!is.factor(col)) {
      tr=col[split$Train]
      ho=col[split$Holdout]
      iVal=col[i]
    } else {
      tr=col[split$Train] == col[i]
      ho=col[split$Holdout] == col[i]
      iVal=1
    }
    mean.tr=mean(tr)
    mean.ho=mean(ho)
    if((iVal - mean.tr)*(iVal-mean.ho) < 0) {
      # any group is suitable
      next
    }
    p.val=t.test(tr,ho)$p.value
    if(is.nan(p.val)){
      p.val=1
    }
    signifLevel = 0.05
    # If p.val is small, the rate is high, i.e., the correction is needed
    #  if p.val is insignificant then the covariate is balanced
    rate=1-min(signifLevel,p.val)/signifLevel

    if((iVal-mean.tr)*(mean.tr-mean.ho) > 0 ) {
      # iVal > mean.tr >mean.ho
      # OR
      # iVal < mean.tr < mean.ho
      # i.e., iVal should be in hold out
      holdoutPosEff = holdoutPosEff + rate
    } else {
      # iVal should be in train
      trainPosEff = trainPosEff + rate
    }
  }
  resProb=NULL
  if(trainPosEff + holdoutPosEff <= 0) {
    resProb=0.5
  } else {
    resProb=trainPosEff/(trainPosEff+holdoutPosEff)
  }
  stopifnot(0 <= resProb && resProb <= 1)
  return(resProb)
}








#' Random balanced proportional train-holdout splitting of a dataset
#'
#' Takes a dataset and randomly assign observations to either train or holdout sets in a given proportion and providing equal shares of treated (Trt = 1) and control observations (Trt = 0) both in train and holdout sets
#'
#' @param opts$TestPropotion defines the relative size of the test set
#' @param dbY is Y component of the dataset
#' @param dbTrt is Trt component of the dataset
#' @param dbX is X component of the dataset
#'
#' @return A list with indices of train and holdout set.
#' @export
#'
#' @examples
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
#' # Computing the train-holdout split
#' split = randomSplit_equal(list(TestProportion=0.2), Y, Trt, X)
randomSplit_equal = function(opts, dbY, dbTrt, dbX) {
  stopifnot(!is.na(opts$TestProportion))
  stopifnot(0 < opts$TestProportion && opts$TestProportion < 1)

  p1=sum(dbTrt)
  p0=NROW(dbTrt)-p1
  num=min(p1,p0)

  Treatment = which(dbTrt == 1)
  Non_Treatment = which(dbTrt == 0)


  Treat = sample(Treatment, round((1-opts$TestProportion)*num, 0))
  Non_Treat = sample(Non_Treatment, round((1-opts$TestProportion)*num, 0))
  indsTrain = c(Treat, Non_Treat)


  Treat_Hol = Treatment[-Treat]
  Non_Treat_Hol = Non_Treatment[-Non_Treat]
  #num1 = min(length(Treat_Hol), length(Non_Treat_Hol))

  indsHoldout = c(Treat_Hol, Non_Treat_Hol)

  #indsHoldout = sample((1:NROW(dbY))[-indsTrain], 0.4*NROW(indsTrain))
  return(list(Train=indsTrain,Holdout=indsHoldout))
}
