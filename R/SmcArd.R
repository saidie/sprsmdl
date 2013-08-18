# File sprsmdl/R/SmcArd.R
# Part of the sprsmdl package for R.
#
# Copyright (C) 2013 Hiroshi Saito
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/

## Sparse multiclass classification with automatic relevance determination
SmcArd <- function(T, X, bias=TRUE, method=c("VB", "VBMacKay", "PX-VB"),
                   control=list(), check.lb=TRUE, a0=0, b0=0, mu0=0, xi0=1){
  ## Check input
  if(is.matrix(T))
    T <- c(T)

  labels <- unique(T)

  ress <- lapply(X=labels, FUN=function(l){
    SlrArd(T == l, X, bias, method, control, check.lb, a0, b0, mu0, xi0)
  })

  ## Return results
  res <- list()
  for(n in names(ress[[1]])){
    res[[n]] <- do.call(cbind, sapply(X=ress, FUN=function(res) res[n]))
    colnames(res[[n]]) <- labels
  }

  return(res)
}
