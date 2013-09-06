package com.deploymentzone.functional

object IsSorted {
  def apply[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    require(as != null)

    if (as.length <= 1) return true

    as.sliding(2).foldLeft(true)((truth, pair) => truth & gt(pair(1), pair(0)) )
  }
}
