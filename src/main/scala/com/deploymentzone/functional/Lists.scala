package com.deploymentzone.functional

import scala.annotation.tailrec

object Lists {

  def tail[A](s: List[A]): List[A] = {
    s match {
      case Nil => Nil
      case _ :: tail => tail
    }
  }

  def setHead[A](newHead: A, s: List[A]): List[A] = {
    require(newHead != Nil)
    newHead :: tail(s)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case 0 => l
      case x => drop(tail(l), x - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case h :: t if f(h) => dropWhile(t)(f)
      case rest => rest
    }
  }

  def init[A](l: List[A]): List[A] = {
    tail(l.reverse).reverse
  }

}
