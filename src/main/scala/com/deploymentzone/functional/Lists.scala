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

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    def loop(l: List[A], z: B)(f: (A, B) => B): B = {
      l match {
        case Nil => z
        case h :: t => f(h, loop(t, z)(f))
      }
    }

    loop(l.reverse, z)(f)
  }

  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, _) => b + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(acc: B, rest: List[A]): B = {
      rest match {
        case Nil => acc
        case h :: t => loop(f(acc, h), t)
      }
    }
    loop(z, l)
  }

  def sum[A: Numeric](l: List[A]): A = {
    val numeric = implicitly[Numeric[A]]
    foldLeft(l, numeric.zero)(numeric.plus)
  }

  def product[A: Numeric](l: List[A]): A = {
    val numeric = implicitly[Numeric[A]]
    foldLeft(l, numeric.one)(numeric.times)
  }
}
