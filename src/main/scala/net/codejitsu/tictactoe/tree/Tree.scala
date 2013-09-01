package net.codejitsu.tictactoe.tree

trait Tree[+A] {
  import scala.annotation.tailrec

  def value: Option[A] = this match {
    case n: Node[A] => Some(n.v)
    case l: Leaf[A] => Some(l.v)
    case Empty => None
  }

  def left: Option[Tree[A]] = this match {
    case n: Node[A] => Some(n.l)
    case l: Leaf[A] => None
    case Empty => None
  }

  def right: Option[Tree[A]] = this match {
    case n: Node[A] => Some(n.r)
    case l: Leaf[A] => None
    case Empty => None
  }
}

case class Node[A](v: A, l: Tree[A], r: Tree[A]) extends Tree[A]
case class Leaf[A](v: A) extends Tree[A]
case object Empty extends Tree[Nothing]