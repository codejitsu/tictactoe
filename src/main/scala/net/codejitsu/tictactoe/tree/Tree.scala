package net.codejitsu.tictactoe.tree

trait Tree[+A] {
  import scala.annotation.tailrec

  def value: Option[A] = this match {
    case n: Fork[A] => Some(n.v)
    case l: Leaf[A] => Some(l.v)
    case Empty => None
  }

  def sub(i: Int): Option[Tree[A]] = this match {
    case n: Fork[A] => Some(n.ch(i))
    case l: Leaf[A] => None
    case Empty => None
  }
  
  def isEmpty: Boolean = this match {
    case n: Fork[A] => false
    case l: Leaf[A] => true
    case Empty => true    
  }
  
  def children: Option[List[Tree[A]]] = this match {
    case n: Fork[A] => Some(n.ch.toList)
    case l: Leaf[A] => None
    case Empty => None    
  }
}

case class Fork[A](v: A, ch: Tree[A]*) extends Tree[A]
case class Leaf[A](v: A) extends Tree[A]
case object Empty extends Tree[Nothing]