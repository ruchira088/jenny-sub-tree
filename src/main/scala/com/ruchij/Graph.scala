package com.ruchij

case class Graph[A](values: Map[A, Set[A]])
{
  self =>

  import Graph._

  def merge(graph: Graph[A]): Graph[A] = Graph(flatten(self) ++ flatten(graph))
}

object Graph
{
  def empty[A]: Graph[A] = Graph(Map.empty[A, Set[A]])

  def apply[A](values: List[(A, A)]): Graph[A] =
    values.foldLeft(Graph.empty[A])(add)

  def flatten[A](graph: Graph[A]): List[(A, A)] =
    graph.values.toList.flatMap { case (key, values) => values.toList.map(value => key -> value) }

  def remove[A](graph: Graph[A], value: A): Graph[A] =
    Graph {
      (graph.values - value)
        .map {
          case (key, neighbours) => key -> neighbours.filter(_ != value)
        }
    }

  def add[A](graph: Graph[A], connection: (A, A)): Graph[A] =
  {
    def addConnection(x: A, y: A): (A, Set[A]) =
      x -> graph.values.get(x).fold(Set(y))(_ + y)

    val (a, b) = connection

    val nodeA = addConnection(a, b)
    val nodeB = addConnection(b, a)

    Graph(graph.values + nodeA + nodeB)
  }

  def isIsomorphic[A, B](graphA: Graph[A], graphB: Graph[B]): Boolean =
    if (graphA.values.isEmpty && graphB.values.isEmpty)
      true
    else {
      val maxA = graphA.values.maxBy { case (_, neighbours) => neighbours.size }
      val maxB = graphB.values.maxBy { case (_, neighbours) => neighbours.size }

      if (maxA._2.size == maxB._2.size)
        isIsomorphic(Graph(graphA.values - maxA._1), Graph(graphB.values - maxB._1))
      else
        false
    }


}
