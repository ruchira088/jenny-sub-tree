package com.ruchij

case class Graph[A](values: Map[A, List[A]])
{
  self =>

  import Graph._

  def merge(graph: Graph[A]): Graph[A] = Graph(flatten(self) ++ flatten(graph))

  def isEmpty: Boolean = values.isEmpty
}

object Graph
{
  def empty[A]: Graph[A] = Graph(Map.empty[A, List[A]])

  def apply[A](values: List[(A, A)]): Graph[A] =
    values.foldLeft(Graph.empty[A])(add)

  def flatten[A](graph: Graph[A]): List[(A, A)] =
    graph.values.toList.flatMap { case (key, values) => values.map(value => key -> value) }

  def remove[A](graph: Graph[A], value: A): Graph[A] =
    Graph {
      (graph.values - value)
        .map {
          case (key, neighbours) => key -> neighbours.filter(_ != value)
        }
    }

  def add[A](graph: Graph[A], connection: (A, A)): Graph[A] =
  {
    def addConnection(x: A, y: A): (A, List[A]) =
      x -> graph.values.get(x).fold(List(y))(y :: _)

    val (a, b) = connection

    val nodeA = addConnection(a, b)
    val nodeB = addConnection(b, a)

    Graph(graph.values + nodeA + nodeB)
  }

  def maxConnectionNode[A](graph: Graph[A]): Option[(A, List[A])] =
    if (graph.isEmpty)
      None
    else
      Some(graph.values.maxBy { case (_, neighbours) => neighbours.length })

  def isIsomorphic[A, B](graphA: Graph[A], graphB: Graph[B]): Boolean =
    if (graphA.isEmpty && graphB.isEmpty)
      true
    else {
      val result = for {
        (a, neighboursA) <- maxConnectionNode(graphA)
        (b, neighboursB) <- maxConnectionNode(graphB) if neighboursA.length == neighboursB.length
      }
      yield isIsomorphic(Graph(graphA.values - a), Graph(graphB.values - b))

      result.getOrElse(false)
    }
}
