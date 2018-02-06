package com.ruchij

import com.ruchij.utils.ScalaUtils

case class Graph[A](values: Map[A, Set[A]])
{
  self =>

  import Graph._

  def merge(graph: Graph[A]): Graph[A] = Graph(flatten(self) ++ flatten(graph))

  def isEmpty: Boolean = values.isEmpty
}

object Graph
{
  def empty[A]: Graph[A] = Graph(Map.empty[A, Set[A]])

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
    def addConnection(x: A, y: A): (A, Set[A]) =
      x -> graph.values.get(x).fold(Set(y))(_ + y)

    val (a, b) = connection

    val nodeA = addConnection(a, b)
    val nodeB = addConnection(b, a)

    Graph(graph.values + nodeA + nodeB)
  }

  def trim[A](graph: Graph[A], value: A, length: Int): Option[Graph[A]] =
    if (length == 0 && graph.values.contains(value))
      Some(Graph(Map(value -> Set.empty[A])))
    else
      for {
        neighbours <- graph.values.get(value)
        rest <- ScalaUtils.optionSet(neighbours.map(trim(remove(graph, value), _, length - 1)))

        result = rest.foldLeft(Graph(Map(value -> neighbours))) { _.merge(_) }
      }
      yield result

  def maxConnectionNode[A](graph: Graph[A]): Option[(A, Set[A])] =
    if (graph.isEmpty)
      None
    else
      Some(graph.values.maxBy { case (_, neighbours) => neighbours.size })

  def isIsomorphic[A, B](graphA: Graph[A], graphB: Graph[B]): Boolean =
    if (graphA.isEmpty && graphB.isEmpty)
      true
    else {
      val result = for {
        (a, neighboursA) <- maxConnectionNode(graphA)
        (b, neighboursB) <- maxConnectionNode(graphB) if neighboursA.size == neighboursB.size
      }
      yield isIsomorphic(Graph(graphA.values - a), Graph(graphB.values - b))

      result.getOrElse(false)
    }
}
