package com.ruchij

import com.ruchij.utils.ScalaUtils.optionSet

case class JennySubTree[A](connections: List[(A, A)], radius: Int)

object JennySubTree
{
  def countIsomorphicTrees[A](jennySubTree: JennySubTree[A]): Int =
  {
    val graph = Graph(jennySubTree.connections)

      optionSet(
        graph.values.keySet
          .map(Graph.trim(graph, _, jennySubTree.radius))
      )
      .getOrElse(Set.empty[Graph[A]])
      .foldLeft(List.empty[Graph[A]]) {
        case (list, current) =>
          if (list.exists(Graph.isIsomorphic(_, current))) list else current :: list
      }
      .length
  }
}