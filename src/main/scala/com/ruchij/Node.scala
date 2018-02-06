package com.ruchij

import com.ruchij.Graph.remove
import com.ruchij.utils.ScalaUtils.optionSequence

case class Node[A](value: A, neighbours: Set[Node[A]] = Set.empty[Node[A]])

object Node
{
  def createNode[A](graph: Graph[A], root: A): Option[Node[A]] =
    for {
      neighbours <- graph.values.get(root)

      neighbourNodes <- optionSequence(neighbours.map(createNode(remove(graph, root), _)).toList)

      rootNode = Node(root, neighbourNodes.toSet)
    }
    yield rootNode

  def trim[A](node: Node[A], length: Int): Node[A] =
    if (length == 0)
      Node(node.value)
    else
      Node(node.value, neighbours = node.neighbours.map(trim(_, length - 1)))

  def flatten[A](node: Node[A]): List[(A, A)] =
    node.neighbours.toList.map(_.value -> node.value) ++ node.neighbours.toList.flatMap(flatten)

  def createGraph[A](node: Node[A]): Graph[A] =
    Graph(flatten(node))
}