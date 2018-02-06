package com.ruchij

import com.ruchij.Graph.remove
import com.ruchij.utils.ScalaUtils.optionSequence

case class Node[A](value: A, neighbours: List[Node[A]] = List.empty[Node[A]])

object Node
{
  def createNode[A](graph: Graph[A], root: A): Option[Node[A]] =
    for {
      neighbours <- graph.values.get(root)

      neighbourNodes <- optionSequence(neighbours.map(createNode(remove(graph, root), _)))

      rootNode = Node(root, neighbourNodes)
    }
    yield rootNode

  def trim[A](node: Node[A], length: Int): Node[A] =
    if (length == 0)
      Node(node.value)
    else
      Node(node.value, neighbours = node.neighbours.map(trim(_, length - 1)))

  def flatten[A](node: Node[A]): List[(A, A)] =
    node.neighbours.map(_.value -> node.value) ++ node.neighbours.flatMap(flatten)

  def createGraph[A](node: Node[A]): Graph[A] = Graph(flatten(node))
}