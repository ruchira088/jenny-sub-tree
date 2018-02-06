package com.ruchij

object App
{
  def main(args: Array[String]): Unit =
  {
    val connections = List(1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 2 -> 6, 2 -> 7)
//val connections = List(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 6 -> 7)
    val graph = Graph(connections)

    println(
      graph.values.keys.toList.map(key => Node.trim(Node.createNode(graph, key).get, 1))
        .map(Node.createGraph)
        .foldLeft(List.empty[Graph[Int]]) {
          case (list, graph1) => if (list.exists(Graph.isIsomorphic(_, graph1))) list else graph1 :: list
        }.length
    )

  }

  def add(x: Int, y: Int): Int = x + y
}
