package com.ruchij

object App
{
  def main(args: Array[String]): Unit =
  {
    val scenario_1 = JennySubTree(List(1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 2 -> 6, 2 -> 7), 1)
    val scenario_2 = JennySubTree(List(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 6 -> 7), 3)

    List(scenario_1, scenario_2)
      .foreach {
        scenario => println(JennySubTree.countIsomorphicTrees(scenario))
      }
  }
}