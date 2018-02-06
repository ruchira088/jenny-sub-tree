package com.ruchij

import org.scalatest.{FlatSpec, Matchers}

class JennySubTreeTest extends FlatSpec with Matchers
{
  val scenario_1 = JennySubTree(List(1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 2 -> 6, 2 -> 7), 1)
  val scenario_2 = JennySubTree(List(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 6 -> 7), 3)

  "Calculating the isomorphic tree count" should "return correct results" in {
    JennySubTree.countIsomorphicTrees(scenario_1) shouldEqual 3
    JennySubTree.countIsomorphicTrees(scenario_2) shouldEqual 4
  }
}
