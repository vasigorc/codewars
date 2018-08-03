package ca.vgorcinschi

import org.scalatest.FlatSpec

class NPointCrossoverTest extends FlatSpec {

  import NPointCrossover.crossover

  it should "work when there is no cross-point index" in {
    assert( crossover(Nil, List(1,1,1,1,1), List(2,2,2,2,2)) === (List(1,1,1,1,1), List(2,2,2,2,2)) )
  }

  it should "work with 1 cross-point index" in {
    assert( crossover(List(1), List(1,1,1,1,1), List(2,2,2,2,2)) === (List(1,2,2,2,2),List(2,1,1,1,1)) )
  }

  it should "work with 1 repeated cross-point index" in {
    assert( crossover(List(1,1), List(1,1,1,1,1), List(2,2,2,2,2)) === (List(1,2,2,2,2),List(2,1,1,1,1)) )
  }

  it should "work for 2 cross-point indices" in {
    assert( crossover(List(1,3), List(1,1,1,1,1), List(2,2,2,2,2)) === (List(1,2,2,1,1),List(2,1,1,2,2)) )
  }

  it should "work for 3 cross-point indices" in {
    assert( crossover(List(1,3,5), List(1,1,1,1,1,1,1), List(2,2,2,2,2,2,2)) === (List(1,2,2,1,1,2,2), List(2,1,1,2,2,1,1)) )
  }

  it should "work for unordered cross-point indices" in {
    assert( crossover(List(3,5,1), List(1,1,1,1,1,1,1), List(2,2,2,2,2,2,2)) === (List(1,2,2,1,1,2,2), List(2,1,1,2,2,1,1)) )
  }

}