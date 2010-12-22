
package prosodicParsing.HMMs
import prosodicParsing.types._
import collection.immutable.HashMap

abstract class AbstractHMM[Q<:AbstractHiddenState,O<:AbstractObservation,P<:AbstractHMMParameters] {
  def randomHMM(
    hiddenStates:Set[Q],
    observations:Set[O],
    randomSeed:Int,
    centeredOn:Int
  ):Unit

  def viterbi( s:List[O] ):List[Q]
  def computeExpectations( s:List[O] ):Expectation

  def setParameters( newParams:P ):Unit

}

abstract class AbstractHMMParameters

/*
class PlainHMMParameters extends AbstractHMMParameters {

  def setInitialStateProbs( newInitialProbs:HashMap[HiddenState,Double] ) {
    initialStateProbs = newInitialProbs
  }

  def setTransitions( newTransitions:HashMap[HiddenState,HashMap[HiddenState,Double]] ) {
    transitions = newTransitions
  }

  def setEmissions( newObservations:HashMap[HiddenState,HashMap[Observation,Double]] ) {
    emissions = newObservations
  }

}
*/

