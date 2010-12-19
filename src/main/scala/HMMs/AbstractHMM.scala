
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
case class PlainHMMParameters(
  initialProbs: HashMap[HiddenState,Double],
  transitionProbs: HashMap[HiddenState,HashMap[HiddenState,Double]],
  emissionProbs: HashMap[HiddenState,HashMap[Observation,Double]]
) extends AbstractHMMParameters

