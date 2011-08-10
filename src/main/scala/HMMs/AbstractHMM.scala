package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.ForwardBackwardInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.util.Models
import cc.mallet.util.Maths

abstract class AbstractHMM[HiddenType<:HiddenLabel,ObservedType<:ObservedLabel](
  hiddenStateTypesSet:Set[HiddenType],
  observationTypesSet:Set[ObservedType]
) {
  //var matrices:Set[AbstractDistribution]
  //var parameters:Parameters
  //type Dist<:AbstractDistribution
  def parameters:List[AbstractDistribution]

  val observationTypes = observationTypesSet.toList.sortWith( (a,b) => a < b )
  val hiddenStateTypes = hiddenStateTypesSet.toList.sortWith( (a,b) => a < b )

  val numHiddenStates = hiddenStateTypes.size

  def randomize( seed:Int, centeredOn:Int ) {
    parameters.foreach( _.randomize( seed, centeredOn ) )
  }

  var localUniverse = new Universe()

  def normalize {
    parameters.foreach( _.normalize )
  }

  def setParams( params:Parameters ):Unit

  var hmm = new DynamicBayesNet(0)

  def buildHMM( tokens:List[ObservedType] ):Unit

  def buildSlicedHMM( tokens:List[ObservedType] ) {
    buildHMM( tokens )
    hmm = Models.addEvidence(
      hmm,
      generateObservationSequence( tokens )
    )
  }


  var observations:Array[Variable] = Array()
  var hiddenVariables:Array[Variable] = Array()

  def generateObservationSequence( tokens:List[ObservedType] ):Assignment

  val inferencer = new ForwardBackwardInferencer()
  def generalProbability( tokens:List[ObservedType] ) = {
    buildHMM( tokens )

    val observationSequence = generateObservationSequence( tokens )

    val simpleLogProb =
      ForwardBackwardInferencer.createForUnnormalizedSumProduct.querySimpleLogProb( hmm, observationSequence )
    simpleLogProb
  }

  def computePartialCounts( sequence:List[ObservedType] ):PartialCounts

  def assignmentToViterbiString( maxAssn:Assignment ):List[HiddenType]

  var stringLength = 0

  def argmax( string:List[ObservedType] ) = {
    buildSlicedHMM( string )
    val maxAssn = Models.bestAssignment( hmm, ForwardBackwardInferencer.createForMaxProduct() )
    assignmentToViterbiString( maxAssn )
  }

  def reestimate( corpus: List[List[ObservedType]] ):Double
}

