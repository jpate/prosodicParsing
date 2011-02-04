package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.ForwardBackwardInferencer

abstract class AbstractHMM[HiddenType<:HiddenLabel,ObservedType<:ObservedLabel](
  hiddenStateTypesSet:Set[HiddenType],
  observationTypesSet:Set[ObservedType]
) {
  var parameters:Set[AbstractDistribution]
  // val observationTypes:List[ObservedType]
  // val hiddenStateTypes:List[HiddenType]

  val observationTypes = observationTypesSet.toList.sortWith( (a,b) => a < b )
  val hiddenStateTypes = hiddenStateTypesSet.toList.sortWith( (a,b) => a < b )

  val numHiddenStates = hiddenStateTypes.size

  def randomize( n:Int ) {
    parameters.foreach( _.randomize( n ) )
  }

  def normalize {
    parameters.foreach( _.normalize )
  }

  def scale(n:Int) {
    parameters.foreach( _.scale(n) )
  }

  def deScale(n:Int) {
    parameters.foreach( _.deScale(n) )
  }

  val scaleBy = 1000

  //var hmm = new DirectedModel()
  var hmm = new DynamicBayesNet(0)

  def buildSlicedHMM( tokens:List[ObservedType] ):Unit

  def buildHMM( tokens:List[ObservedType] ):Unit

  // translated from nltk
  def log_add( ns:List[Double] ) = {
    import math.{log,exp}
    val maxVal = ns.max
    if( maxVal > scala.Double.NegativeInfinity )
      maxVal + log( ns.foldLeft(0D)( (a,b) => a + exp( b - maxVal ) ) )
    else
      maxVal
  }

  var observations:Array[Variable] = Array()
  var hiddenVariables:Array[Variable] = Array()

  def generateObservationSequence( tokens:List[ObservedType] ):Assignment

  val inferencer = new ForwardBackwardInferencer()
  def generalProbability( tokens:List[ObservedType] ) = {
    buildHMM( tokens )


    val observationSequence = generateObservationSequence( tokens )

    //println( "shazam" )
    inferencer.queryLogForwardBackward( hmm, observationSequence )
  }

  def computePartialCounts( sequence:List[ObservedType] ):PartialCounts

  def viterbi( sequence:List[ObservedType] ):List[HiddenType]

  def reestimate( corpus: List[List[ObservedType]] ):Double
}

