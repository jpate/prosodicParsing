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

  // def argmax( corpus:List[List[ObservedType]] ) =
  //   corpus.map{ string =>
  //     buildSlicedHMM( string )
  //     val maxAssn = Models.viterbi( hmm,  ForwardBackwardInferencer.createForMaxProduct() )
  //     assignmentToViterbiString( maxAssn )
  //   }

  def reestimate( corpus: List[List[ObservedType]] ):Double
}

/*
trait PointEstimateEM {
  def mapPartialCounts( input:Double ):Double = input
}

trait VariationalBayes {
  import math.{exp,log}
  def mapPartialCounts( input:Double ):Double =
    //exp( 
      if( input <= 0 ) {
        Double.NegativeInfinity
      } else {
        var r = 0D
        var x = input
        while( x <= 5 ) {
          r -= 1/x
          x += 1
        }
        val f = 1/(x*x)
        val t = f*(-1/12.0 + f*(1/120.0 + f*(-1/252.0 + f*(1/240.0 + f*(-1/132.0
            + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
        r + log(x) - 0.5/x + t;
      }
    //)
}
*/

