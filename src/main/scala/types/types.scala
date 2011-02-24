package ProsodicParsing.types
//import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap
//import ProsodicParsing.util.Util
import cc.mallet.util.Maths
import math.{exp,log}


abstract class Label(s:String) {
  // THIS IS NECESSARY TO GET A PREDICTABLE SORT SO THAT toArray IS STABLE
  override def toString = s

  def <( l2:Label) = toString < l2.toString
  def >( l2:Label) = toString > l2.toString
}

abstract class HiddenLabel( s:String ) extends Label(s)
abstract class ObservedLabel( s:String ) extends Label(s)

case class HiddenState(s:String) extends HiddenLabel(s) {
  def * (hiddenState:HiddenState):HiddenStatePair = HiddenStatePair( s, hiddenState.s )
}
case class ObservedState(s:String) extends ObservedLabel(s)

trait StatePair

case class HiddenStatePair( hidd1:String, hidd2:String )
  extends HiddenLabel( hidd1+"^"+hidd2 ) with StatePair
case class ObservedStatePair( obs1:String, obs2:String )
  extends ObservedLabel( obs1+"^"+obs2 ) with StatePair

abstract class AbstractDistribution {
  def randomize(seed:Int,centeredOn:Int):Unit
  def normalize:Unit
}

abstract class AbstractConditionalProbabilityDistribution[T<:Label,U<:Label] extends AbstractDistribution {
  var cpt:HashMap[T,HashMap[U,Double]]

  def apply( k:T ) = cpt( k )

  def setCPT( updatedCPT: HashMap[T,HashMap[U,Double]] ) {
    cpt = updatedCPT
  }

  def normalize {
    val maxes = HashMap(
      cpt.keySet.map( parent => parent -> (cpt(parent)).values.sum ).toSeq:_*
    )

    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            if( maxes(parent) == 0D )
              child -> 0D
            else
              child -> cpt(parent)(child) / maxes(parent)
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( r.nextDouble + centeredOn )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    normalize
  }



  val sortedParentKeys:List[T]
  val sortedChildKeys:List[U]

  def toArray = {
    sortedParentKeys.flatMap{ parent =>
      sortedChildKeys.map{ child =>
        cpt(parent)(child)
      }
    }.toArray
  }

  def keySet = cpt.keySet

  override def toString = sortedParentKeys.map{ parent =>
    sortedChildKeys.map{ ch =>
      parent + " --> " +ch + ":\t" + cpt(parent)(ch)
    }.mkString("\n\t","\n\t","")
  }.mkString("","\n","\n")
}

abstract class AbstractConditionalLogProbabilityDistribution[T<:Label,U<:Label] extends AbstractDistribution {
  import math.{exp,log}

  var cpt:HashMap[T,HashMap[U,Double]]

  def apply( k:T ) = cpt( k )

  def setCPT( updatedCPT: HashMap[T,HashMap[U,Double]] ) {
    cpt = updatedCPT
  }

  def normalize {
    val maxes = HashMap(
      cpt.keySet.map( parent =>
        parent -> ( cpt(parent).values.reduceLeft( Maths.sumLogProb(_,_) ) )
      ).toSeq:_*
    )

    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> ( cpt(parent)(child) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  private def zeroOut( from:T, to:U ) = {
    cpt(from)(to) = Double.NegativeInfinity
  }

  def zeroAll( toZero:Set[Tuple2[T,U]] ) {
    toZero.foreach{ case( from, to ) => zeroOut( from, to ) }
    normalize
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( log( r.nextDouble + centeredOn ) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    normalize
  }

  val sortedParentKeys:List[T]
  val sortedChildKeys:List[U]

  def toArray = {
    sortedParentKeys.flatMap{ parent =>
      sortedChildKeys.map{ child =>
        exp( cpt(parent)(child) )
      }
    }.toArray
  }

  def toLogArray = {
    sortedParentKeys.flatMap{ parent =>
      sortedChildKeys.map{ child =>
        cpt(parent)(child)
      }
    }.toArray
  }

  def keySet = cpt.keySet

  override def toString = sortedParentKeys.map{ parent =>
    sortedChildKeys.map{ ch =>
      parent + " --> " +ch + ":\t" + exp( cpt(parent)(ch) )
    }.mkString("\n\t","\n\t","")
  }.mkString("","\n","\n")
}

class ConditionalLogProbabilityDistribution[T<:Label,U<:Label]( parents:Set[T], children:Set[U] )
  extends AbstractConditionalLogProbabilityDistribution[T,U] {
  val sortedParentKeys = parents.toList.sortWith( (a,b) => a < b )
  val sortedChildKeys = children.toList.sortWith( (a,b) => a < b )
  var cpt = HashMap(
    parents.map( parent =>
        parent -> (
          HashMap(
            children.map( child =>
              child -> log( 1D/children.size )
            ).toSeq: _*
          )
        )
      ).toSeq: _*
    )
}


abstract class AbstractLogProbabilityDistribution[T<:Label] extends AbstractDistribution {
  var pt:HashMap[T,Double]

  def setPT( updatedPT: HashMap[T,Double] ) {
    pt = updatedPT
  }

  def *[U<:Label]( otherCPT: AbstractConditionalLogProbabilityDistribution[T,U] ) =
    new AbstractConditionalLogProbabilityDistribution[T,U] {
      val sortedParentKeys = otherCPT.sortedParentKeys
      val sortedChildKeys = otherCPT.sortedChildKeys
      var cpt = HashMap(
        otherCPT.keySet.map{ parent =>
          parent -> HashMap (
            otherCPT(parent).keySet.map{ child =>
              child -> ( pt( parent ) + otherCPT( parent )( child ) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }

  def keySet = pt.keySet

  def *[U<:Label]( otherCPT: AbstractConditionalProbabilityDistribution[T,U] ) =
    new AbstractConditionalProbabilityDistribution[T,U] {
      val sortedParentKeys = otherCPT.sortedParentKeys
      val sortedChildKeys = otherCPT.sortedChildKeys
      var cpt = HashMap(
        otherCPT.keySet.map{ parent =>
          parent -> HashMap (
            otherCPT(parent).keySet.map{ child =>
              child -> ( pt( parent ) + log( otherCPT( parent )( child ) ) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }

  private def zeroOut( element:T ) = {
    pt(element) = Double.NegativeInfinity
  }

  def zeroAll( toZero:Set[T] ) {
    toZero.foreach{ element => zeroOut( element ) }
    normalize
  }

  def normalize {
    val max = pt.values.reduceLeft( Maths.sumLogProb( _ , _) )

    pt = HashMap(
      pt.keySet.map{ parent =>
          // by convention 0 / 0 = 0
        if( max == Double.NegativeInfinity )
          parent -> Double.NegativeInfinity
        else
          parent -> ( pt(parent) - max )
      }.toSeq:_*
    )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( log( r.nextDouble + centeredOn ) )
      }.toSeq:_*
    )

    normalize
  }

  val sortedElements:List[T]
  def toArray = sortedElements.map( k => exp( pt(k) ) ).toArray
  def toLogArray = sortedElements.map( k => pt(k) ).toArray

  def apply( k:T ) = pt( k )

  override def toString = sortedElements.map{ parent =>
    parent + ":\t" + exp( pt(parent) )
  }.mkString("\n\t","\n\t","\n")
}

class LogProbabilityDistribution[T<:Label]( domain:Set[T] ) extends AbstractLogProbabilityDistribution[T] {
  val sortedElements = domain.toList.sortWith( (a,b) => a < b )
  var pt = HashMap(
    domain.map( element =>
      element -> log( 1D/ domain.size )
    ).toSeq: _*
  )
}


abstract class AbstractProbabilityDistribution[T<:Label] extends AbstractDistribution {
  var pt:HashMap[T,Double]

  def setPT( updatedPT: HashMap[T,Double] ) {
    pt = updatedPT
  }

  def *[U<:Label]( otherCPT: AbstractConditionalLogProbabilityDistribution[T,U] ) =
    new AbstractConditionalLogProbabilityDistribution[T,U] {
      val sortedParentKeys = otherCPT.sortedParentKeys
      val sortedChildKeys = otherCPT.sortedChildKeys
      var cpt = HashMap(
        otherCPT.keySet.map{ parent =>
          parent -> HashMap (
            otherCPT(parent).keySet.map{ child =>
              child -> ( log( pt( parent )) + otherCPT( parent )( child ) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }

  def *[U<:Label]( otherCPT: AbstractConditionalProbabilityDistribution[T,U] ) =
    new AbstractConditionalProbabilityDistribution[T,U] {
      val sortedParentKeys = otherCPT.sortedParentKeys
      val sortedChildKeys = otherCPT.sortedChildKeys
      var cpt = HashMap(
        otherCPT.keySet.map{ parent =>
          parent -> HashMap (
            otherCPT(parent).keySet.map{ child =>
              child -> ( pt( parent ) * otherCPT( parent )( child ) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }


  def domain = pt.keySet

  def normalize {
    val max = pt.values.sum
    pt = HashMap(
      pt.keySet.map{ parent =>
        if( max == 0D )
          parent -> 0D
        else
          parent -> pt(parent) / max
      }.toSeq:_*
    )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( r.nextDouble + centeredOn )
      }.toSeq:_*
    )

    normalize
  }

  def sortedElements:List[T]
  def toArray = sortedElements.map( pt(_) ).toArray

  def apply( k:T ) = pt( k )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    parent + ":\t" + pt(parent)
  }.mkString("\n\t","\n\t","\n")
}


abstract class PartialCounts {
  def +( otherPC:PartialCounts ):PartialCounts
  def toParameters:Parameters
  val logProb:Double
}

case class PlainHMMPartialCounts(
  stringLogProb: Double,
  initialStateCounts: HashMap[HiddenState,Double],
  transitionCounts: HashMap[HiddenState,HashMap[HiddenState,Double]],
  emissionCounts: HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends PartialCounts {
  val logProb = stringLogProb
  def +( otherPC: PartialCounts ) = {
    val PlainHMMPartialCounts(
      otherStringLogProb,
      otherInitialStateCounts,
      otherTransitionCounts,
      otherEmissionCounts
    ) = otherPC

    PlainHMMPartialCounts(
      stringLogProb + otherStringLogProb,
      HashMap(
        initialStateCounts.keySet.map{ q =>
          q -> Maths.sumLogProb(
            initialStateCounts(q), otherInitialStateCounts(q) //- otherStringLogProb
          )
        }.toSeq:_*
      ),
      HashMap(
        transitionCounts.keySet.map{ qFrom =>
          qFrom -> HashMap(
            transitionCounts(qFrom).keySet.map{ qTo =>
              qTo -> Maths.sumLogProb(
                  transitionCounts(qFrom)(qTo),
                  otherTransitionCounts(qFrom)(qTo) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCounts.keySet.map{ q =>
          q -> HashMap(
            emissionCounts(q).keySet.map{ obs =>
              obs -> Maths.sumLogProb(
                emissionCounts(q)(obs),
                otherEmissionCounts(q)(obs) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }

  def toParameters = PlainHMMParameters(
    initialStateCounts,
    transitionCounts,
    emissionCounts
  )
}

case class CoupledHMMPartialCounts(
  stringLogProb: Double,
  initialStateCounts: HashMap[HiddenStatePair,Double],
  transitionCountsA: HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
  transitionCountsB: HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
  emissionCountsA: HashMap[HiddenState,HashMap[ObservedState,Double]],
  emissionCountsB: HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends PartialCounts {
  val logProb = stringLogProb
  def +( otherPC: PartialCounts ) = {
    val CoupledHMMPartialCounts(
      otherStringLogProb,
      otherInitialStateCounts,
      otherTransitionCountsA,
      otherTransitionCountsB,
      otherEmissionCountsA,
      otherEmissionCountsB
    ) = otherPC

    CoupledHMMPartialCounts(
      stringLogProb + otherStringLogProb,
      HashMap(
        initialStateCounts.keySet.map{ q =>
          q -> Maths.sumLogProb(
            initialStateCounts(q), otherInitialStateCounts(q) //- otherStringLogProb
          )
        }.toSeq:_*
      ),
      HashMap(
        transitionCountsA.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            transitionCountsA(qsFrom).keySet.map{ qA =>
              qA -> Maths.sumLogProb(
                  transitionCountsA(qsFrom)(qA),
                  otherTransitionCountsA(qsFrom)(qA) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        transitionCountsB.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            transitionCountsB(qsFrom).keySet.map{ qB =>
              qB -> Maths.sumLogProb(
                  transitionCountsB(qsFrom)(qB),
                  otherTransitionCountsB(qsFrom)(qB) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCountsA.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            emissionCountsA(qsFrom).keySet.map{ obsA =>
              obsA -> Maths.sumLogProb(
                  emissionCountsA(qsFrom)(obsA),
                  otherEmissionCountsA(qsFrom)(obsA) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCountsB.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            emissionCountsB(qsFrom).keySet.map{ obsB =>
              obsB -> Maths.sumLogProb(
                  emissionCountsB(qsFrom)(obsB),
                  otherEmissionCountsB(qsFrom)(obsB) //- otherStringLogProb
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }

  def toParameters = CoupledHMMParameters(
    initialStateCounts,
    transitionCountsA,
    transitionCountsB,
    emissionCountsA,
    emissionCountsB
  )
}


abstract class Parameters

case class CoupledHMMParameters(
initialProbs:HashMap[HiddenStatePair,Double],
transitionsA:HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
transitionsB:HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
emissionsA:HashMap[HiddenState,HashMap[ObservedState,Double]],
emissionsB:HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends Parameters

case class PlainHMMParameters(
initialProbs:HashMap[HiddenState,Double],
transitions:HashMap[HiddenState,HashMap[HiddenState,Double]],
emissions:HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends Parameters

abstract class Estimate
case class EstimateCorpus( s:List[List[ObservedLabel]] ) extends Estimate
case class EstimateUtterance( s:List[ObservedLabel] ) extends Estimate

//case class Viterbi( s:List[ObservedLabel] ) extends Estimate

case class ViterbiString( stringLabel:String, string:List[ObservedLabel] ) {
  def size = string.size
}

case class Viterbi( iterationCount:Int, vit:List[ViterbiString] )

case class Randomize( seed:Int, centeredOn:Int )

case object Initialize

case object EMEnd

case object Stop


