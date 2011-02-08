package ProsodicParsing.types
import scala.collection.immutable.HashMap
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
  def randomize(n:Int):Unit
  def normalize:Unit
  def scale(n:Int):Unit
  def deScale(n:Int):Unit
}

abstract class ConditionalProbabilityDistribution[T<:Label,U<:Label] extends AbstractDistribution {
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
            child -> cpt(parent)(child) / maxes(parent)
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  val seed = 15
  def randomize( centeredOn:Int ) {
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

  def scale( n:Int ) {
    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( cpt(parent)(child) * n )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def deScale( n:Int ) {
    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( cpt(parent)(child) / n )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def toArray = {
    cpt.keySet.toList.sortWith( (a,b) => a < b ).flatMap{ parent =>
      cpt(parent).keySet.toList.sortWith( (c,d) => c < d ).map{ child =>
        cpt(parent)(child)
      }
    }.toArray
  }

  def keySet = cpt.keySet

  override def toString = cpt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    cpt(parent).keySet.toList.sortWith( (a,b) => a < b ).map{ ch =>
      parent + " --> " +ch + ":\t" + cpt(parent)(ch)
    }.mkString("\n\t","\n\t","")
  }.mkString("","\n","\n")
}

abstract class ConditionalLogProbabilityDistribution[T<:Label,U<:Label] extends AbstractDistribution {
  import math.{exp,log}
  // translated from nltk
  def log_add( ns:List[Double] ) = {
    val maxVal = ns.max
    if( maxVal > scala.Double.NegativeInfinity )
      maxVal + log( ns.foldLeft(0D)( (a,b) => a + exp( b - maxVal ) ) )
    else
      maxVal
  }

  var cpt:HashMap[T,HashMap[U,Double]]

  def apply( k:T ) = cpt( k )

  def setCPT( updatedCPT: HashMap[T,HashMap[U,Double]] ) {
    cpt = updatedCPT
  }

  def normalize {
    val maxes = HashMap(
      cpt.keySet.map( parent => parent -> ( log_add( cpt(parent).values.toList ) ) ).toSeq:_*
    )

    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( cpt(parent)(child) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  val seed = 15
  def randomize( centeredOn:Int ) {
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

  def deScale( n:Int ) {
    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( cpt(parent)(child) - log( n ) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def scale( n:Int ) {
    cpt = HashMap(
      cpt.keySet.map{ parent =>
        parent -> HashMap(
          cpt(parent).keySet.map{ child =>
            child -> ( cpt(parent)(child) + log( n ) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def toArray = {
    cpt.keySet.toList.sortWith( (a,b) => a < b ).flatMap{ parent =>
      cpt(parent).keySet.toList.sortWith( (c,d) => c < d ).map{ child =>
        exp( cpt(parent)(child) )
      }
    }.toArray
  }

  def keySet = cpt.keySet

  override def toString = cpt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    cpt(parent).keySet.toList.sortWith( (a,b) => a < b ).map{ ch =>
      parent + " --> " +ch + ":\t" + exp( cpt(parent)(ch) )
    }.mkString("\n\t","\n\t","")
  }.mkString("","\n","\n")
}

abstract class LogProbabilityDistribution[T<:Label] extends AbstractDistribution {
  //protected[this] var pt:HashMap[T,Double]
  var pt:HashMap[T,Double]

  def setPT( updatedPT: HashMap[T,Double] ) {
    pt = updatedPT
  }

  def *[U<:Label]( otherCPT: ConditionalLogProbabilityDistribution[T,U] ) =
    new ConditionalLogProbabilityDistribution[T,U] {
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

      // // UGLY UGLY HACK BAD JOHN
      // def *( otherPT: LogProbabilityDistribution[T] ) = {
      //   //println( keySet )
      //   //println( "=====" )
      //   val crossProductTypes = keySet.flatMap{ p1 =>
      //     otherPT.keySet.map{ p2 =>
      //       HiddenStatePair( p1.toString, p2.toString )
      //     }
      //   }
      // 
      //     val crossProduct = new LogProbabilityDistribution[HiddenStatePair] {
      //       var pt = HashMap(
      //         crossProductTypes.map( thisStateName =>
      //           thisStateName -> 1D/crossProductTypes.size
      //         ).toSeq: _*
      //       )
      //     }
      // 
      //     val newPT = HashMap(
      //       keySet.flatMap{ parent1 =>
      //         otherPT.keySet.map{ parent2 =>
      //           //println( HiddenStatePair( parent1.toString, parent2.toString ) )
      //           HiddenStatePair( parent1.toString, parent2.toString ) -> ( apply( parent1 ) + otherPT( parent2 ) )
      //         }
      //       }.toSeq:_*
      //     )
      //     crossProduct.setPT( newPT )
      //     crossProduct
      //   }

  def keySet = pt.keySet

  def *[U<:Label]( otherCPT: ConditionalProbabilityDistribution[T,U] ) =
    new ConditionalProbabilityDistribution[T,U] {
      var cpt = HashMap(
        otherCPT.keySet.map{ parent =>
          parent -> HashMap (
            otherCPT(parent).keySet.map{ child =>
              //child -> ( exp( pt( parent ) + log( otherCPT( parent )( child ) ) ) )
              child -> ( pt( parent ) + log( otherCPT( parent )( child ) ) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }

  //type U<:Label
  /*
  def *( otherPT: ProbabilityDistribution[HiddenState] ):ProbabilityDistribution[HiddenStatePair] = {
    val newPT = HashMap{
        keySet.flatMap{ x =>
          otherPT.domain.map{ y =>
            //HiddenStatePair(x.s, y) ->  apply(x) * otherPT(y)
            (y * x) ->  apply(x) * otherPT(y)
          }
        }.toSeq:_*
      }

    new ProbabilityDistribution[HiddenStatePair] {
      //assert( domain == otherPT.domain )
      var pt = newPT
    }
  }
  */

  def domain = pt.keySet

  // translated from nltk
  def log_add( ns:List[Double] ) = {
    val maxVal = ns.max
    if( maxVal > scala.Double.NegativeInfinity )
      maxVal + log( ns.foldLeft(0D)( (a,b) => a + exp( b - maxVal ) ) )
    else
      maxVal
  }

  def normalize {
    val max = log_add( pt.values.toList )

    //val singleSupport = pt.values.exists( _ == 0D )

    pt = HashMap(
      pt.keySet.map{ parent =>
        //if( singleSupport )
        //  if( pt(parent) == 0D )
        //    parent -> 0D
        //  else
        //    parent -> Double.NegativeInfinity
        //else
          parent -> ( pt(parent) - max )
      }.toSeq:_*
    )
  }

  // def normalize {
  //   val max = log_add( pt.values.toList )

  //   pt = HashMap(
  //     pt.keySet.map{ parent =>
  //       parent -> ( pt(parent) - max )
  //     }.toSeq:_*
  //   )
  // }

  val seed = 15
  def randomize( centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( log( r.nextDouble + centeredOn ) )
      }.toSeq:_*
    )

    normalize
  }

  def scale( n:Int ) {
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( pt(parent) + log( n ) )
      }.toSeq:_*
    )
  }

  def deScale( n:Int ) {
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( pt(parent) - log( n ) )
      }.toSeq:_*
    )
  }

  def toArray = pt.keySet.toList.sortWith( (a,b) => a < b ).map( k => exp( pt(k) ) ).toArray

  def apply( k:T ) = pt( k )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    parent + ":\t" + exp( pt(parent) )
  }.mkString("\n\t","\n\t","\n")
}


abstract class ProbabilityDistribution[T<:Label] extends AbstractDistribution {
  var pt:HashMap[T,Double]

  def setPT( updatedPT: HashMap[T,Double] ) {
    pt = updatedPT
  }

  def *[U<:Label]( otherCPT: ConditionalLogProbabilityDistribution[T,U] ) =
    new ConditionalLogProbabilityDistribution[T,U] {
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

  def *[U<:Label]( otherCPT: ConditionalProbabilityDistribution[T,U] ) =
    new ConditionalProbabilityDistribution[T,U] {
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

  //type U<:Label
  /*
  def *( otherPT: ProbabilityDistribution[HiddenState] ):ProbabilityDistribution[HiddenStatePair] = {
    val newPT = HashMap{
        keySet.flatMap{ x =>
          otherPT.domain.map{ y =>
            //HiddenStatePair(x.s, y) ->  apply(x) * otherPT(y)
            (y * x) ->  apply(x) * otherPT(y)
          }
        }.toSeq:_*
      }

    new ProbabilityDistribution[HiddenStatePair] {
      //assert( domain == otherPT.domain )
      var pt = newPT
    }
  }
  */

  def domain = pt.keySet

  def normalize {
    val max = pt.values.sum
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent -> pt(parent) / max
      }.toSeq:_*
    )
  }

  def scale(n:Int) {
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent -> pt(parent) * n
      }.toSeq:_*
    )
  }

  def deScale(n:Int) {
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent -> pt(parent) / n
      }.toSeq:_*
    )
  }

  val seed = 15
  def randomize( centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    pt = HashMap(
      pt.keySet.map{ parent =>
        parent ->  ( r.nextDouble + centeredOn )
      }.toSeq:_*
    )

    normalize
  }

  def toArray = pt.keySet.toList.sortWith( (a,b) => a < b ).map( pt(_) ).toArray

  def apply( k:T ) = pt( k )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    parent + ":\t" + pt(parent)
  }.mkString("\n\t","\n\t","\n")
}


abstract class PartialCounts

case class PlainHMMPartialCounts(
  stringProb: Double,
  stateCounts: HashMap[HiddenState,Double],
  transitionCounts: HashMap[HiddenState,HashMap[HiddenState,Double]],
  emissionCounts: HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends PartialCounts

case class CoupledHMMPartialCounts(
  stringProb: Double,
  //stateCounts: HashMap[HiddenState,Double],
  initialStateCounts: HashMap[HiddenStatePair,Double],
  //initialStateCountsB: HashMap[HiddenState,Double],
  transitionCountsA: HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
  transitionCountsB: HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
  emissionCountsA: HashMap[HiddenState,HashMap[ObservedState,Double]],
  emissionCountsB: HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends PartialCounts


abstract class Parameters

case class CoupledHMMParameters(
initialProbs:HashMap[HiddenStatePair,Double],
//initialProbsB:HashMap[HiddenState,Double],
transitionsA:HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
transitionsB:HashMap[HiddenStatePair,HashMap[HiddenState,Double]],
emissionsA:HashMap[HiddenState,HashMap[ObservedState,Double]],
emissionsB:HashMap[HiddenState,HashMap[ObservedState,Double]]
) extends Parameters



