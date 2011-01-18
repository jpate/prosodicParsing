package ProsodicParsing.types
import scala.collection.immutable.HashMap


abstract class Label(s:String) {
  override def toString = s

  def <( l2:Label) = s < l2.toString
  def >( l2:Label) = s > l2.toString
}

case class HiddenState(s:String) extends Label(s)
case class ObservedState(s:String) extends Label(s)

abstract class ConditionalProbabilityDistribution[T<:Label,U<:Label] {
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

abstract class ProbabilityDistribution[T<:Label] {
  var pt:HashMap[T,Double]

  def setPT( updatedPT: HashMap[T,Double] ) {
    pt = updatedPT
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

  def normalize {
    // val maxes = HashMap(
    //   pt.keySet.map( parent => parent -> (cpt(parent)).values.sum ).toSeq:_*
    // )

    val max = pt.values.sum
    pt = HashMap(
      pt.keySet.map{ parent =>
        parent -> pt(parent) / max
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
  

  def apply( k:T ) = pt( k )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    parent + ":\t" + pt(parent)
  }.mkString("\n\t","\n\t","\n")
}

case class PartialCounts(
  stateCounts: HashMap[HiddenState,Double],
  transitionCounts: HashMap[HiddenState,HashMap[HiddenState,Double]],
  emissionCounts: HashMap[HiddenState,HashMap[ObservedState,Double]]
)

