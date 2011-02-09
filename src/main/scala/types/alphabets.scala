
package prosodicParsing.types


abstract class Letter( s:String ) {
  def apply = s
  override def toString = s
}

abstract class AbstractHiddenState(s:String) extends Letter(s)
abstract class AbstractObservation(s:String) extends Letter(s)

case class HiddenState(s:String) extends AbstractHiddenState(s)
case class Observation(s:String) extends AbstractObservation(s)

abstract class Estimate
case class EstimateCorpus( s:List[List[AbstractObservation]] ) extends Estimate
case class EstimateUtterance( s:List[AbstractObservation] ) extends Estimate
case class Viterbi( s:List[AbstractObservation] ) extends Estimate


