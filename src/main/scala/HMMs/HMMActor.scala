package ProsodicParsing.HMMs

import ProsodicParsing.types._
import scala.actors._
import scala.actors.Actor

trait HMMActor[Q<:HiddenLabel,O<:ObservedLabel] extends Actor {

  def setParams( params:Parameters ):Unit
  def computePartialCounts( s:List[O] ):PartialCounts
  def argmax( corpus:List[List[O]] ):List[List[Q]]
  def normalize:Unit


  //type P<:Parameters
  def act() {
    println( "Starting..." )
    loop {
      react{
        case parameters:Parameters => {
          setParams(parameters)
          normalize
        }
        case EstimateCorpus( corpus:List[List[O]] ) => {
          //println( "Got a (sub?)corpus" )
          corpus.foreach( sender ! computePartialCounts(_) )
        }
        case EstimateUtterance( utt:List[O] ) => {
          //println( "Got an utterance: " + utt )
          reply( computePartialCounts(utt) )
        }
        case Viterbi( corpus:List[List[O]] ) => {
          reply( argmax( corpus ) )
        }
        case Stop => exit()
      }
    }
  }
}

case object Stop

