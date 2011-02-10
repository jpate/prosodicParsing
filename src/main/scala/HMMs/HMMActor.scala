package ProsodicParsing.HMMs

import ProsodicParsing.types._
import scala.actors._
import scala.actors.Actor

trait HMMActor[Q<:HiddenLabel,O<:ObservedLabel] extends Actor {

  def setParams( params:Parameters ):Unit
  def computePartialCounts( s:List[O] ):PartialCounts
  def argmax( corpus:List[O] ):List[Q]
  def normalize:Unit
  def initialPartialCounts:PartialCounts


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
          println( "Got a (sub?)corpus" )
          //corpus.foreach( sender ! computePartialCounts(_) )
          //reply( Tuple2( corpus.size, corpus.map( computePartialCounts(_) ).reduceLeft(_+_) ) )
          var summingPartialCounts:PartialCounts = initialPartialCounts
          corpus.foreach{ s =>
            summingPartialCounts = summingPartialCounts + computePartialCounts( s )
          }
          reply( Tuple2( corpus.size, summingPartialCounts ) )
        }
        case EstimateUtterance( utt:List[O] ) => {
          println( "Got an utterance: " + utt )
          reply( computePartialCounts(utt) )
        }
        case Viterbi( iteration, corpus ) => {
          corpus.foreach{ case ViterbiString(label, string:List[O] ) =>
            //val ViterbiString( label, string ) = vit
            println( "it"+iteration + "," + label + "," + argmax( string ).mkString(""," ","") )
          }
          //println( argmax(corpus).map{_.mkString(""," ","")} )
          //reply( argmax( corpus ) )
        }
        case Stop => exit()
        case somethingElse:Any => println( "Slave got something else:\n" + somethingElse )
      }
    }
  }
}

case object Stop

