package ProsodicParsing.HMMs

import ProsodicParsing.types._
//import scala.actors._
//import scala.actors.Actor
import akka.actor.Actor
import akka.actor.Actor._

trait HMMActor[Q<:HiddenLabel,O<:ObservedLabel] extends Actor {

  def setParams( params:Parameters ):Unit
  def computePartialCounts( s:List[O] ):PartialCounts
  def argmax( corpus:List[O] ):List[Q]
  def normalize:Unit
  def initialPartialCounts:PartialCounts
  def randomize(n:Int):Unit


  def receive = {
    case Initialize => {
      println( "Starting..." )
    }
    case parameters:Parameters => {
      println( "Got parameters" )
      setParams(parameters)
      normalize
    }
    case EstimateCorpus( corpus:List[List[O]] ) => {
      println( "Got a (sub?)corpus" )
      var summingPartialCounts:PartialCounts = initialPartialCounts
      var n = 0
      corpus.foreach{ s =>
        n = n + 1
        if ( n % 10 == 0 )
          println(
            "HMM " + self.uuid + " processing sentence " + n + " of " + corpus.size
          )
        summingPartialCounts = summingPartialCounts + computePartialCounts( s )
      }
      self.reply( Tuple2( corpus.size, summingPartialCounts ) )
    }
    case EstimateUtterance( utt:List[O] ) => {
      println( "Got an utterance: " + utt )
      self.reply( computePartialCounts(utt) )
    }
    case Viterbi( iteration, corpus ) => {
      println( "Got Viterbi" )
      corpus.foreach{ case ViterbiString(label, string:List[O] ) =>
        println( "it"+iteration + "," + label + "," + argmax( string ).mkString(""," ","") )
      }
    }
    case Stop => exit()
    case somethingElse:Any => println( "Slave got something else:\n" + somethingElse )
  }
}

case object Stop

