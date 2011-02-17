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
  def randomize(seed:Int,centeredOn:Int):Unit

  val hmmID:String

  def receive = {
    case Initialize => {
      println( "HMM " + hmmID + " starting..." )
    }
    case parameters:Parameters => {
      println( "Got parameters" )
      setParams(parameters)
      normalize
    }
    case EstimateCorpus( corpus:List[List[O]] ) => {
      println( "HMM " + hmmID + " got a (sub?)corpus with " + corpus.size + " utterances" )
      var summingPartialCounts:PartialCounts = initialPartialCounts
      var n = 0
      val numUtts = corpus.size
      corpus.foreach{ s =>
        n = n + 1
        if ( n % (numUtts/10) == 0 )
          println(
            "HMM " + hmmID + " processing sentence " + n + " of " + numUtts
          )
        summingPartialCounts = summingPartialCounts + computePartialCounts( s )
      }
      self.reply( Tuple2( corpus.size, summingPartialCounts ) )
    }
    case EstimateUtterance( utt:List[O] ) => {
      //println( "HMM " + hmmID + " got an utterance: " + utt )
      self.reply( computePartialCounts(utt) )
    }
    case Viterbi( iteration, corpus ) => {
      //println(  "HMM " + hmmID + " got Viterbi with " + corpus.size + " sentences" )
      corpus.foreach{ case ViterbiString(label, string:List[O] ) =>
        println( "it"+iteration + "," + label + "," + argmax( string ).mkString(""," ","") )
      }
      if( iteration == -1 ) {
        self.reply( Stop )
        exit
      }
    }
    case somethingElse:Any => println( "Slave HMM " + hmmID + " got something else:\n" + somethingElse )
  }
}


