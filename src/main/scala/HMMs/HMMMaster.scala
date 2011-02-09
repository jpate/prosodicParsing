package ProsodicParsing.HMMs

import ProsodicParsing.types._
import scala.actors._
import scala.actors.Actor

trait HMMMaster[Q<:HiddenLabel,O<:ObservedLabel] extends Actor {
  //var hmms:Array[HMMActor[Q,O]]
  var hmms:List[HMMActor[Q,O]]
  val trainingData: List[List[ObservedLabel]]

  def converged(iterationNum:Int, deltaLogProb:Double):Boolean

  def packageParameters:Parameters

  var iterationCount = 0

  var sent:List[List[ObservedLabel]] = Nil
  var received = 0
  var toSend:List[List[ObservedLabel]] = trainingData

  var lastCorpusLogProb = 0D
  var corpusLogProb = 0D

  def initialPartialCounts:PartialCounts

  var summingPartialCounts = initialPartialCounts

  var parameters:List[AbstractDistribution]
  def setParams( params:Parameters )
  def normalize:Unit

  def iterationStart {
    //println( "Starting iteration: " + iterationCount)
    //println( "At the start of this iteration, I am:\n" + toString )
    toSend = trainingData
    sent = Nil
    received = 0

    summingPartialCounts = initialPartialCounts

    //println ( "head slave hmm is:\n"+hmms.head+"\n\n\n" )

    hmms.foreach{ hmm =>
      //hmm ! parameters
      hmm ! packageParameters
      val thisUtt = toSend.head
      toSend = toSend.tail
      hmm ! EstimateUtterance( thisUtt )
      sent = thisUtt :: sent
    }
    //println ( "head slave hmm is now:\n"+hmms.head+"\n\n\n" )
  }

  def iterationEnd {
    iterationCount += 1

    val corpusLogProb = summingPartialCounts.logProb

    //println( "Final summingPartialCounts are : " + summingPartialCounts )
    setParams( summingPartialCounts.toParameters )
    normalize

    //println( "At the end of this iteration, I am:\n" + toString )

    //hmms.foreach( _ ! summingPartialCounts.toParameters )

    val deltaLogProb = (corpusLogProb - lastCorpusLogProb)/lastCorpusLogProb
    println( "iteration " + iterationCount + ": " + corpusLogProb+ " (" + deltaLogProb +")" )
    if( converged( iterationCount, deltaLogProb ) ) {
      println( "EM Done! final HMM:\n\n" + toString )
      hmms.foreach( _ ! Stop )
      exit()
    } else {
      lastCorpusLogProb = corpusLogProb
      iterationStart
    }
  }

  def act() {
    toSend = trainingData
    //var sent:List[List[ObservedLabel]] = Nil
    received = 0

    hmms.foreach(_.start)
    iterationStart

    loop{
      react {
        case pc:PartialCounts => {
          summingPartialCounts = summingPartialCounts + pc
          //println( "Partial Counts are " + pc )
          //println( "total partial counts so far are: " +summingPartialCounts + "\n" )
          received += 1
          if( received == sent.size && toSend.isEmpty ) {
            iterationEnd
          } else {
            if( ! toSend.isEmpty ) {
              val nextUtt = toSend.head
              toSend = toSend.tail
              reply( EstimateUtterance( nextUtt ) )
              //println( "sent had " + sent.size + " sentences" )
              sent = nextUtt :: sent
              //println( "sent now has  " + sent.size + " sentences" )
            }
          }
        }
      }
    }
  }
}

