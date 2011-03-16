package ProsodicParsing.HMMs

import ProsodicParsing.types._
// import scala.actors._
// import scala.actors.Actor
import akka.actor.Actor
import akka.actor.ActorRef

trait HMMMaster[Q<:HiddenLabel,O<:ObservedLabel] extends Actor {
  //var hmms:Array[HMMActor[Q,O]]
  //var hmms:List[HMMActor[Q,O]]
  var hmms:List[ActorRef]
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
  def randomize(seed:Int, centeredOn:Int):Unit

  var summingPartialCounts = initialPartialCounts

  def parameters:List[AbstractDistribution]
  def setParams( params:Parameters )
  def normalize:Unit

  def iterationStart {
    toSend = trainingData
    sent = Nil
    received = 0

    summingPartialCounts = initialPartialCounts

    //println( "At iteration start, we are:\n" + toString )

    hmms.foreach{ hmm =>
      hmm ! packageParameters
      val thisUtt = toSend.head
      toSend = toSend.tail
      hmm ! EstimateUtterance( thisUtt )
      sent = thisUtt :: sent
    }

      // var corpusPartitions = toSend.grouped( toSend.size/hmms.size  + 1 ).toList
      // hmms.foreach{ hmm =>
      //   hmm ! packageParameters
      //   val theseUtts = corpusPartitions.head
      //   corpusPartitions = corpusPartitions.tail
      //   hmm ! EstimateCorpus( theseUtts )
      //   sent = theseUtts ++ sent
      // }
  }

  def emEnd {
    exit()
  }

  def mapCounts( x:Double ) = x

  def iterationEnd {
    iterationCount += 1

    val corpusLogProb = summingPartialCounts.logProb

    setParams( summingPartialCounts.toTransformedParameters( mapCounts ) )
    normalize

    val deltaLogProb = (corpusLogProb - lastCorpusLogProb)/lastCorpusLogProb
    println( "iteration " + iterationCount + ": " + corpusLogProb+ " (" + deltaLogProb +")" )
    if( converged( iterationCount, deltaLogProb ) ) {
      println( "EM Done! final HMM:\n\n" + toString )
      hmms.foreach( _ ! Stop )
      emEnd
      //exit()
    } else {
      lastCorpusLogProb = corpusLogProb
      iterationStart
    }
  }

  def emInit = ()


  def receive = {
    case Initialize => {
      emInit
      toSend = trainingData
      received = 0


      hmms.foreach( _ ! Initialize )
      println( "We are:"+toString )
      iterationStart
    }

    case pc:PartialCounts => {
      summingPartialCounts = summingPartialCounts + pc
      received += 1
      if( received == sent.size && toSend.isEmpty ) {
        iterationEnd
      } else {
        if( ! toSend.isEmpty ) {
          if( (sent.size) % (trainingData.size/10) == 0 )
            println( "Sending utt " + (trainingData.size - toSend.size) + " of " + trainingData.size )
          val nextUtt = toSend.head
          toSend = toSend.tail
          self.reply( EstimateUtterance( nextUtt ) )
          sent = nextUtt :: sent
        }
      }
    }

    case Tuple2( numSentences:Int, pc:PartialCounts ) => {
      summingPartialCounts = summingPartialCounts + pc
      received += numSentences
      if( received == sent.size ) {
        iterationEnd
      }
    }

    case Stop => {
      hmms.foreach{_.stop}
      //exit
    }

    case Randomize( seed:Int, centeredOn:Int) => randomize( seed, centeredOn )

    case somethingElse:Any => println( "Manager got something else:\n" + somethingElse )
  }
}

trait VariationalBayesMaster[Q<:HiddenLabel,O<:ObservedLabel] extends HMMMaster[Q,O] {
  // This is the Digamma function ganked from Percy Liang
  override def mapCounts( input:Double ) = {
    assert( math.exp( input ) > 0, input )

    var r = 0D
    var x = math.exp( input )
    while( x <= 5 ) {
      r -= 1/x
      x += 1
    }
    val f = 1/(x*x)
    val t = f*(-1/12.0 + f*(1/120.0 + f*(-1/252.0 + f*(1/240.0 + f*(-1/132.0 + f*(691/32760.0 +
      f*(-1/12.0 + f*3617/8160.0)))))));
    r + math.log(x) - 0.5/x + t;
  }
}


trait EvaluatingMaster[Q<:HiddenLabel,O<:ObservedLabel] extends HMMMaster[Q,O] {
  //val viterbiHMM:HMMActor[Q,O]
  val viterbiHMM:ActorRef

  override def emInit {
    viterbiHMM.start()
    viterbiHMM ! packageParameters
    viterbiHMM ! Viterbi( 0, testSet )
  }

  val frequency:Int
  val testSet:List[ViterbiString]
  var iterationCount:Int
  //var hmms:List[HMMActor[Q,O]]
  //var hmms:List[ActorRef]

  var summingPartialCounts:PartialCounts
  def setParams( params:Parameters )
  def normalize:Unit
  def converged(iterationNum:Int, deltaLogProb:Double):Boolean
  def iterationStart:Unit

  def packageParameters:Parameters

  override def emEnd {
    viterbiHMM ! packageParameters
    viterbiHMM ! Viterbi( -1, testSet )
  }

  override def iterationEnd {
    iterationCount += 1

    setParams( summingPartialCounts.toTransformedParameters( mapCounts ) )
    normalize

    if( iterationCount % frequency == 0 ) {
      viterbiHMM ! packageParameters
      viterbiHMM ! Viterbi( iterationCount, testSet )
    }

    val corpusLogProb = summingPartialCounts.logProb

    val deltaLogProb = (corpusLogProb - lastCorpusLogProb)/lastCorpusLogProb
    println( "iteration " + iterationCount + ": " + corpusLogProb+ " (" + deltaLogProb +")" )
    if( converged( iterationCount, deltaLogProb ) ) {
      println( "EM Done! final HMM:\n\n" + toString )
      emEnd
    } else {
      lastCorpusLogProb = corpusLogProb
      iterationStart
    }
  }
}
