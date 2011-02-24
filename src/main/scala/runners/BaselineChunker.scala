package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.HMMs.CoupledHMM
import ProsodicParsing.HMMs.HMMMaster
import ProsodicParsing.HMMs.HMMActor
import ProsodicParsing.HMMs.EvaluatingMaster
import ProsodicParsing.types._
import akka.actor.Actor.actorOf
import akka.actor.ActorRef


object BaselineChunker {
  def main( args:Array[String]) {
    import scala.math.log

    val dataPath = args(0)
    val testDataPath = args(1)
    val convergenceTolerance = args(2).toDouble
    val numHMMs = args(3).toInt
    val whichStream = args(4).toInt
    val randSeed = if(args.length > 5 ) args(5).toInt else 15

    //val obieCoding = Array( "O", "B", "I", "E" )
    val obieCoding = Array( "B", "E", "I", "O" )

    // val hiddenStates =
    //   obieCoding.flatMap{ obieCode =>
    //     (0 to (numProsodicStates-1)).map{ y =>
    //       HiddenStatePair( obieCode, "P_"+y )
    //     }
    //   }.toSet


    val chunkingStates = obieCoding.map{ HiddenState( _ ) }.toSet

    val transitionsToZero = chunkingStates.flatMap{ fromTransition =>
      fromTransition match {
        case HiddenState( "O" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "I" ) ),
            Tuple2( fromTransition, HiddenState( "E" ) )
          )
        case HiddenState( "B" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "O" ) ),
            Tuple2( fromTransition, HiddenState( "B" ) )
          )
        case HiddenState( "I" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "O" ) ),
            Tuple2( fromTransition, HiddenState( "B" ) )
          )
        case HiddenState( "E" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "I" ) ),
            Tuple2( fromTransition, HiddenState( "E" ) )
          )
      }
    }.toSet

    val initialStatesToZero = chunkingStates.filter{ hiddenState =>
      hiddenState match {
        case HiddenState( "I" ) => true
        case HiddenState( "E" ) => true
        case _ => false
      }
    }


    val chunkingTransitions =
      new ConditionalLogProbabilityDistribution( chunkingStates, chunkingStates )

    chunkingTransitions.randomize( randSeed, 10 )

    chunkingTransitions.zeroAll( transitionsToZero )

    chunkingTransitions.normalize


    val corpus = io.Source.fromFile( dataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
        tokenized.tail.map{ w =>
          ObservedState( w.split( "#" )(whichStream) )
        }
    }.filter{ s => s.size > 2 }//&& s.size < 20 }


    val testCorpus = io.Source.fromFile( testDataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
      ViterbiString(
        tokenized.head,
        tokenized.tail.map{ w =>
          ObservedState( w.split( "#" )(whichStream) )
        }
      )
    }.filter{ s => s.size > 2 }//&& s.size < 25 }

    println( "random seed: " + randSeed )
    println( corpus.size + " training sentences" )
    println( testCorpus.size + " dev sentences" )

    val observationTypes = Set( corpus.flatten).flatten.toSet

    //println( hiddenStates.size + " hidden states: " + hiddenStates.mkString("",", ",".") )

    // println( observationTypes.size + " observed types:\n" +
    // observationTypes.mkString("\t","\n\t","\n" ) )

    val manager = actorOf(
        new PlainHMM(chunkingStates,observationTypes,"Master") with HMMMaster[HiddenState,ObservedState]
        with EvaluatingMaster[HiddenState,ObservedState] {
        val trainingData = corpus

        println( "creating " + numHMMs + " HMMs")
        var hmms = (0 to (numHMMs-1)).map{ n =>
          actorOf( new PlainHMM( chunkingStates, observationTypes.toSet, n.toString )
            with HMMActor[HiddenState,ObservedState]).start
        }.toList
        println( "Made " + hmms.size + " HMMs")

        val viterbiHMM = actorOf( new PlainHMM( chunkingStates, observationTypes.toSet, "viterbi" ) with
        HMMActor[HiddenState,ObservedState] )

        randomize( randSeed, 10 )
        transitionMatrix.zeroAll( transitionsToZero )
        initialStateProbabilities.zeroAll( initialStatesToZero )


        val frequency = 4
        val testSet = testCorpus

        def converged( iterations:Int, deltaLogProb:Double ) =
          ( math.abs( deltaLogProb ) < convergenceTolerance && iterations > 30 )
      }
    )

    println( "Initially, we are: " + manager.toString )

    manager.start
    manager ! Initialize
    println( "Starting HMM: ")
    //println( Manager )
    manager.start()
  }
}




