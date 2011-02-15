package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.HMMs.CoupledHMM
import ProsodicParsing.HMMs.HMMMaster
import ProsodicParsing.HMMs.HMMActor
import ProsodicParsing.HMMs.EvaluatingMaster
import ProsodicParsing.types._
import akka.actor.Actor.actorOf
import akka.actor.ActorRef


object CoupledRunner {
  def main( args:Array[String]) {
    import scala.math.log

    val dataPath = args(0)
    val testDataPath = args(1)
    val numHiddenStates = args(2).toInt
    val numHMMs = args(3).toInt
    val randSeed = if(args.length > 4 ) args(4).toInt else 15

    // val hiddenStates =
    //   (0 to (numHiddenStates-1)).flatMap{ i =>
    //     Set( "S_"+i, "S_"+i, "S_"+i ).flatMap{ x =>
    //       Set( "P_"+i,"P_"+i,"P_"+i ).map{ y =>
    //         HiddenStatePair( x, y )
    //       }
    //     }
    //   }.toSet
    val hiddenStates =
      (0 to (numHiddenStates-1)).flatMap{ x =>
        (0 to (numHiddenStates-1)).map{ y =>
          HiddenStatePair( "S_"+x, "P_"+y )
        }
      }.toSet


    val corpus = io.Source.fromFile( dataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
        tokenized.tail.map{ w =>
          val Array( word, prosody ) = w.split( "#" )
          ObservedStatePair( word, prosody )
        }
    }.filter{ s => s.size > 2 }//&& s.size < 20 }

    println( corpus.size + " training sentences" )

    val testCorpus = io.Source.fromFile( testDataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
      ViterbiString(
        tokenized.head,
        tokenized.tail.map{ w =>
          val Array( word, prosody ) = w.split( "#" )
          ObservedStatePair( word, prosody )
        }
      )
    }.filter{ s => s.size > 2 }//&& s.size < 25 }

    println( testCorpus.size + " dev sentences" )

    val observationTypes = Set( corpus.flatten).flatten.toSet

    println( hiddenStates.size + " hidden states: " + hiddenStates.mkString("",", ",".") )

    println( observationTypes.size + " observed types:\n" +
    observationTypes.mkString("\t","\n\t","\n" ) )

    val manager = actorOf(
        new CoupledHMM(hiddenStates,observationTypes,"Master") with HMMMaster[HiddenStatePair,ObservedStatePair]
        with EvaluatingMaster[HiddenStatePair,ObservedStatePair] {
        val trainingData = corpus

        println( "creating " + numHMMs + " HMMs")
        var hmms = (0 to (numHMMs-1)).map{ n =>
          actorOf( new CoupledHMM( hiddenStates, observationTypes.toSet, n.toString )
            with HMMActor[HiddenStatePair,ObservedStatePair]).start
        }.toList
        println( "Made " + hmms.size + " HMMs")

        val viterbiHMM = actorOf( new CoupledHMM( hiddenStates, observationTypes.toSet, "viterbi" ) with
        HMMActor[HiddenStatePair,ObservedStatePair] )

        val frequency = 4
        val testSet = testCorpus

        def converged( iterations:Int, deltaLogProb:Double ) =
          iterations > 100 || ( math.abs( deltaLogProb ) < 0.01 && iterations > 15 )
      }
    )

    manager.start
    manager ! Randomize(randSeed,10)
    manager ! Initialize
    println( "Starting HMM: ")
    //println( Manager )
    manager.start()
  }
}


