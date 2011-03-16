package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.HMMs.HMMMaster
import ProsodicParsing.HMMs.HMMActor
import ProsodicParsing.HMMs.EvaluatingMaster
import ProsodicParsing.types._
import akka.actor.Actor.actorOf
import akka.actor.ActorRef
import collection.mutable.HashMap
import joptsimple.OptionParser;
import joptsimple.OptionSet;


object BaselineChunker {
  def main( args:Array[String]) {
    import scala.math.log

    val optsParser = new OptionParser("t:e:c:s:n:r:luv")

    val opts = optsParser.parse( args:_* )

    val dataPath = opts.valueOf( "t" ).toString
    val testDataPath = opts.valueOf( "e" ).toString
    val convergenceTolerance = opts.valueOf( "c" ).toString.toDouble
    val numHMMs = opts.valueOf("n").toString.toInt
    val whichStream = opts.valueOf( "s" ).toString.toInt
    val randSeed = if( opts.has( "r" ) ) opts.valueOf( "r" ).toString.toInt else 15
    val lambdaSmoothedEmissions = opts.has( "l" )
    val unkSmoothedEmissions = opts.has( "u" ) 
    val variationalBayes = opts.has( "v" ) 

    println( "dataPath: " + dataPath )
    println( "testDataPath: " +testDataPath )
    println( "convergenceTolerance: " + convergenceTolerance )
    println( "numHMMs: " + numHMMs )
    println( "whichStream: " + whichStream )
    println( "randSeed: " + randSeed )
    println( "lambdaSmoothedEmissions: " + lambdaSmoothedEmissions )
    println( "unkSmoothedEmissions: " + unkSmoothedEmissions )
    println( "variationalBayes: " + variationalBayes )

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


    val findRareWords = new HashMap[String,Int]{
      override def default( s:String ) = 0
    }
    var corpus = io.Source.fromFile( dataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
        tokenized.tail.map{ w =>
          val toUse = w.split( "#" )(whichStream)
          findRareWords( toUse ) += 1
          ObservedState( toUse )
        }
    }.filter{ s => s.size > 2 }//&& s.size < 20 }

    var unkTokens = 0
    corpus = corpus.map( s =>
      s.map{ case ObservedState( w )=>
        if( findRareWords( w ) == 1 ) {
          unkTokens += 1
          ObservedState( "UNK" )
        } else {
          ObservedState( w )
        }
      }
    )



    val observationTypes = Set( corpus.flatten).flatten.toSet.filter( _ match {
        case ObservedState(w) => findRareWords(w) > 1
      }
    ) + ObservedState( "UNK" )

    var unknownTokens = 0
    val testCorpus = io.Source.fromFile( testDataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
      ViterbiString(
        tokenized.head,
        tokenized.tail.map{ w =>
          val relevantToken = w.split( "#" )(whichStream)
          if( observationTypes.contains( ObservedState( relevantToken ) ) ) {
             ObservedState( relevantToken )
          } else {
            unknownTokens += 1
            ObservedState(  "UNK" )
          }
        }
      )
    }.filter{ s => s.size > 2 }//&& s.size < 25 }

    println( "random seed: " + randSeed )
    println( corpus.size + " training sentences" )
    println( testCorpus.size + " dev sentences" )
    println( unkTokens + " unk tokens in training set" )
    println( unknownTokens + " unk tokens in dev set" )

    //println( "\n\ntestCorpus is: " + testCorpus )

    //println( hiddenStates.size + " hidden states: " + hiddenStates.mkString("",", ",".") )

    // println( observationTypes.size + " observed types:\n" +
    // observationTypes.mkString("\t","\n\t","\n" ) )

    val manager = actorOf(
        new PlainHMM(chunkingStates,observationTypes,"Master")
        with EvaluatingMaster[HiddenState,ObservedState] {
        val trainingData = corpus

        override def mapCounts( input:Double ) = 
          if( variationalBayes ) {
            //assert( math.exp( input ) > 0, input )

            if( math.exp( input ) < 0 ) {
              Double.NegativeInfinity
            } else {
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
          } else {
            input
          }

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

        if( lambdaSmoothedEmissions ) {
          emissionMatrix =
            new LambdaSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates,
            observationTypes.toSet )
          emissionMatrix.randomize( randSeed, 10 )
        } else if( unkSmoothedEmissions ) {
          emissionMatrix =
            new UnkSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates,
            observationTypes.toSet )
          emissionMatrix.randomize( randSeed, 10 )
        }

        val frequency = 4
        val testSet = testCorpus

        def converged( iterations:Int, deltaLogProb:Double ) =
          ( math.abs( deltaLogProb ) < convergenceTolerance )
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




