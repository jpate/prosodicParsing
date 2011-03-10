package ProsodicParsing.runners
import ProsodicParsing.HMMs.TwoOutputHMM
import ProsodicParsing.HMMs.HMMMaster
import ProsodicParsing.HMMs.HMMActor
import ProsodicParsing.HMMs.EvaluatingMaster
import ProsodicParsing.types._
import akka.actor.Actor.actorOf
import akka.actor.ActorRef
import collection.mutable.HashMap
import joptsimple.OptionParser;
import joptsimple.OptionSet;


object TwoOutputChunker {
  def main( args:Array[String]) {
    import scala.math.log

    val optsParser = new OptionParser("t:e:c:n:r:lub")

    val opts = optsParser.parse( args:_* )

    val dataPath = opts.valueOf( "t" ).toString
    val testDataPath = opts.valueOf( "e" ).toString
    val convergenceTolerance = opts.valueOf( "c" ).toString.toDouble
    val numHMMs = opts.valueOf( "n" ).toString.toInt
    val randSeed = if( opts.has( "r" ) ) opts.valueOf( "r" ).toString.toInt else 15
    val lambdaSmoothedEmissions = opts.has( "l" )
    val unkSmoothedEmissions = opts.has( "u" )
    val smoothBoth = opts.has( "b" )
    //val randSeed = if(args.length > 5 ) args(5).toInt else 15

    println( "dataPath: " + dataPath )
    println( "testDataPath: " +testDataPath )
    println( "convergenceTolerance: " + convergenceTolerance )
    println( "numHMMs: " + numHMMs )
    println( "randSeed: " + randSeed )
    println( "lambdaSmoothedEmissions: " + lambdaSmoothedEmissions )
    println( "unkSmoothedEmissions: " + unkSmoothedEmissions )
    println( "smoothBoth: " + smoothBoth )

    //val obieCoding = Array( "O", "B", "I", "E" )
    val obieCoding = Array( "B", "E", "I", "O" )


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

    val initialStatesToZero =
      Set( "E", "I" ).map( HiddenState( _ ) )


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
          val Array( word, prosody ) = w.split( "#" )
          findRareWords( word ) += 1
          ObservedStatePair( word, prosody )
        }
    }.filter{ s => s.size > 2 }//&& s.size < 20 }

    var observationTypes = Set( corpus.flatten).flatten.toSet

    var unkTokens = 0
    corpus = corpus.map( s =>
      s.map{ case ObservedStatePair( w, p )=>
        if( findRareWords( w ) == 1 ) {
          unkTokens += 1
          if( smoothBoth )
            ObservedStatePair( "UNK", "UNK" )
          else
            ObservedStatePair( "UNK", p )
        } else {
          ObservedStatePair( w, p )
        }
      }
    )


    observationTypes = observationTypes.filter{ case ObservedStatePair( w, _ ) =>
      findRareWords( w ) > 1
    } ++ observationTypes.map{
      case ObservedStatePair( _,p ) => p
    }.map{ p => if(smoothBoth) ObservedStatePair( "UNK","UNK") else ObservedStatePair( "UNK", p ) }

    //println( "\n\nobservationTypes: " + observationTypes.mkString("",", ","\n\n" ) );

    var unknownTokens = 0
    val testCorpus = io.Source.fromFile( testDataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
      ViterbiString(
        tokenized.head,
        tokenized.tail.map{ w =>
          val Array( word, prosody ) = w.split( "#" )
          if( observationTypes.exists{ _.obs1 == word } ) {
            ObservedStatePair( word, prosody )
          } else {
            unknownTokens += 1
            if( smoothBoth )
              ObservedStatePair( "UNK", "UNK" )
            else
              ObservedStatePair( "UNK", prosody )
          }
        }
      )
    }.filter{ s => s.size > 2 }//&& s.size < 25 }


    println( "random seed: " + randSeed )
    println( corpus.size + " training sentences" )
    println( testCorpus.size + " dev sentences" )
    println( unkTokens + " unk tokens in training set" )
    println( unknownTokens + " unkn tokens in dev set" )


    //println( "\n\ntestCorpus is: " + testCorpus )


    //println( hiddenStates.size + " hidden states: " + hiddenStates.mkString("",", ",".") )

    // println( observationTypes.size + " observed types:\n" +
    // observationTypes.mkString("\t","\n\t","\n" ) )

    val manager = actorOf(
        new TwoOutputHMM(chunkingStates,observationTypes,"Master")
        with HMMMaster[HiddenState,ObservedStatePair]
        with EvaluatingMaster[HiddenState,ObservedStatePair] {
        val trainingData = corpus

        println( "creating " + numHMMs + " HMMs")
        var hmms = (0 to (numHMMs-1)).map{ n =>
          actorOf( new TwoOutputHMM( chunkingStates, observationTypes.toSet, n.toString )
            with HMMActor[HiddenState,ObservedStatePair]).start
        }.toList
        println( "Made " + hmms.size + " HMMs")

        val viterbiHMM = actorOf( new TwoOutputHMM( chunkingStates, observationTypes.toSet, "viterbi" ) with
        HMMActor[HiddenState,ObservedStatePair] )

        randomize( randSeed, 10 )
        transitionMatrix.zeroAll( transitionsToZero )
        initialStateProbabilities.zeroAll( initialStatesToZero )

        if( lambdaSmoothedEmissions ) {
          emissionMatrixA =
            new LambdaSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates.toSet, obsATypes )
          emissionMatrixA.randomize( randSeed, 10 )
          if( smoothBoth ) {
            emissionMatrixB =
              new LambdaSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates.toSet, obsBTypes )
            emissionMatrixB.randomize( randSeed, 10 )
          }
        } else if( unkSmoothedEmissions ) {
          emissionMatrixA =
            new UnkSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates.toSet, obsATypes )
          emissionMatrixA.randomize( randSeed, 10 )
          if( smoothBoth ) {
            emissionMatrixB =
              new UnkSmoothedConditionalLogProbabilityDistribution( 0.0001, chunkingStates.toSet, obsBTypes )
            emissionMatrixB.randomize( randSeed, 10 )
          }
        }

        val frequency = 4
        val testSet = testCorpus

        def converged( iterations:Int, deltaLogProb:Double ) =
          ( math.abs( deltaLogProb ) < convergenceTolerance )
      }
    )


    manager.start
    manager ! Initialize
    manager.start()
  }
}



