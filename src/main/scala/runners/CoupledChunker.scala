package ProsodicParsing.runners
import ProsodicParsing.HMMs.CoupledHMM
import ProsodicParsing.HMMs.ZigZagCoupledHMM
import ProsodicParsing.HMMs.HMMMaster
import ProsodicParsing.HMMs.HMMActor
import ProsodicParsing.HMMs.EvaluatingMaster
import ProsodicParsing.types._
import cc.mallet.util.Maths
import akka.actor.Actor.actorOf
import akka.actor.ActorRef
import collection.mutable.HashMap
import joptsimple.OptionParser;
import joptsimple.OptionSet;


object CoupledChunker {
  def main( args:Array[String]) {
    import scala.math.log

    val optsParser = new OptionParser("t:e:c:p:n:r:lubdzv")
    optsParser.accepts( "bioCoding" )

    val opts = optsParser.parse( args:_* )

    val dataPath = opts.valueOf( "t" ).toString
    val testDataPath = opts.valueOf( "e" ).toString
    val convergenceTolerance = opts.valueOf( "c" ).toString.toDouble
    val numProsodicStates = opts.valueOf( "p").toString.toInt
    val numHMMs = opts.valueOf( "n" ).toString.toInt
    val randSeed = if( opts.has( "r" ) ) opts.valueOf( "r" ).toString.toInt else 15
    val lambdaSmoothedEmissions = opts.has( "l" )
    val unkSmoothedEmissions = opts.has( "u" )
    val smoothBoth = opts.has( "b" )
    val chunkBoth = opts.has( "d" )
    val zigzagChunker = opts.has( "z" )
    val bioCoding = opts.has( "bioCoding" )
    val variationalBayes = opts.has( "v" )
    //val randSeed = if(args.length > 5 ) args(5).toInt else 15

    println( "dataPath: " + dataPath )
    println( "testDataPath: " +testDataPath )
    println( "convergenceTolerance: " + convergenceTolerance )
    println( "numProsodicStates: " + numProsodicStates )
    println( "numHMMs: " + numHMMs )
    println( "randSeed: " + randSeed )
    println( "lambdaSmoothedEmissions: " + lambdaSmoothedEmissions )
    println( "unkSmoothedEmissions: " + unkSmoothedEmissions )
    println( "smoothBoth: " + smoothBoth )
    println( "chunkBoth: " + chunkBoth )
    println( "zigzagChunker: " + zigzagChunker )
    println( "bioCoding: " + bioCoding )
    println( "variationalBayes: " + variationalBayes )

    //val obieCoding = Array( "O", "B", "I", "E" )
    val chunksCoding =
      if( bioCoding )
        Array( "B", "I", "O" )
      else
        Array( "B", "E", "I", "O" )

    val hiddenStates =
      if( chunkBoth )
        chunksCoding.flatMap{ obieCode =>
          chunksCoding.map{ y =>
            HiddenStatePair( "C_"+obieCode, "P_"+y )
          }
        }.toSet
      else
        chunksCoding.flatMap{ obieCode =>
          (0 to (numProsodicStates-1)).map{ y =>
            HiddenStatePair( "C_"+obieCode, "P_"+y )
          }
        }.toSet


    val chunkingStates = chunksCoding.map{ HiddenState( _ ) }.toSet

    val transitionsToZero = hiddenStates.flatMap{ fromTransition =>
      fromTransition match {
        case HiddenStatePair( "C_O", _ ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_I" ) ),
            Tuple2( fromTransition, HiddenState( "C_E" ) )
          )
        case HiddenStatePair( "C_B", _ ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_O" ) ),
            Tuple2( fromTransition, HiddenState( "C_B" ) )
          )
        case HiddenStatePair( "C_I", _ ) =>
          if( bioCoding )
            List(
              Tuple2( fromTransition, HiddenState( "C_B" ) )
            )
          else
            List(
              Tuple2( fromTransition, HiddenState( "C_O" ) ),
              Tuple2( fromTransition, HiddenState( "C_B" ) )
            )
        case HiddenStatePair( "C_E", _ ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_I" ) ),
            Tuple2( fromTransition, HiddenState( "C_E" ) )
          )
      }
    }.toSet

    val prosodicTransitionsToZero =
      if( chunkBoth )
        hiddenStates.flatMap{ fromTransition =>
          fromTransition match {
            case HiddenStatePair( _, "P_O" ) =>
              List(
                Tuple2( fromTransition, HiddenState( "P_I" ) ),
                Tuple2( fromTransition, HiddenState( "P_E" ) )
              )
            case HiddenStatePair( _, "P_B" ) =>
              List(
                Tuple2( fromTransition, HiddenState( "P_O" ) ),
                Tuple2( fromTransition, HiddenState( "P_B" ) )
              )
            case HiddenStatePair( _, "P_I" ) =>
              List(
                Tuple2( fromTransition, HiddenState( "P_O" ) ),
                Tuple2( fromTransition, HiddenState( "P_B" ) )
              )
            case HiddenStatePair( _, "P_E" ) =>
              List(
                Tuple2( fromTransition, HiddenState( "P_I" ) ),
                Tuple2( fromTransition, HiddenState( "P_E" ) )
              )
          }
        }.toSet
      else
        Set[Tuple2[HiddenStatePair,HiddenState]]()


    val initialStatesToZero =
      ( Set( "E", "I" ) & chunksCoding.toSet ).map( q => HiddenState( "C_" + q ) )
    val prosodicInitialStatesToZero =
      ( Set( "E", "I" ) & chunksCoding.toSet ).map( q => HiddenState( "P_" + q ) )


    val chunkingTransitions =
      new ConditionalLogProbabilityDistribution( hiddenStates, chunkingStates )

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
        new CoupledHMM(hiddenStates,observationTypes,"Master")
          with EvaluatingMaster[HiddenStatePair,ObservedStatePair] {
        val trainingData = corpus

        println( "creating " + numHMMs + " HMMs")
        var hmms = (0 to (numHMMs-1)).map{ n =>
          actorOf(
            if( zigzagChunker )
              new ZigZagCoupledHMM( hiddenStates, observationTypes.toSet, n.toString )
                with HMMActor[HiddenStatePair,ObservedStatePair]
            else
              new CoupledHMM( hiddenStates, observationTypes.toSet, n.toString )
                with HMMActor[HiddenStatePair,ObservedStatePair]
          ).start
        }.toList
        println( "Made " + hmms.size + " HMMs")

        override def mapCounts( input:Double ) = 
          if( variationalBayes ) {
            if( input == Double.NegativeInfinity ) {
              Double.NegativeInfinity
            } else {
            //assert( math.exp( input ) > 0, input )

            // if( input == Double.NegativeInfinity ) {
            //   Double.NegativeInfinity
            // } else {
            //   // from Mark Johnson's insideoutside code
            //   var x = input;
            //   var result = Double.NegativeInfinity;
            //   while( x < 7 ) {
            //     result = Maths.sumLogProb( result, -1 * (math.log( 1 ) - x ) )
            //     x = Maths.sumLogProb( math.log(1) , x )
            //   }
            //   x = Maths.sumLogProb( x, -1* ( math.log(1D) - math.log(2D) ) )
            //   var xx = math.log( 1D ) - x
            //   var xx2 = xx + xx
            //   var xx4 = xx2 + xx2
            //   result = Maths.sumLogProb(
            //     Array(
            //       result,
            //       x,
            //       ( math.log(1) - math.log( 24 ) ) + xx2,
            //       -1 * ( math.log( 7D ) - math.log( 960D ) ) + xx4,
            //       ( math.log(31D) - math.log( 8064D ) ) + xx4 + xx2,
            //       -1 * ( math.log( 127D ) - math.log( 30720D ) ) + xx4 + xx4
            //     )
            //   )
            //   result
            // }
            if( input == Double.NegativeInfinity ) {
              Double.NegativeInfinity
            } else {
              // from percy liang
              var r = Double.NegativeInfinity
              // var r = 0D
              //var x = math.exp( input )
              var x = input

              while( x <= math.log( 5 ) ) {
                r = Maths.sumLogProb( r, -1 * ( math.log(1) - x ) )
                //r -= math.exp( math.log( 1D )-x );
                x = Maths.sumLogProb( x, math.log( 1 ) )
              }

              val f = math.log(1) - ( x + x )

              val t = 
                f + Maths.sumLogProb( Array( -1 * math.log( 1/12.0 ) ,
                  f + Maths.sumLogProb( Array( math.log( 1/120.0 ) ,
                    f + Maths.sumLogProb( Array( -1 * math.log( 1/252.0 ) ,
                      f + Maths.sumLogProb( Array( math.log( 1/240.0 ) ,
                        f + Maths.sumLogProb( Array( -1 * math.log( 1/132.0 ) ,
                          f + Maths.sumLogProb( Array( math.log( 691/32760.0 ) ,
                            f + Maths.sumLogProb( Array( -1 * math.log( 1/12.0 ) ,
                              f + math.log( 3617/8160.0 )))))))))))))))

              Maths.sumLogProb( Array( r , x , -1* ( math.log( 0.5 ) - x ), t ) )
            }
            }
              // //assert( math.exp( input ) > 0, input )

              // if( math.exp( input ) < 0 ) {
              //   Double.NegativeInfinity
              // } else {
              //   var r = 0D
              //   var x = math.exp( input )
              //   while( x <= 5 ) {
              //     r -= 1/x
              //     x += 1
              //   }
              //   val f = 1/(x*x)
              //   val t = f*(-1/12.0 + f*(1/120.0 + f*(-1/252.0 + f*(1/240.0 + f*(-1/132.0 + f*(691/32760.0 +
              //     f*(-1/12.0 + f*3617/8160.0)))))));
              //   r + math.log(x) - 0.5/x + t;
              // }
          } else {
            input
          }

        val viterbiHMM =
          actorOf(
            if( zigzagChunker )
              new ZigZagCoupledHMM( hiddenStates, observationTypes.toSet, "viterbi" )
                with HMMActor[HiddenStatePair,ObservedStatePair]
            else
              new CoupledHMM( hiddenStates, observationTypes.toSet, "viterbi" )
                with HMMActor[HiddenStatePair,ObservedStatePair]
          )

        randomize( randSeed, 10 )
        transitionMatrixA.zeroAll( transitionsToZero )
        initialStateProbabilitiesA.zeroAll( initialStatesToZero )

        if( chunkBoth ) {
          initialStateProbabilitiesB.zeroAll( prosodicInitialStatesToZero )
          transitionMatrixB.zeroAll( prosodicTransitionsToZero )
        }

        if( lambdaSmoothedEmissions ) {
          emissionMatrixA =
            new LambdaSmoothedConditionalLogProbabilityDistribution( 0.0001, hiddATypes.toSet, obsATypes )
          emissionMatrixA.randomize( randSeed, 10 )
          if( smoothBoth ) {
            emissionMatrixB =
              new LambdaSmoothedConditionalLogProbabilityDistribution( 0.0001, hiddBTypes.toSet, obsBTypes )
            emissionMatrixB.randomize( randSeed, 10 )
          }
        } else if( unkSmoothedEmissions ) {
          emissionMatrixA =
            new UnkSmoothedConditionalLogProbabilityDistribution( 0.0001, hiddATypes.toSet, obsATypes )
          emissionMatrixA.randomize( randSeed, 10 )
          if( smoothBoth ) {
            emissionMatrixB =
              new UnkSmoothedConditionalLogProbabilityDistribution( 0.0001, hiddBTypes.toSet, obsBTypes )
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



