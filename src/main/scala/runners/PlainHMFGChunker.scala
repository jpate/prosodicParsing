package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMFG
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


object PlainHMFGChunker {
  def main( args:Array[String]) {

    val optsParser = new OptionParser("t:e:c:s:n:r:luva")
    optsParser.accepts( "unkCutoff" ).withRequiredArg()

    val opts = optsParser.parse( args:_* )

    val dataPath = opts.valueOf( "t" ).toString
    val testDataPath = opts.valueOf( "e" ).toString
    val convergenceTolerance = opts.valueOf( "c" ).toString.toDouble
    val numHMFGs = opts.valueOf("n").toString.toInt
    val randSeed = if( opts.has( "r" ) ) opts.valueOf( "r" ).toString.toInt else 15
    val lambdaSmoothedEmissions = opts.has( "l" )
    val unkSmoothedEmissions = opts.has( "u" ) 
    val variationalBayes = opts.has( "v" ) 
    val useAllStreams = opts.has( "a" ) 
    val whichStream = if( useAllStreams ) 0 else opts.valueOf( "s" ).toString.toInt
    val unkCutoff = if( opts.has( "unkCutoff" ) ) opts.valueOf( "unkCutoff" ).toString.toInt else 1

    println( "dataPath: " + dataPath )
    println( "testDataPath: " +testDataPath )
    println( "convergenceTolerance: " + convergenceTolerance )
    println( "numHMFGs: " + numHMFGs )
    println( "whichStream: " + whichStream )
    println( "randSeed: " + randSeed )
    println( "lambdaSmoothedEmissions: " + lambdaSmoothedEmissions )
    println( "unkSmoothedEmissions: " + unkSmoothedEmissions )
    println( "variationalBayes: " + variationalBayes )
    println( "useAllStreams: " + useAllStreams )
    println( "unkCutoff: " + unkCutoff )

    //val obieCoding = Array( "O", "B", "I", "E" )
    val obieCoding = Array( "C_B", "C_E", "C_I", "C_O" )

    // val hiddenStates =
    //   obieCoding.flatMap{ obieCode =>
    //     (0 to (numProsodicStates-1)).map{ y =>
    //       HiddenStatePair( obieCode, "P_"+y )
    //     }
    //   }.toSet


    val chunkingStates = obieCoding.map{ HiddenState( _ ) }.toSet

    val transitionsToZero = chunkingStates.flatMap{ fromTransition =>
      fromTransition match {
        case HiddenState( "C_O" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_I" ) ),
            Tuple2( fromTransition, HiddenState( "C_E" ) )
          )
        case HiddenState( "C_B" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_O" ) ),
            Tuple2( fromTransition, HiddenState( "C_B" ) )
          )
        case HiddenState( "C_I" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_O" ) ),
            Tuple2( fromTransition, HiddenState( "C_B" ) )
          )
        case HiddenState( "C_E" ) =>
          List(
            Tuple2( fromTransition, HiddenState( "C_I" ) ),
            Tuple2( fromTransition, HiddenState( "C_E" ) )
          )
      }
    }.toSet

    val initialStatesToZero = chunkingStates.filter{ hiddenState =>
      hiddenState match {
        case HiddenState( "C_I" ) => true
        case HiddenState( "C_E" ) => true
        case _ => false
      }
    }

    val finalStatesToZero = chunkingStates.filter{ hiddenState =>
      hiddenState match {
        case HiddenState( "C_B" ) => true
        case HiddenState( "C_I" ) => true
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
      if( useAllStreams ) {
        tokenized.tail.map{ w =>
          findRareWords( w ) += 1
          ObservedState( w )
        }
      } else {
        tokenized.tail.map{ w =>
          val toUse = w.split( "#" )(whichStream)
          findRareWords( toUse ) += 1
          ObservedState( toUse )
        }
      }
    }.filter{ s => s.size > 2 }//&& s.size < 20 }

    var unkTokens = 0
    corpus = corpus.map( s =>
      s.map{ case ObservedState( w )=>
        if( findRareWords( w ) <= unkCutoff ) {
          unkTokens += 1
          ObservedState( "UNK" )
        } else {
          ObservedState( w )
        }
      }
    )



    val observationTypes = Set( corpus.flatten).flatten.toSet.filter( _ match {
        case ObservedState(w) => findRareWords(w) > unkCutoff
      }
    ) + ObservedState( "UNK" )

    var unknownTokens = 0
    val testCorpus = io.Source.fromFile( testDataPath ).getLines().toList.map{ rawString =>
      val tokenized = rawString.split(" ").toList
      ViterbiString(
        tokenized.head,
        tokenized.tail.map{ w =>
          val relevantToken = if( useAllStreams ) w else  w.split( "#" )(whichStream)
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
        new PlainHMFG(chunkingStates,observationTypes,"Master")
        with EvaluatingMaster[HiddenState,ObservedState] {
        val trainingData = corpus

        // override def mapCounts( input:Double ) = 
        //   if( variationalBayes ) {
        //     //assert( math.exp( input ) > 0, input )

        //     if( math.exp( input ) < 0 ) {
        //       Double.NegativeInfinity
        //     } else {
        //       var r = 0D
        //       var x = math.exp( input )
        //       while( x <= 5 ) {
        //         r -= 1/x
        //         x += 1
        //       }
        //       val f = 1/(x*x)
        //       val t = f*(-1/12.0 + f*(1/120.0 + f*(-1/252.0 + f*(1/240.0 + f*(-1/132.0 + f*(691/32760.0 +
        //         f*(-1/12.0 + f*3617/8160.0)))))));
        //       r + math.log(x) - 0.5/x + t;
        //     }
        //   } else {
        //     input
        //   }

        override def mapCounts( input:Double ) = 
          if( variationalBayes ) {
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
              var x = input + 0.1

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
          } else {
            input
          }

        println( "creating " + numHMFGs + " HMFGs")
        var hmms = (0 to (numHMFGs-1)).map{ n =>
          actorOf( new PlainHMFG( chunkingStates, observationTypes.toSet, n.toString )
            with HMMActor[HiddenState,ObservedState]).start
        }.toList
        println( "Made " + hmms.size + " HMFGs")

        val viterbiHMM = actorOf( new PlainHMFG( chunkingStates, observationTypes.toSet, "viterbi" ) with
        HMMActor[HiddenState,ObservedState] )

        randomize( randSeed, 10 )
        transitionMatrix.zeroAll( transitionsToZero )
        initialStateProbabilities.zeroAll( initialStatesToZero )
        finalStateProbabilities.zeroAll( finalStatesToZero )

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
    println( "Starting HMFG: ")
    //println( Manager )
    manager.start()
  }
}




