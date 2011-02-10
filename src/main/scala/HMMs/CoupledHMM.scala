package ProsodicParsing.HMMs

import ProsodicParsing.types._
import ProsodicParsing.util.Util
import cc.mallet.types.LabelAlphabet
import collection.immutable.HashMap
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import collection.mutable.{HashMap => MHashMap}


class CoupledHMM(
  hiddenStateTypesSet:Set[HiddenStatePair],
  observationTypesSet:Set[ObservedStatePair]
) extends AbstractHMM[HiddenStatePair,ObservedStatePair] (hiddenStateTypesSet,observationTypesSet) {
  import math.{log,exp}

  val Tuple2( obsATypes, obsBTypes ) =
    observationTypesSet.map{ case ObservedStatePair( obs1, obs2 ) =>
      Tuple2( ObservedState( obs1 ), ObservedState( obs2 ) )
    }.foldLeft(Tuple2[Set[ObservedState],Set[ObservedState]](Set(),Set())){ (a,b) =>
      val Tuple2( obs1s, obs2s ) = a
      val Tuple2( obs1, obs2 ) = b
      Tuple2( obs1s + obs1, obs2s + obs2 )
    }

  val Tuple2( hiddATypes, hiddBTypes ) =
    hiddenStateTypesSet.map{ case HiddenStatePair( hidd1, hidd2 ) =>
      Tuple2( HiddenState( hidd1 ), HiddenState( hidd2 ) )
    }.foldLeft(Tuple2[Set[HiddenState],Set[HiddenState]](Set(),Set())){ (a,b) =>
      val Tuple2( hidd1s, hidd2s ) = a
      val Tuple2( hidd1, hidd2 ) = b
      Tuple2( hidd1s + hidd1, hidd2s + hidd2 )
    }

  val obsAlphA = new LabelAlphabet()
  val obsAlphB = new LabelAlphabet()
  obsATypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphA.lookupIndex( _,true ) )
  obsBTypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphB.lookupIndex( _,true ) )

  val hiddAlphA = new LabelAlphabet()
  val hiddAlphB = new LabelAlphabet()
  val hiddAIndexToLabel = new MHashMap[Int,HiddenState]
  val hiddBIndexToLabel = new MHashMap[Int,HiddenState]
  hiddATypes.toList.sortWith( (a,b) => a < b ).foreach( qA =>
    hiddAIndexToLabel( hiddAlphA.lookupIndex( qA , true ) ) = qA
  )
  hiddBTypes.toList.sortWith( (a,b) => a < b ).foreach( qB =>
    hiddBIndexToLabel( hiddAlphB.lookupIndex( qB, true ) ) = qB
  )

  def assignmentToViterbiString( maxAssn:Assignment ) =
    (0 to ( stringLength -1 ) ).map{ t =>
      val hiddA = hiddAIndexToLabel( maxAssn.get( hiddenVariables( t ) ) )
      val hiddB =
        hiddBIndexToLabel( maxAssn.get( hiddenVariables( stringLength + t ) ) )

      HiddenStatePair(
        hiddA toString,
        hiddB toString
      )
    }.toList


  //def hiddenIndexToLabel( index:Int ) = HiddenStatePair("what","whoa")


  /*
  object InitialStateProbabilitiesA extends LogProbabilityDistribution[HiddenState] {
    var pt = HashMap(
      hiddATypes.map( thisStateName =>
        thisStateName -> 1D/hiddATypes.size
      ).toSeq: _*
    )
  }

  object InitialStateProbabilitiesB extends LogProbabilityDistribution[HiddenState] {
    var pt = HashMap(
      hiddBTypes.map( thisStateName =>
        thisStateName -> 1D/hiddBTypes.size
      ).toSeq: _*
    )
  }
  */

  object TransitionMatrixA
    extends ConditionalLogProbabilityDistribution[HiddenStatePair, HiddenState] {
    // For now we'll initialize to a uniform transition matrix and define a
    // randomize method for people to have a random initialization whenever they
    // like
    var cpt = HashMap(
      hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              hiddATypes.map( toStateName =>
                toStateName -> log( 1D/hiddATypes.size )
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object TransitionMatrixB
    extends ConditionalLogProbabilityDistribution[HiddenStatePair, HiddenState] {
    // For now we'll initialize to a uniform transition matrix and define a
    // randomize method for people to have a random initialization whenever they
    // like
    var cpt = HashMap(
      hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              hiddBTypes.map( toStateName =>
                toStateName -> log( 1D/hiddBTypes.size )
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object EmissionMatrixA extends ConditionalLogProbabilityDistribution[HiddenState,ObservedState] {
    var cpt = HashMap(
      hiddATypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              obsATypes.map( toStateName =>
                toStateName -> log( 1D/obsATypes.size )
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object EmissionMatrixB extends ConditionalLogProbabilityDistribution[HiddenState,ObservedState] {
    var cpt = HashMap(
      hiddBTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              obsBTypes.map( toStateName =>
                toStateName -> log( 1D/obsBTypes.size )
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object InitialStateProbabilities extends LogProbabilityDistribution[HiddenStatePair] {
    var pt = HashMap(
      hiddenStateTypes.map( thisStateName =>
        thisStateName -> log( 1D/ numHiddenStates )
      ).toSeq: _*
    )
  }

  var parameters = List(
    InitialStateProbabilities,
    TransitionMatrixA,
    TransitionMatrixB,
    EmissionMatrixA,
    EmissionMatrixB
  )

  def packageParameters = CoupledHMMParameters(
    InitialStateProbabilities.pt,
    TransitionMatrixA.cpt,
    TransitionMatrixB.cpt,
    EmissionMatrixA.cpt,
    EmissionMatrixB.cpt
  )

        // var matrices = Set[AbstractDistribution](
        //   TransitionMatrixA,
        //   TransitionMatrixB,
        //   EmissionMatrixA,
        //   EmissionMatrixB,
        //   InitialStateProbabilities
        //   //InitialStateProbabilitiesA,
        //   //InitialStateProbabilitiesB
        // )

  def initialPartialCounts = CoupledHMMPartialCounts(
      0D,
      HashMap(
        hiddenStateTypes.map( thisStateName =>
          thisStateName -> Double.NegativeInfinity
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              hiddATypes.map( toStateName =>
                toStateName -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( qsFrom =>
          qsFrom -> (
            HashMap(
              hiddBTypes.map( toStateName =>
                toStateName -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddATypes.map( qsFrom =>
          qsFrom -> (
            HashMap(
              obsATypes.map( obs =>
                obs -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddBTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              obsBTypes.map( obs =>
                obs -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
    )


  def setParams( newParams:Parameters ) {
    val CoupledHMMParameters(
      initialProbs,
      transitionsA,
      transitionsB,
      emissionsA,
      emissionsB
    ) = newParams

    InitialStateProbabilities.setPT( initialProbs )
    TransitionMatrixA.setCPT( transitionsA )
    TransitionMatrixB.setCPT( transitionsB )
    EmissionMatrixA.setCPT( emissionsA )
    EmissionMatrixB.setCPT( emissionsB )
  }

  def buildHMM( tokens: List[ObservedStatePair] ) {
    hmm = new DynamicBayesNet( tokens.size )

    stringLength = tokens.size

    val hiddenVarA = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphA.size ) )
    val hiddenVarB = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphB.size ) )

    hiddenVariables = hiddenVarA ++ hiddenVarB

    val obsVarA = Array.tabulate( tokens.size )( _ => new Variable( obsAlphA.size ) )
    val obsVarB = Array.tabulate( tokens.size )( _ => new Variable( obsAlphB.size ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVarA( i ).setLabel("hidden.A"+i)
      hiddenVarB( i ).setLabel("hidden.B"+i)
      obsVarA( i ).setLabel("observation.A"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }


          // // initial states:
          // hmm.addFactor(
          //   new CPT(
          //     new TableFactor(
          //       Array( hiddenVariables(0) , hiddenVariables(1) ),
          //       (InitialStateProbabilitiesA * TransitionMatrixA).toArray
          //     ),
          //     hiddenVariables(1)
          //   )
          // )
          // hmm.addFactor(
          //   new CPT(
          //     new TableFactor(
          //       Array( hiddenVariables(tokens.size) , hiddenVariables(tokens.size+1) ),
          //       (InitialStateProbabilitiesB * TransitionMatrixB).toArray
          //     ),
          //     hiddenVariables(tokens.size+1)
          //   )
          // )
          // state transitions:

    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarA(1) ),
          ( InitialStateProbabilities * TransitionMatrixA).toArray
          //( ( InitialStateProbabilitiesA * InitialStateProbabilitiesB ) * TransitionMatrixA).toArray
        ),
        hiddenVarA(1)
      ),
      0
    )
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarB(1) ),
          (InitialStateProbabilities * TransitionMatrixB).toArray
          //( InitialStateProbabilitiesA * InitialStateProbabilitiesB * TransitionMatrixA).toArray
        ),
        hiddenVarB(1)
      ),
      0
    )


    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarA(i) ),
            TransitionMatrixA.toArray
          ),
        hiddenVarA(i)
        ),
        i-1
      )
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarB(i) ),
            TransitionMatrixB.toArray
          ),
          hiddenVarB(i)
        ),
        i-1
      )
    }

    // emissions
    ( 0 to tokens.size-1) foreach{ i =>
      //val ObservedStatePair( obsA, obsB ) = tokens(i)
      hmm.addObservedTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i), obsVarA(i) ),
            EmissionMatrixA.toArray
          ),
          obsVarA(i)
        ),
        i
      )
      hmm.addObservedTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarB(i), obsVarB(i) ),
            EmissionMatrixB.toArray
          ),
          obsVarB(i)
        ),
        i
      )
    }
  }

      // def buildHMM( tokens: List[ObservedStatePair] ) {
      //   hmm = new DirectedModel()

      //   val hiddenVarA = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphA ) )
      //   val hiddenVarB = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphB ) )

      //   hiddenVariables = hiddenVarA ++ hiddenVarB

      //   val obsVarA = Array.tabulate( tokens.size )( _ => new Variable( obsAlphA ) )
      //   val obsVarB = Array.tabulate( tokens.size )( _ => new Variable( obsAlphB ) )

      //   observations = obsVarA ++ obsVarB

      //   ( 0 to tokens.size - 1 ) foreach{ i =>
      //     hiddenVarA( i ).setLabel("hidden.A"+i)
      //     hiddenVarB( i ).setLabel("hidden.B"+i)
      //     obsVarA( i ).setLabel("observation.A"+i)
      //     obsVarB( i ).setLabel("observation.B"+i)
      //   }


      //         // // initial states:
      //         // hmm.addFactor(
      //         //   new CPT(
      //         //     new TableFactor(
      //         //       Array( hiddenVariables(0) , hiddenVariables(1) ),
      //         //       (InitialStateProbabilitiesA * TransitionMatrixA).toArray
      //         //     ),
      //         //     hiddenVariables(1)
      //         //   )
      //         // )
      //         // hmm.addFactor(
      //         //   new CPT(
      //         //     new TableFactor(
      //         //       Array( hiddenVariables(tokens.size) , hiddenVariables(tokens.size+1) ),
      //         //       (InitialStateProbabilitiesB * TransitionMatrixB).toArray
      //         //     ),
      //         //     hiddenVariables(tokens.size+1)
      //         //   )
      //         // )
      //         // state transitions:


      //   // initial states:
      //   hmm.addFactor(
      //     new CPT(
      //       new TableFactor(
      //         Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarA(1) ),
      //         ( InitialStateProbabilities * TransitionMatrixA).toArray
      //         //( (InitialStateProbabilitiesA * InitialStateProbabilitiesB )* TransitionMatrixA).toArray
      //       ),
      //       hiddenVarA(1)
      //     )
      //   )
      //   hmm.addFactor(
      //     new CPT(
      //       new TableFactor(
      //         Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarB(1) ),
      //         (InitialStateProbabilities * TransitionMatrixB).toArray
      //       ),
      //       hiddenVarB(1)
      //     )
      //   )


      //   ( 2 to (tokens.size-1) ) foreach{ i =>
      //     hmm.addFactor(
      //       new CPT(
      //         new TableFactor(
      //           Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarA(i) ),
      //           TransitionMatrixA.toArray
      //         ),
      //       hiddenVarA(i)
      //       )
      //     )
      //     hmm.addFactor(
      //       new CPT(
      //         new TableFactor(
      //           Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarB(i) ),
      //           TransitionMatrixB.toArray
      //         ),
      //         hiddenVarB(i)
      //       )
      //     )
      //   }

      //   // emissions
      //   ( 0 to tokens.size-1) foreach{ i =>
      //     //val ObservedStatePair( obsA, obsB ) = tokens(i)
      //     hmm.addFactor(
      //       new CPT(
      //         new TableFactor(
      //           Array( hiddenVarA(i), obsVarA(i) ),
      //           EmissionMatrixA.toArray
      //         ),
      //         obsVarA(i)
      //       )
      //     )
      //     hmm.addFactor(
      //       new CPT(
      //         new TableFactor(
      //           Array( hiddenVarB(i), obsVarB(i) ),
      //           EmissionMatrixB.toArray
      //         ),
      //         obsVarB(i)
      //       )
      //     )
      //   }
      // }

  def buildSlicedHMM( tokens: List[ObservedStatePair] ) {
    hmm = new DynamicBayesNet(tokens.size)

    stringLength = tokens.size

    val hiddenVarA = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphA.size ) )
    val hiddenVarB = Array.tabulate( tokens.size )( _ => new Variable( hiddAlphB.size ) )

    hiddenVariables = hiddenVarA ++ hiddenVarB

    val obsVarA = Array.tabulate( tokens.size )( _ => new Variable( obsAlphA.size ) )
    val obsVarB = Array.tabulate( tokens.size )( _ => new Variable( obsAlphB.size ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVarA( i ).setLabel("hidden.A"+i)
      hiddenVarB( i ).setLabel("hidden.B"+i)
      obsVarA( i ).setLabel("observation.A"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }


        // println( ( (InitialStateProbabilitiesA * InitialStateProbabilitiesB )* TransitionMatrixA) )
        // println( "----" )
        // println( ( (InitialStateProbabilitiesA * InitialStateProbabilitiesB )*
        // TransitionMatrixA).toArray.size )
        // println( ( (InitialStateProbabilitiesA * InitialStateProbabilitiesB )*
        // TransitionMatrixA).toArray.mkString("[ "," ; "," ]" ) )

        // println( "\n\n\n\n\n==============]]]]]]]]]]]]][[[[[[[[[[[[[[[[[[===========\n\n\n\n\n" )
        // println( ( InitialStateProbabilities * TransitionMatrixA ) )
        // println( "\n\n\n\n\n==============]]]]]]]]]]]]][[[[[[[[[[[[[[[[[[===========\n\n" )
        // println( ( InitialStateProbabilities * TransitionMatrixA ).toArray.mkString("\t",",\n\t","") )
        // println( "\n\n==============]]]]]]]]]]]]][[[[[[[[[[[[[[[[[[===========\n\n\n\n\n" )

    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarA(1) ),
          (InitialStateProbabilities * TransitionMatrixA).toArray
          //( InitialStateProbabilitiesA * InitialStateProbabilitiesB * TransitionMatrixA).toArray
          //( (InitialStateProbabilitiesA * InitialStateProbabilitiesB )* TransitionMatrixA).toArray
        ),
        hiddenVarA(1)
      ),
      0
    )
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarB(1) ),
          (InitialStateProbabilities * TransitionMatrixB).toArray
          //( InitialStateProbabilitiesA * InitialStateProbabilitiesB * TransitionMatrixA).toArray
        ),
        hiddenVarB(1)
      ),
      0
    )

    /*
    if( (InitialStateProbabilities * TransitionMatrixA).toArray.exists( _ == 0 )  ) {
      println( "Aha! " + (InitialStateProbabilities *
      TransitionMatrixA).toArray.mkString("[\n\t","\n\t","\n]\n")  )
      println( "InitialStateProbabilities" + InitialStateProbabilities )
      println( "TransitionMatrixA" + TransitionMatrixA )
    }
    */

    //state transitions:
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarA(i) ),
            TransitionMatrixA.toArray
          ),
        hiddenVarA(i)
        ),
        i-1
      )
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarB(i) ),
            TransitionMatrixB.toArray
          ),
          hiddenVarB(i)
        ),
        i-1
      )
    }

    // emissions
    ( 0 to tokens.size-1) foreach{ i =>
      val ObservedStatePair( obsA, obsB ) = tokens(i)

      val assignmentA = new Assignment(
        obsVarA(i),
        obsAlphA.lookupIndex( ObservedState( obsA ) )
      )

      val assignmentB = new Assignment(
        obsVarB(i),
        obsAlphB.lookupIndex( ObservedState( obsB ) )
      )


      hmm.addObservedTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarA(i), obsVarA(i) ),
            EmissionMatrixA.toArray
          ),
          obsVarA(i),
          assignmentA
        ),
        i
      )
      hmm.addObservedTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVarB(i), obsVarB(i) ),
            EmissionMatrixB.toArray
          ),
          obsVarB(i),
          assignmentB
        ),
        i
      )
    }


  }

  def reestimate( corpus: List[List[ObservedStatePair]] ) = {
    import math.exp

    val corpusInitialStateCounts  = new MHashMap[HiddenStatePair,Double] {
      override def default( qs:HiddenStatePair ) = Double.NegativeInfinity
    }

    val corpusTransitionCountsA = new MHashMap[HiddenStatePair,MHashMap[HiddenState,Double]] {
      override def default( qsFrom:HiddenStatePair ) = {
        this += Pair(
          qsFrom,
          new MHashMap[HiddenState,Double] {
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qsFrom)
      }
    }

    val corpusTransitionCountsB = new MHashMap[HiddenStatePair,MHashMap[HiddenState,Double]] {
      override def default( qsFrom:HiddenStatePair ) = {
        this += Pair(
          qsFrom,
          new MHashMap[HiddenState,Double] {
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qsFrom)
      }
    }

    val corpusEmissionCountsA = new MHashMap[HiddenState,MHashMap[ObservedState,Double]]{
      override def default( q:HiddenState ) = {
        this += Pair(
          q,
          new MHashMap[ObservedState,Double]{
            override def default( o:ObservedState ) = {
              this += Pair( o, Double.NegativeInfinity )
              this(o)
            }
          }
        )
        this(q)
      }
    }

    val corpusEmissionCountsB = new MHashMap[HiddenState,MHashMap[ObservedState,Double]]{
      override def default( q:HiddenState ) = {
        this += Pair(
          q,
          new MHashMap[ObservedState,Double]{
            override def default( o:ObservedState ) = {
              this += Pair( o, Double.NegativeInfinity )
              this(o)
            }
          }
        )
        this(q)
      }
    }

    var totalCorpusLogProb = 0D


    //var n = 0

    corpus.foreach{ string =>
      val CoupledHMMPartialCounts(
        stringLogProb,
        initialStateCounts,
        transitionCountsA,
        transitionCountsB,
        emissionCountsA,
        emissionCountsB
      ) = computePartialCounts( string )

      //println( "stringLogProb" )
      //println( stringLogProb )
      //println( "transitionCountsA" )
      //println( transitionCountsA )

      //if( n % 10 == 0 )
      //  println( n + ": " + string.length )

      //n += 1

      // Sum totals for this string
      assert( transitionCountsA.keySet == transitionCountsB.keySet )

      transitionCountsA.keySet.foreach{ qsFrom =>
        corpusInitialStateCounts(qsFrom) = Util.log_add(
          List(
            corpusInitialStateCounts(qsFrom),
            initialStateCounts(qsFrom) - stringLogProb
          )
        )

        transitionCountsA(qsFrom).keySet.foreach{ qTo =>
          corpusTransitionCountsA(qsFrom)(qTo) = Util.log_add(
            List(
              corpusTransitionCountsA(qsFrom)(qTo),
              transitionCountsA(qsFrom)(qTo) - stringLogProb
            )
          )
        }

        transitionCountsB(qsFrom).keySet.foreach{ qTo =>
          corpusTransitionCountsB(qsFrom)(qTo) = Util.log_add(
            List(
              corpusTransitionCountsB(qsFrom)(qTo),
              transitionCountsB(qsFrom)(qTo) - stringLogProb
            )
          )
        }
      }

      emissionCountsA.keySet.foreach{ qA =>
        emissionCountsA(qA).keySet.foreach{ obsA =>
          corpusEmissionCountsA(qA)(obsA) = Util.log_add(
            List(
              corpusEmissionCountsA(qA)(obsA),
              emissionCountsA(qA)(obsA) //- stringLogProb
            )
          )
        }
      }


      emissionCountsB.keySet.foreach{ qB =>
        emissionCountsB(qB).keySet.foreach{ obsB =>
          corpusEmissionCountsB(qB)(obsB) = Util.log_add(
            List(
              corpusEmissionCountsB(qB)(obsB),
              emissionCountsB(qB)(obsB) //- stringLogProb
            )
          )
        }
      }


      totalCorpusLogProb += stringLogProb
    }


    val initialStateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ qsFrom =>
        qsFrom -> 
          ( corpusInitialStateCounts( qsFrom ) )
      }.toSeq:_*
    )

    val transitionProbsA = HashMap(
      corpusTransitionCountsA.keySet.map{ qsFrom =>
        qsFrom -> HashMap(
          corpusTransitionCountsA(qsFrom).keySet.map{ qTo =>
            qTo -> (
              corpusTransitionCountsA( qsFrom )( qTo )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionProbsB = HashMap(
      corpusTransitionCountsB.keySet.map{ qsFrom =>
        qsFrom -> HashMap(
          corpusTransitionCountsB(qsFrom).keySet.map{ qTo =>
            qTo -> (
              corpusTransitionCountsB( qsFrom )( qTo )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )


    val emissionProbsA = HashMap(
      corpusEmissionCountsA.keySet.map{ qA =>
        qA -> HashMap(
          corpusEmissionCountsA(qA).keySet.map{ obsA =>
            obsA -> (
              corpusEmissionCountsA( qA )( obsA )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionProbsB = HashMap(
      corpusEmissionCountsB.keySet.map{ qB =>
        qB -> HashMap(
          corpusEmissionCountsB(qB).keySet.map{ obsB =>
            obsB -> (
              corpusEmissionCountsB( qB )( obsB )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )


    InitialStateProbabilities.setPT( initialStateProbs )
    TransitionMatrixA.setCPT( transitionProbsA )
    TransitionMatrixB.setCPT( transitionProbsB )
    EmissionMatrixA.setCPT( emissionProbsA )
    EmissionMatrixB.setCPT( emissionProbsB )
    normalize


    totalCorpusLogProb
  }

  def viterbi( corpus: List[ObservedStatePair] ) = List[HiddenStatePair]()

  def generateObservationSequence( tokens:List[ObservedStatePair] ) = {
    val theSequence = new Assignment(
      observations,
      tokens.map{ case ObservedStatePair( obsA, _ ) =>
        obsAlphA.lookupIndex( ObservedState( obsA ) )
      }.toArray ++ tokens.map{ case ObservedStatePair( _, obsB ) =>
        obsAlphB.lookupIndex( ObservedState( obsB ) )
      }
    )
    //println( "sequence is: " + theSequence )
    theSequence
  }

  def computePartialCounts( tokens:List[ObservedStatePair] ) = {
    buildSlicedHMM( tokens )

    inferencer.computeMarginals( hmm )

    val initialStateCounts = MHashMap(
      hiddenStateTypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )


    val emissionCountsA = MHashMap(
      hiddATypes.map{ q =>
        q -> MHashMap(
          obsATypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionCountsB = MHashMap(
      hiddBTypes.map{ q =>
        q -> MHashMap(
          obsBTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsA = MHashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> MHashMap(
          hiddATypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsB = MHashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> MHashMap(
          hiddBTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    ( 0 to (tokens.size-2) ) foreach{ i =>
      val( fromVarA, fromVarB, toVarA, toVarB ) = (
        hiddenVariables(i), hiddenVariables(tokens.size+i),
        hiddenVariables(i+1), hiddenVariables(tokens.size+i+1)
      )

      val ObservedStatePair( obsA, obsB ) = tokens(i)

      val transitionA = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, fromVarB, toVarA ) )
      )
      val transitionB = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, fromVarB, toVarB ) )
      )

      hiddenStateTypes.foreach{ qsFrom =>
        val HiddenStatePair( qFromAString, qFromBString ) = qsFrom
        val qFromA = HiddenState( qFromAString )
        val qFromB = HiddenState( qFromBString )

        // first add up for the A stream
        hiddATypes.foreach{ qToA =>

          val thisTransitionCountA = transitionA.logValue(
            new Assignment(
              Array( fromVarA, fromVarB, toVarA ),
              Array(
                hiddAlphA.lookupIndex( qFromA ),
                hiddAlphB.lookupIndex( qFromB ),
                hiddAlphA.lookupIndex( qToA )
              )
            )
          )

          if( i == 0 ) {
            initialStateCounts( qsFrom ) = Util.log_add(
              List(
                initialStateCounts( qsFrom ),
                thisTransitionCountA
              )
            )
          }

          transitionCountsA(qsFrom)(qToA) = Util.log_add(
            List(
              transitionCountsA(qsFrom)(qToA),
              thisTransitionCountA
            )
          )

          emissionCountsA(qFromA)( ObservedState(obsA) ) = Util.log_add(
            List(
              emissionCountsA(qFromA)( ObservedState(obsA) ),
              thisTransitionCountA
            )
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( lastObsAString, _ ) = tokens(i+1)

            emissionCountsA(qToA)( ObservedState(lastObsAString) ) = Util.log_add(
              List(
                emissionCountsA(qToA)( ObservedState(lastObsAString) ),
                thisTransitionCountA
              )
            )
          }

        }

        hiddBTypes.foreach{ qToB =>

          val thisTransitionCountB = transitionB.logValue(
            new Assignment(
              Array( fromVarA, fromVarB, toVarB ),
              Array(
                hiddAlphA.lookupIndex( qFromA ),
                hiddAlphB.lookupIndex( qFromB ),
                hiddAlphB.lookupIndex( qToB )
              )
            )
          )

          if( i == 0 ) {
            initialStateCounts( qsFrom ) = Util.log_add(
              List(
                initialStateCounts( qsFrom ),
                thisTransitionCountB
              )
            )
          }

          transitionCountsB(qsFrom)(qToB) = Util.log_add(
            List(
              transitionCountsB(qsFrom)(qToB),
              thisTransitionCountB
            )
          )

          emissionCountsB(qFromB)( ObservedState(obsB) ) = Util.log_add(
            List(
              emissionCountsB(qFromB)( ObservedState(obsB) ),
              thisTransitionCountB
            )
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( _, lastObsBString ) = tokens(i+1)

            emissionCountsB(qToB)( ObservedState(lastObsBString) ) = Util.log_add(
              List(
                emissionCountsB(qToB)( ObservedState(lastObsBString) ),
                thisTransitionCountB
              )
            )
          }

        }
      }
    }

    //println( "\n\nAbout to hit the doozy?" )
    CoupledHMMPartialCounts(
      generalProbability( tokens ),
      HashMap(
        initialStateCounts.keySet.map{ qsFrom =>
          qsFrom -> initialStateCounts(qsFrom)
        }.toSeq:_*
      ),
      //HashMap(
      //  initialStateCountsB.keySet.map{ qFrom =>
      //    qFrom -> initialStateCountsB(qFrom)
      //  }.toSeq:_*
      //),
      HashMap(
        transitionCountsA.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            transitionCountsA(qsFrom).keySet.map{ qTo =>
              qTo -> transitionCountsA(qsFrom)(qTo)
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        transitionCountsB.keySet.map{ qsFrom =>
          qsFrom -> HashMap(
            transitionCountsB(qsFrom).keySet.map{ qTo =>
              qTo -> transitionCountsB(qsFrom)(qTo)
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCountsA.keySet.map{ q =>
          q -> HashMap(
            emissionCountsA(q).keySet.map{ obs =>
              obs -> emissionCountsA(q)(obs)
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCountsB.keySet.map{ q =>
          q -> HashMap(
            emissionCountsB(q).keySet.map{ obs =>
              obs -> emissionCountsB(q)(obs)
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }

    // def computePartialCounts( sequence:List[ObservedStatePair] ) = {
    //   object Dummy extends PartialCounts
    //   Dummy
    // }
    // PartialCounts(
    //   0D,
    //   HashMap(
    //     hiddATypes.map( thisStateName =>
    //       thisStateName -> 1D/hiddATypes.size
    //     ).toSeq: _*
    //   ),
    //   HashMap(
    //     hiddenStateTypes.map( fromStateName =>
    //       fromStateName -> (
    //         HashMap(
    //           obsBTypes.map( toStateName =>
    //             toStateName -> 1D/obsBTypes.size 
    //           ).toSeq: _*
    //         )
    //       )
    //     ).toSeq: _*
    //   ),
    //   HashMap(
    //     hiddBTypes.map( fromStateName =>
    //       fromStateName -> (
    //         HashMap(
    //           obsBTypes.map( toStateName =>
    //             toStateName -> 1D/obsBTypes.size 
    //           ).toSeq: _*
    //         )
    //       )
    //     ).toSeq: _*
    //   ),

    //   
    // )

  override def toString =
    "  == HMM Parameters == \n" +
    "\nInitialProbabilities" +
    InitialStateProbabilities +
    //"\nInitialProbabilitiesB" +
    //InitialStateProbabilitiesB +
    "\nTransitionsA:" +
    TransitionMatrixA +
    "\nTransitionsB:" +
    TransitionMatrixB +
    "\nEmissionsA" +
    EmissionMatrixA +
    "\nEmissionsB" +
    EmissionMatrixB

}

