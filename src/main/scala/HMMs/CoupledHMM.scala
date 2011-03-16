package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.util.Maths
import collection.mutable.HashMap


class CoupledHMM(
  hiddenStateTypesSet:Set[HiddenStatePair],
  observationTypesSet:Set[ObservedStatePair],
  idString:String
) extends AbstractHMM[HiddenStatePair,ObservedStatePair] (hiddenStateTypesSet,observationTypesSet) {
  import math.{log,exp}

  val hmmID = idString

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
  val hiddAIndexToLabel = new HashMap[Int,HiddenState]
  val hiddBIndexToLabel = new HashMap[Int,HiddenState]
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





  val transitionMatrixA =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddATypes )

  val transitionMatrixB =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddBTypes )

  var emissionMatrixA =
    new ConditionalLogProbabilityDistribution( hiddATypes.toSet, obsATypes )

  var emissionMatrixB =
    new ConditionalLogProbabilityDistribution( hiddBTypes.toSet, obsBTypes )

  val initialStateProbabilitiesA =
    new LogProbabilityDistribution[HiddenState]( hiddATypes.toSet )

  val initialStateProbabilitiesB =
    new LogProbabilityDistribution[HiddenState]( hiddBTypes.toSet )

  // val initialStateProbabilities =
  //   new LogProbabilityDistribution[HiddenStatePair]( hiddenStateTypesSet )

  def parameters = List(
    initialStateProbabilitiesA,
    initialStateProbabilitiesB,
    transitionMatrixA,
    transitionMatrixB,
    emissionMatrixA,
    emissionMatrixB
  )

  def packageParameters = CoupledHMMParameters(
    //initialStateProbabilities.pt,
    initialStateProbabilitiesA.pt,
    initialStateProbabilitiesB.pt,
    transitionMatrixA.cpt,
    transitionMatrixB.cpt,
    emissionMatrixA.cpt,
    emissionMatrixB.cpt
  )


  def initialPartialCounts = CoupledHMMPartialCounts(
    0D,
    HashMap(
      hiddATypes.map( thisStateName =>
        thisStateName -> Double.NegativeInfinity
      ).toSeq: _*
    ),
    HashMap(
      hiddBTypes.map( thisStateName =>
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
      initialProbsA,
      initialProbsB,
      transitionsA,
      transitionsB,
      emissionsA,
      emissionsB
    ) = newParams

    initialStateProbabilitiesA.setPT( initialProbsA )
    initialStateProbabilitiesB.setPT( initialProbsB )
    transitionMatrixA.setCPT( transitionsA )
    transitionMatrixB.setCPT( transitionsB )
    emissionMatrixA.setCPT( emissionsA )
    emissionMatrixB.setCPT( emissionsB )
  }

  def buildHMM( tokens: List[ObservedStatePair] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet( tokens.size )

    stringLength = tokens.size

    val hiddenVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphA, t ) )
    val hiddenVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphB, t ) )

    hiddenVariables = hiddenVarA ++ hiddenVarB

    val obsVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphA, t ) )
    val obsVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphB, t ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVarA( i ).setLabel("hidden.A"+i)
      hiddenVarB( i ).setLabel("hidden.B"+i)
      obsVarA( i ).setLabel("observation.A"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }


    // initial states:
        // hmm.addHiddenTimedFactor(
        //   new CPT(
        //     LogTableFactor.makeFromLogValues(
        //       Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarA(1) ),
        //       transitionMatrixA.toLogArray
        //       //( initialStateProbabilities * transitionMatrixA).toArray
        //       //( ( initialStateProbabilitiesA * initialStateProbabilitiesB ) * transitionMatrixA).toArray
        //     ),
        //     hiddenVarA(1)
        //   ),
        //   0
        // )
        // hmm.addHiddenTimedFactor(
        //   new CPT(
        //     LogTableFactor.makeFromLogValues(
        //       Array( hiddenVarA(0) , hiddenVarB(0), hiddenVarB(1) ),
        //       transitionMatrixB.toLogArray
        //       //(initialStateProbabilities * transitionMatrixB).toArray
        //       //( initialStateProbabilitiesA * initialStateProbabilitiesB * transitionMatrixA).toArray
        //     ),
        //     hiddenVarB(1)
        //   ),
        //   0
        // )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVarA(0) ),
        //Array( hiddenVarA(0) , hiddenVarB(0) ),
        initialStateProbabilitiesA.toLogArray
      )
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVarB(0) ),
        //Array( hiddenVarA(0) , hiddenVarB(0) ),
        initialStateProbabilitiesB.toLogArray
      )
    )

    ( 1 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarA(i) ),
            transitionMatrixA.toLogArray
          ),
        hiddenVarA(i)
        ),
        Array( i-1, i-1 ),
        i
      )
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarB(i) ),
            transitionMatrixB.toLogArray
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
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i), obsVarA(i) ),
            emissionMatrixA.toLogArray
          ),
          obsVarA(i)
        ),
        i
      )
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarB(i), obsVarB(i) ),
            emissionMatrixB.toLogArray
          ),
          obsVarB(i)
        ),
        i
      )
    }

  }


  def reestimate( corpus: List[List[ObservedStatePair]] ) = {
    import math.exp

    /*
    val corpusInitialStateCounts  = new HashMap[HiddenStatePair,Double] {
      override def default( qs:HiddenStatePair ) = Double.NegativeInfinity
    }
    */

    val corpusInitialStateCountsA  = new HashMap[HiddenState,Double] {
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpusInitialStateCountsB  = new HashMap[HiddenState,Double] {
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpustransitionCountsA = new HashMap[HiddenStatePair,HashMap[HiddenState,Double]] {
      override def default( qsFrom:HiddenStatePair ) = {
        this += Pair(
          qsFrom,
          new HashMap[HiddenState,Double] {
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qsFrom)
      }
    }

    val corpustransitionCountsB = new HashMap[HiddenStatePair,HashMap[HiddenState,Double]] {
      override def default( qsFrom:HiddenStatePair ) = {
        this += Pair(
          qsFrom,
          new HashMap[HiddenState,Double] {
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qsFrom)
      }
    }

    val corpusEmissionCountsA = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
      override def default( q:HiddenState ) = {
        this += Pair(
          q,
          new HashMap[ObservedState,Double]{
            override def default( o:ObservedState ) = {
              this += Pair( o, Double.NegativeInfinity )
              this(o)
            }
          }
        )
        this(q)
      }
    }

    val corpusEmissionCountsB = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
      override def default( q:HiddenState ) = {
        this += Pair(
          q,
          new HashMap[ObservedState,Double]{
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
        initialStateCountsA,
        initialStateCountsB,
        transitionCountsA,
        transitionCountsB,
        emissionCountsA,
        emissionCountsB
      ) = computePartialCounts( string )

      // Sum totals for this string
      assert( transitionCountsA.keySet == transitionCountsB.keySet )

      transitionCountsA.keySet.foreach{ qsFrom =>
        // corpusInitialStateCounts(qsFrom) = Maths.sumLogProb(
        //     corpusInitialStateCounts(qsFrom),
        //     initialStateCounts(qsFrom)
        // )

        transitionCountsA(qsFrom).keySet.foreach{ qTo =>
          corpustransitionCountsA(qsFrom)(qTo) = Maths.sumLogProb(
              corpustransitionCountsA(qsFrom)(qTo),
              transitionCountsA(qsFrom)(qTo) //- stringLogProb
          )
        }

        transitionCountsB(qsFrom).keySet.foreach{ qTo =>
          corpustransitionCountsB(qsFrom)(qTo) = Maths.sumLogProb(
              corpustransitionCountsB(qsFrom)(qTo),
              transitionCountsB(qsFrom)(qTo) //- stringLogProb
          )
        }
      }

      emissionCountsA.keySet.foreach{ qA =>
        emissionCountsA(qA).keySet.foreach{ obsA =>
          corpusInitialStateCountsA(qA) = Maths.sumLogProb(
              corpusInitialStateCountsA(qA),
              initialStateCountsA(qA)
          )
          corpusEmissionCountsA(qA)(obsA) = Maths.sumLogProb(
              corpusEmissionCountsA(qA)(obsA),
              emissionCountsA(qA)(obsA)
          )
        }
      }


      emissionCountsB.keySet.foreach{ qB =>
        emissionCountsB(qB).keySet.foreach{ obsB =>
          corpusInitialStateCountsB(qB) = Maths.sumLogProb(
              corpusInitialStateCountsB(qB),
              initialStateCountsB(qB)
          )
          corpusEmissionCountsB(qB)(obsB) = Maths.sumLogProb(
              corpusEmissionCountsB(qB)(obsB),
              emissionCountsB(qB)(obsB)
          )
        }
      }


      totalCorpusLogProb += stringLogProb
    }


    // val initialStateProbs = HashMap(
    //   corpusInitialStateCounts.keySet.map{ qFrom =>
    //     qFrom -> 
    //       ( corpusInitialStateCountsA( qFrom ) )
    //   }.toSeq:_*
    // )

    val initialStateProbsA = HashMap(
      corpusInitialStateCountsA.keySet.map{ qFrom =>
        qFrom -> 
          ( corpusInitialStateCountsA( qFrom ) )
      }.toSeq:_*
    )
    val initialStateProbsB = HashMap(
      corpusInitialStateCountsB.keySet.map{ qFrom =>
        qFrom -> 
          ( corpusInitialStateCountsB( qFrom ) )
      }.toSeq:_*
    )

    val transitionProbsA = HashMap(
      corpustransitionCountsA.keySet.map{ qsFrom =>
        qsFrom -> HashMap(
          corpustransitionCountsA(qsFrom).keySet.map{ qTo =>
            qTo -> (
              corpustransitionCountsA( qsFrom )( qTo )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionProbsB = HashMap(
      corpustransitionCountsB.keySet.map{ qsFrom =>
        qsFrom -> HashMap(
          corpustransitionCountsB(qsFrom).keySet.map{ qTo =>
            qTo -> (
              corpustransitionCountsB( qsFrom )( qTo )
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


    //initialStateProbabilities.setPT( initialStateProbs )
    initialStateProbabilitiesA.setPT( initialStateProbsA )
    initialStateProbabilitiesB.setPT( initialStateProbsB )
    transitionMatrixA.setCPT( transitionProbsA )
    transitionMatrixB.setCPT( transitionProbsB )
    emissionMatrixA.setCPT( emissionProbsA )
    emissionMatrixB.setCPT( emissionProbsB )
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
    //buildHMM( tokens )


        // println( "\n\n\n\n=====\n\n\n\n")
        // println( hmm.dumpToString )
        // println( "\n\n\n\n=====\n\n\n\n")

    inferencer.computeMarginals( hmm )

    val initialStateCountsA = HashMap(
      hiddATypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )

    val initialStateCountsB = HashMap(
      hiddBTypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )


    val emissionCountsA = HashMap(
      hiddATypes.map{ q =>
        q -> HashMap(
          obsATypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionCountsB = HashMap(
      hiddBTypes.map{ q =>
        q -> HashMap(
          obsBTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsA = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddATypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsB = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
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

              // if( qFromA == HiddenState( "C_B" ) && qToA == HiddenState( "C_O" ) ) {
              //   println(
              //     "<<< HiddenState(\"C_B\") is " + hiddAlphA.lookupIndex(qFromA ) + " ;  " +
              //     "HiddenState(\"C_O\") is " + hiddAlphA.lookupIndex(qToA ) + " ;  " +
              //     "qFromB is " + qFromB + " ; " +
              //     thisTransitionCountA + " >>>" 
              //   )
              // }

          if( i == 0 ) {
            initialStateCountsA( qFromA ) = Maths.sumLogProb(
                initialStateCountsA( qFromA ),
                thisTransitionCountA
            )
          }

          transitionCountsA(qsFrom)(qToA) = Maths.sumLogProb(
              transitionCountsA(qsFrom)(qToA),
              thisTransitionCountA
          )

          emissionCountsA(qFromA)( ObservedState(obsA) ) = Maths.sumLogProb(
              emissionCountsA(qFromA)( ObservedState(obsA) ),
              thisTransitionCountA
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( lastObsAString, _ ) = tokens(i+1)

            emissionCountsA(qToA)( ObservedState(lastObsAString) ) = Maths.sumLogProb(
                emissionCountsA(qToA)( ObservedState(lastObsAString) ),
                thisTransitionCountA
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
            initialStateCountsB( qFromB ) = Maths.sumLogProb(
                initialStateCountsB( qFromB ),
                thisTransitionCountB
            )
          }

          transitionCountsB(qsFrom)(qToB) = Maths.sumLogProb(
              transitionCountsB(qsFrom)(qToB),
              thisTransitionCountB
          )

          emissionCountsB(qFromB)( ObservedState(obsB) ) = Maths.sumLogProb(
              emissionCountsB(qFromB)( ObservedState(obsB) ),
              thisTransitionCountB
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( _, lastObsBString ) = tokens(i+1)

            emissionCountsB(qToB)( ObservedState(lastObsBString) ) = Maths.sumLogProb(
                emissionCountsB(qToB)( ObservedState(lastObsBString) ),
                thisTransitionCountB
            )
          }

        }
      }
    }

    CoupledHMMPartialCounts(
      generalProbability( tokens ),
      initialStateCountsA,
      initialStateCountsB,
      transitionCountsA,
      transitionCountsB,
      emissionCountsA,
      emissionCountsB
    )
  }


  override def toString =
    "  == HMM Parameters == \n" +
    "\nInitialProbabilitiesA" +
    initialStateProbabilitiesA +
    "\nInitialProbabilitiesB" +
    initialStateProbabilitiesB +
    "\ntransitionsA:" +
    transitionMatrixA +
    "\ntransitionsB:" +
    transitionMatrixB +
    "\nemissionsA" +
    emissionMatrixA +
    "\nemissionsB" +
    emissionMatrixB

}

