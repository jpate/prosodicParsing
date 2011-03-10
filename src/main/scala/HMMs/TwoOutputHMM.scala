package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.types.LabelAlphabet
//import collection.immutable.HashMap
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.util.Maths
import collection.mutable.HashMap
//import collection.mutable.{HashMap => HashMap}


class TwoOutputHMM(
  hiddenStateTypesSet:Set[HiddenState],
  observationTypesSet:Set[ObservedStatePair],
  idString:String
) extends AbstractHMM[HiddenState,ObservedStatePair] (hiddenStateTypesSet,observationTypesSet) {

  val hmmID = idString

  val hiddenStateAlphabet = new LabelAlphabet()
  val hiddenStateIndexToLabel = new HashMap[Int,HiddenState]
  hiddenStateTypes.foreach( q =>
    hiddenStateIndexToLabel( hiddenStateAlphabet.lookupIndex( q, true ) ) = q
  )

  val Tuple2( obsATypes, obsBTypes ) =
    observationTypesSet.map{ case ObservedStatePair( obs1, obs2 ) =>
      Tuple2( ObservedState( obs1 ), ObservedState( obs2 ) )
    }.foldLeft(Tuple2[Set[ObservedState],Set[ObservedState]](Set(),Set())){ (a,b) =>
      val Tuple2( obs1s, obs2s ) = a
      val Tuple2( obs1, obs2 ) = b
      Tuple2( obs1s + obs1, obs2s + obs2 )
    }

  val obsAlphA = new LabelAlphabet()
  val obsAlphB = new LabelAlphabet()
  obsATypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphA.lookupIndex( _,true ) )
  obsBTypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphB.lookupIndex( _,true ) )

  def assignmentToViterbiString( maxAssn:Assignment ) = {
    hiddenVariables.map{ hiddenVar =>
      hiddenStateIndexToLabel( maxAssn.get( hiddenVar ) )
    }.toList
  }

  val transitionMatrix =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddenStateTypesSet )

  var emissionMatrixA =
    new ConditionalLogProbabilityDistribution ( hiddenStateTypesSet, obsATypes )

  var emissionMatrixB =
    new ConditionalLogProbabilityDistribution ( hiddenStateTypesSet, obsBTypes )

  val initialStateProbabilities = new LogProbabilityDistribution( hiddenStateTypesSet )

  def parameters = List(
    initialStateProbabilities,
    transitionMatrix,
    emissionMatrixA,
    emissionMatrixB
  )

  def packageParameters = TwoOutputHMMParameters(
    //initialStateProbabilities.pt,
    initialStateProbabilities.pt,
    transitionMatrix.cpt,
    emissionMatrixA.cpt,
    emissionMatrixB.cpt
  )

  def initialPartialCounts = TwoOutputHMMPartialCounts(
      0D,
      HashMap(
        hiddenStateTypesSet.map( thisStateName =>
          thisStateName -> Double.NegativeInfinity
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( qFrom =>
          qFrom -> (
            HashMap(
              hiddenStateTypes.map( toStateName =>
                toStateName -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( qFrom =>
          qFrom -> (
            HashMap(
              obsATypes.map( obs =>
                obs -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( fromStateName =>
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
    val TwoOutputHMMParameters(
      initialProbs,
      transitions,
      emissionsA,
      emissionsB
    ) = newParams

    initialStateProbabilities.setPT( initialProbs )
    transitionMatrix.setCPT( transitions )
    emissionMatrixA.setCPT( emissionsA )
    emissionMatrixB.setCPT( emissionsB )
  }

  def buildHMM( tokens: List[ObservedStatePair] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet( tokens.size )

    stringLength = tokens.size

    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( localUniverse, hiddenStateAlphabet ) )

    val obsVarA = Array.tabulate( tokens.size )( _ => new Variable( localUniverse, obsAlphA ) )
    val obsVarB = Array.tabulate( tokens.size )( _ => new Variable( localUniverse, obsAlphB ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVariables( i ).setLabel("hidden"+i)
      obsVarA( i ).setLabel("observation.A"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }


    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        LogTableFactor.makeFromLogValues(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          transitionMatrix.toLogArray
          //( initialStateProbabilities * transitionMatrixA).toArray
          //( ( initialStateProbabilitiesA * initialStateProbabilitiesB ) * transitionMatrixA).toArray
        ),
        hiddenVariables(1)
      ),
      0
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVariables(0) ),
        //Array( hiddenVarA(0) , hiddenVarB(0) ),
        initialStateProbabilities.toLogArray
      )
    )

    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
            transitionMatrix.toLogArray
          ),
        hiddenVariables(i)
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
            Array( hiddenVariables(i), obsVarA(i) ),
            emissionMatrixA.toLogArray
          ),
          obsVarA(i)
        ),
        i
      )
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i), obsVarB(i) ),
            emissionMatrixB.toLogArray
          ),
          obsVarB(i)
        ),
        i
      )
    }
  }

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


    val initialStateCounts = HashMap(
      hiddenStateTypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )


    val emissionCountsA = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          obsATypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionCountsB = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          obsBTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCounts = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddenStateTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    ( 0 to (tokens.size-2) ) foreach{ i =>
      val (fromVar, toVar) = (hiddenVariables(i), hiddenVariables(i+1) )

      val ObservedStatePair( obsA, obsB ) = tokens(i)

      val thisTransition = inferencer.lookupMarginal( new HashVarSet( Array( fromVar, toVar ) ) )

      hiddenStateTypes.foreach{ qFrom =>
        // first add up for the A stream
        hiddenStateTypes.foreach{ qTo =>

          val thisTransitionCount = thisTransition.logValue(
            new Assignment(
              Array( fromVar, toVar ),
              Array(
                hiddenStateAlphabet.lookupIndex( qFrom ),
                hiddenStateAlphabet.lookupIndex( qTo )
              )
            )
          )

          if( i == 0 ) {
            initialStateCounts( qFrom ) = Maths.sumLogProb(
                initialStateCounts( qFrom ),
                thisTransitionCount
            )
          }

          transitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              transitionCounts(qFrom)(qTo),
              thisTransitionCount
          )

          emissionCountsA(qFrom)( ObservedState(obsA) ) = Maths.sumLogProb(
              emissionCountsA(qFrom)( ObservedState(obsA) ),
              thisTransitionCount
          )

          emissionCountsB(qFrom)( ObservedState(obsB) ) = Maths.sumLogProb(
              emissionCountsB(qFrom)( ObservedState(obsB) ),
              thisTransitionCount
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( lastObsAString, lastObsBString ) = tokens(i+1)

            emissionCountsA(qTo)( ObservedState(lastObsAString) ) = Maths.sumLogProb(
                emissionCountsA(qTo)( ObservedState(lastObsAString) ),
                thisTransitionCount
            )

            emissionCountsB(qTo)( ObservedState(lastObsBString) ) = Maths.sumLogProb(
                emissionCountsB(qTo)( ObservedState(lastObsBString) ),
                thisTransitionCount
            )
          }

        }
      }
    }

    //println( "\n\nAbout to hit the doozy?" )
    TwoOutputHMMPartialCounts(
      generalProbability( tokens ),
      initialStateCounts,
      transitionCounts,
      emissionCountsA,
      emissionCountsB
    )
  }

  def reestimate( corpus: List[List[ObservedStatePair]] ) = {
    import math.exp


    val corpusInitialStateCounts  = new HashMap[HiddenState,Double] {
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpusTransitionCounts = new HashMap[HiddenState,HashMap[HiddenState,Double]] {
      override def default( qFrom:HiddenState) = {
        this += Pair(
          qFrom,
          new HashMap[HiddenState,Double] {
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qFrom)
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
      val TwoOutputHMMPartialCounts(
        stringLogProb,
        initialStateCounts,
        transitionCounts,
        emissionCountsA,
        emissionCountsB
      ) = computePartialCounts( string )

      // Sum totals for this string

      transitionCounts.keySet.foreach{ qFrom =>

        corpusInitialStateCounts(qFrom) = Maths.sumLogProb(
            corpusInitialStateCounts(qFrom),
            initialStateCounts(qFrom)
        )

        transitionCounts(qFrom).keySet.foreach{ qTo =>
          corpusTransitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              corpusTransitionCounts(qFrom)(qTo),
              transitionCounts(qFrom)(qTo)
          )
        }

        emissionCountsA(qFrom).keySet.foreach{ obsA =>
          corpusEmissionCountsA(qFrom)(obsA) = Maths.sumLogProb(
              corpusEmissionCountsA(qFrom)(obsA),
              emissionCountsA(qFrom)(obsA)
          )
        }

        emissionCountsB(qFrom).keySet.foreach{ obsB =>
          corpusEmissionCountsB(qFrom)(obsB) = Maths.sumLogProb(
              corpusEmissionCountsB(qFrom)(obsB),
              emissionCountsB(qFrom)(obsB)
          )
        }
      }

      totalCorpusLogProb += stringLogProb
    }

    val initialStateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ qFrom =>
        qFrom -> 
          ( corpusInitialStateCounts( qFrom ) )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      corpusTransitionCounts.keySet.map{ qFrom =>
        qFrom -> HashMap(
          corpusTransitionCounts(qFrom).keySet.map{ qTo =>
            qTo -> (
              corpusTransitionCounts( qFrom )( qTo )
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


    initialStateProbabilities.setPT( initialStateProbs )
    transitionMatrix.setCPT( transitionProbs )
    emissionMatrixA.setCPT( emissionProbsA )
    emissionMatrixB.setCPT( emissionProbsB )
    normalize


    totalCorpusLogProb
  }

  override def toString =
    "  == HMM Parameters == \n" +
    "\nInitialProbabilities" +
    initialStateProbabilities +
    "\nTransitions:" +
    transitionMatrix +
    "\nEmissionsA" +
    emissionMatrixA
    "\nEmissionsB" +
    emissionMatrixB
}
