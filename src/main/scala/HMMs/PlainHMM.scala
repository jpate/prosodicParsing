package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.ForwardBackwardInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer._
import cc.mallet.util.Maths
//import scala.collection.immutable.{HashMap,HashSet}
import scala.collection.mutable.{HashMap,HashSet}
//import scala.collection.mutable.{HashMap => HashMap}

class PlainHMM(
  hiddenStateTypesSet:Set[HiddenState],
  observationTypesSet:Set[ObservedState],
  idString:String
) extends AbstractHMM[HiddenState,ObservedState] (hiddenStateTypesSet, observationTypesSet) {

  val hmmID = idString

  val observationAlphabet = new LabelAlphabet()
  val hiddenStateAlphabet = new LabelAlphabet()

  observationTypes.foreach( observationAlphabet.lookupIndex( _, true ) )

  val hiddenStateIndexToLabel = new HashMap[Int,HiddenState]
  hiddenStateTypes.foreach( q =>
    hiddenStateIndexToLabel( hiddenStateAlphabet.lookupIndex( q, true ) ) = q
  )

  //def hiddenIndexToLabel( index:Int ) = hiddenStateIndexToLabel( index )

  def assignmentToViterbiString( maxAssn:Assignment ) = {
    hiddenVariables.map{ hiddenVar =>
      hiddenStateIndexToLabel( maxAssn.get( hiddenVar ) )
    }.toList
  }


  val transitionMatrix =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddenStateTypesSet )

  var emissionMatrix =
    new ConditionalLogProbabilityDistribution ( hiddenStateTypesSet, observationTypesSet )

  val initialStateProbabilities = new LogProbabilityDistribution( hiddenStateTypesSet )

  def parameters = List( initialStateProbabilities, transitionMatrix, emissionMatrix )

  def packageParameters = PlainHMMParameters(
    initialStateProbabilities.pt,
    transitionMatrix.cpt,
    emissionMatrix.cpt
  )

  def settransitionMatrix( newTransitions:HashMap[HiddenState,HashMap[HiddenState,Double]] ) {
    transitionMatrix.setCPT( newTransitions )
  }

  def setemissionMatrix( newEmissions:HashMap[HiddenState,HashMap[ObservedState,Double]] ) {
    emissionMatrix.setCPT( newEmissions )
  }

  def setInitialProbs( newInitialProbs:HashMap[HiddenState,Double] ) {
    initialStateProbabilities.setPT( newInitialProbs )
  }

  def initialPartialCounts = PlainHMMPartialCounts(
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
              hiddenStateTypes.map( toStateName =>
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
              observationTypes.map( obs =>
                obs -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
    )

  def setParams( newParams:Parameters ) {
    val PlainHMMParameters(
      initialProbs,
      transitions,
      emissions
    ) = newParams
    setInitialProbs( initialProbs )
    settransitionMatrix( transitions )
    setemissionMatrix( emissions )
  }


  def buildSlicedHMM( tokens:List[ObservedState] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet(tokens.size)


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( localUniverse, hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( localUniverse, observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }


    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        LogTableFactor.makeFromLogValues(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          transitionMatrix.toLogArray
        ),
        hiddenVariables(1)
      ),
      0
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVariables(0) ),
        initialStateProbabilities.toLogArray
      )
    )

    // state transitions
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
    ( 0 to tokens.size-1 ) foreach { i =>
      val thisObservation = new Assignment(
        observations(i),
        observationAlphabet.lookupIndex( tokens(i) )
      )

      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i), observations(i) ),
            emissionMatrix.toLogArray
          ),
          observations(i),
          thisObservation
        ),
        i
      )
    }
  }

  def buildHMM( tokens:List[ObservedState] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet(tokens.size)


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( localUniverse, hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( localUniverse, observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }

    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        LogTableFactor.makeFromLogValues(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          transitionMatrix.toLogArray
        ),
        hiddenVariables(1)
      ),
      0
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVariables(0) ),
        initialStateProbabilities.toLogArray
      )
    )



    // state transitions
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
    ( 0 to tokens.size-1 ) foreach { i =>
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i), observations(i) ),
            emissionMatrix.toLogArray
          ),
          observations(i)
        ),
        i
      )
    }
  }


  def generateObservationSequence( tokens:List[ObservedState] ) = new Assignment(
      observations, tokens.map{ w =>
        observationAlphabet.lookupIndex( w )
      }.toArray
    )


  def computePartialCounts( sequence:List[ObservedState] ) = {
    val stringLogProb = generalProbability( sequence )
    buildSlicedHMM( sequence )
    //buildHMM( sequence )


    val inferencer = new ForwardBackwardInferencer()
    //println( "calling computeMarginals from within computePartialCounts directly" );
    //inferencer.computeMarginals( hmm )

    inferencer.computeMarginals( hmm )

    val initialStateCounts = HashMap(
      hiddenStateTypes.map{ _ -> Double.NegativeInfinity }.toSeq:_*
    )

    val emissionCounts = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          observationTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCounts = HashMap(
      hiddenStateTypes.map{ qFrom =>
        qFrom -> HashMap(
          hiddenStateTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    // loop through all transitions
    //( hiddenVariables.init zip hiddenVariables.tail ) foreach { case ( fromVar, toVar ) =>
    ( 0 to (hiddenVariables.size-2) ) foreach { i =>

      val (fromVar, toVar) = (hiddenVariables(i), hiddenVariables(i+1) )

      val thisTransition = inferencer.lookupMarginal( new HashVarSet( Array( fromVar, toVar ) ) )
      hiddenStateTypes foreach { qFrom =>
        hiddenStateTypes foreach { qTo =>


          val thisTransitionCount = thisTransition.logValue(
            new Assignment(
              Array( fromVar, toVar ),
              Array(
                hiddenStateAlphabet.lookupIndex( qFrom ),
                hiddenStateAlphabet.lookupIndex( qTo )
              )
            )
          )


          transitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              transitionCounts(qFrom)(qTo),
              thisTransitionCount
          )

          if( i == 0 )
            initialStateCounts(qFrom) = Maths.sumLogProb(
                initialStateCounts(qFrom),
                thisTransitionCount
            )


          emissionCounts(qFrom)(sequence(i)) = Maths.sumLogProb(
              emissionCounts(qFrom)(sequence(i)),
              thisTransitionCount
          )

          if( i == (hiddenVariables.size-2) ) {
            emissionCounts(qTo)(sequence(i+1)) = Maths.sumLogProb(
                emissionCounts(qTo)(sequence(i+1)),
                thisTransitionCount
            )
          }
        }
      }
    }

    PlainHMMPartialCounts(
      generalProbability( sequence ),
      HashMap(
        initialStateCounts.keySet.map{ q =>
          q -> initialStateCounts(q)
        }.toSeq:_*
      ),
      HashMap(
        transitionCounts.keySet.map{ from =>
          from -> HashMap(
            transitionCounts(from).keySet.map{ to =>
              to -> transitionCounts(from)(to)
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCounts.keySet.map{ q =>
          q -> HashMap(
            emissionCounts(q).keySet.map{ obs =>
              obs -> emissionCounts(q)(obs)
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }


  def reestimate( corpus:List[List[ObservedState]] ) = {
    import math.{log,exp}

    var corpusInitialStateDenominator = Double.NegativeInfinity
    val corpusInitialStateCounts = new HashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpusTransitionCounts = new HashMap[HiddenState,HashMap[HiddenState,Double]]{
      override def default( qFrom:HiddenState ) = {
        this += Pair(
          qFrom,
          new HashMap[HiddenState,Double]{
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qFrom)
      }
    }

    val corpusTransitionDenominator = new HashMap[HiddenState,Double]{
      override def default( qFrom:HiddenState ) = Double.NegativeInfinity
    }

    val corpusEmissionCounts = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
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

    val corpusEmissionDenominator = new HashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    var totalCorpusLogProb = 0D

    var n = 0
    corpus.foreach{ string =>
      // sums over time
      val PlainHMMPartialCounts(
        stringLogProb,
        initialStateCounts,
        transitionCounts,
        emissionCounts
      ) = computePartialCounts( string ) 


      var stringInitialStateDenominator = Double.NegativeInfinity

      val stringInitialStateCounts = new HashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      val stringTransitionCounts = new HashMap[HiddenState,HashMap[HiddenState,Double]]{
        override def default( qFrom:HiddenState ) = {
          this += Pair(
            qFrom,
            new HashMap[HiddenState,Double]{
              override def default( qTo:HiddenState ) = {
                this += Pair( qTo, Double.NegativeInfinity )
                this(qTo)
              }
            }
          )
          this(qFrom)
        }
      }

      val stringTransitionDenominator = new HashMap[HiddenState,Double]{
        override def default( qFrom:HiddenState ) = Double.NegativeInfinity
      }

      val stringEmissionCounts = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
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

      val stringEmissionDenominator = new HashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      // sum over strings, scaling the numerator and denominator summands by the string probability
      // initialStateCounts.keySet.foreach{ q =>
      // }

      transitionCounts.keySet.foreach{ qFrom =>
        transitionCounts(qFrom).keySet.foreach{ qTo =>
          stringTransitionDenominator(qFrom) = Maths.sumLogProb(
              stringTransitionDenominator(qFrom),
              transitionCounts(qFrom)(qTo)
          )
          stringTransitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              stringTransitionCounts(qFrom)(qTo),
              transitionCounts(qFrom)(qTo)
          )
        }
        stringInitialStateDenominator = Maths.sumLogProb(
            stringInitialStateDenominator,
            initialStateCounts(qFrom)
        )
        stringInitialStateCounts(qFrom) = Maths.sumLogProb(
            stringInitialStateCounts(qFrom),
            initialStateCounts(qFrom)
        )
        emissionCounts(qFrom).keySet.foreach{ o =>
          stringEmissionDenominator(qFrom) = Maths.sumLogProb(
              stringEmissionDenominator(qFrom),
              emissionCounts(qFrom)(o)
          )
          stringEmissionCounts(qFrom)(o) = Maths.sumLogProb(
              stringEmissionCounts(qFrom)(o),
              emissionCounts(qFrom)(o)
          )
        }
      }

      // emissionCounts.keySet.foreach{ q =>
      // }




      // stringInitialStateCounts.keySet.foreach{ q =>
      // }

      corpusInitialStateDenominator = Maths.sumLogProb(
          corpusInitialStateDenominator,
          stringInitialStateDenominator //- stringLogProb
      )


      stringTransitionCounts.keySet.foreach{ qFrom =>
        stringTransitionCounts(qFrom).keySet.foreach{ qTo =>
          corpusTransitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              corpusTransitionCounts(qFrom)(qTo),
              stringTransitionCounts(qFrom)(qTo) //- stringLogProb
          )
        }
        corpusInitialStateCounts(qFrom) = Maths.sumLogProb(
            corpusInitialStateCounts(qFrom),
            stringInitialStateCounts(qFrom) //- stringLogProb
        )
        corpusTransitionDenominator(qFrom) = Maths.sumLogProb(
            corpusTransitionDenominator(qFrom),
            stringTransitionDenominator(qFrom) //- stringLogProb
        )
        stringEmissionCounts(qFrom).keySet.foreach{ obs =>
          corpusEmissionCounts(qFrom)(obs) = Maths.sumLogProb(
              corpusEmissionCounts(qFrom)(obs),
              stringEmissionCounts(qFrom)(obs) //- stringLogProb
          )
        }
        corpusEmissionDenominator(qFrom) = Maths.sumLogProb(
            corpusEmissionDenominator(qFrom),
            stringEmissionDenominator(qFrom) //- stringLogProb
        )
      }


      totalCorpusLogProb += stringLogProb
    }

    //val corpusInitialStateCountsTotal = corpusInitialStateCounts.values.sum
    val stateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ q =>
        q -> (
          corpusInitialStateCounts( q ) - corpusInitialStateDenominator
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      corpusTransitionCounts.keySet.map{ qFrom =>
        qFrom -> HashMap(
          corpusTransitionCounts(qFrom).keySet.map{ qTo =>
            qTo -> (
              corpusTransitionCounts( qFrom )( qTo ) - corpusTransitionDenominator( qFrom )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionProbs = HashMap(
      corpusEmissionCounts.keySet.map{ q =>
        q -> HashMap(
          corpusEmissionCounts(q).keySet.map{ obs =>
            obs -> (
              corpusEmissionCounts( q )( obs ) - corpusEmissionDenominator( q )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    setemissionMatrix( emissionProbs )
    settransitionMatrix( transitionProbs )
    setInitialProbs( stateProbs )

    //normalize

    totalCorpusLogProb
    //corpus.map( s => math.log( generalProbability( s ) ) ).sum
  }



  def reestimateSingle( sequence:List[ObservedState] ) = {
    import math.{log,exp}
    val PlainHMMPartialCounts(
      totalLogProb,
      initialStateCounts,
      transitionCounts,
      emissionCounts
    ) = computePartialCounts( sequence ) 


    val initialStateCountsTotal =
      initialStateCounts.values.reduceLeft( (a,b) => Maths.sumLogProb( a, b ) )
    val stateProbs = HashMap(
      initialStateCounts.keySet.map{ q =>
        q -> (
          exp( initialStateCounts( q ) - initialStateCountsTotal )
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      transitionCounts.keySet.map{ qFrom =>
        val qFromTotal =
          transitionCounts(qFrom).values.reduceLeft( (a,b) => Maths.sumLogProb( a, b ) )
        qFrom -> HashMap(
          transitionCounts(qFrom).keySet.map{ qTo =>
            qTo -> (
              exp( transitionCounts( qFrom )( qTo ) - qFromTotal )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionProbs = HashMap(
      emissionCounts.keySet.map{ q =>
        val qTotal = emissionCounts(q).values.reduceLeft( (a,b) => Maths.sumLogProb( a, b ) )
        q -> HashMap(
          emissionCounts(q).keySet.map{ obs =>
            obs -> (
              exp( emissionCounts( q )( obs ) -  qTotal )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    setemissionMatrix( emissionProbs )
    settransitionMatrix( transitionProbs )
    setInitialProbs( stateProbs )


    totalLogProb
  }


  def argMaxUsingCreateForMaxProduct( s: List[ObservedState] ) = {
    val inferencer = JunctionTreeInferencer.createForMaxProduct()
    inferencer.computeMarginals( hmm )
    hiddenVariables foreach ( someHiddenVar =>
      println( inferencer.lookupMarginal( someHiddenVar ).dumpToString() )
    )
  }

  def viterbi( string:List[ObservedState] ) = {
    def argmax( h:HashMap[HiddenState,Double] ):HiddenState =
      h.keySet.reduceLeft{ (p, q) => if( h(p) > h(q) ) p else q }

    //initialize deltas
    var lastDelta = HashMap(
      hiddenStateTypes.map{ q =>
        q -> initialStateProbabilities(q) * emissionMatrix(q)(string.head)
      }.toSeq:_*
    )

    //println( "simple argmax at w = "+string.head+": " + argmax( lastDelta ) )

    var psis = new Array[HashMap[HiddenState,HiddenState]](0)
    (string.tail) foreach { w =>
      // Keep track of best ways to get wherever we want to go
      psis = psis ++ Array(
        HashMap(
          hiddenStateTypes.map{ qTo =>
            // For each state, record the best way to get to that state.
            qTo -> argmax(
              HashMap(
                hiddenStateTypes.map{ qFrom =>
                  qFrom -> transitionMatrix(qFrom)(qTo) * lastDelta(qFrom)
                }.toSeq:_*
              )
            )
          }.toSeq:_*
        )
      )


      // update deltas
      lastDelta = HashMap(
        hiddenStateTypes.map{ qTo =>
          qTo -> (
            hiddenStateTypes.map{ qFrom =>
              lastDelta(qFrom) * transitionMatrix(qFrom)(qTo)
            }
          ).max * emissionMatrix(qTo)(w)
        }.toSeq:_*
      )
    }

    val bestLastState = argmax( lastDelta )

    var bestPath = Array[HiddenState]( bestLastState )
    (psis reverse).foreach{ psi =>
      bestPath = Array(
        psi(bestPath head)
      ) ++ bestPath
    }

    bestPath
  }

  def totalProbability( allObservations:List[ObservedState] ):Double = {
    def forwardPass( allObservations:List[ObservedState] ) = {
      var lastAlphas =
        HashMap(
          hiddenStateTypes.map{ q =>
             q -> initialStateProbabilities( q ) * emissionMatrix(q)(allObservations(0))
          }.toSeq:_*
        )

      (allObservations.tail).foreach{ obs =>
        lastAlphas = HashMap(
          hiddenStateTypes.map{ qTo =>
            qTo -> {
              hiddenStateTypes.map{ qFrom =>
                transitionMatrix(qFrom)(qTo) * lastAlphas(qFrom)
              }.sum
            } *
            emissionMatrix(qTo)(obs)
          }.toSeq:_*
        )
      }
      lastAlphas
    }

    forwardPass( allObservations ).values.sum
  }

  def marginalsForString( string:List[ObservedState] ) {
    buildSlicedHMM( string )
    //buildHMM( string )
    val inferencer = new ForwardBackwardInferencer()
    inferencer.computeMarginals( hmm )

    hiddenVariables foreach ( someHiddenVar =>
      println( inferencer.lookupMarginal( someHiddenVar ).dumpToString() )
    )
  }

  def seeMarginals() {
    val inferencer = new ForwardBackwardInferencer()
    inferencer.computeMarginals( hmm )

    hiddenVariables foreach ( someHiddenVar =>
      println( inferencer.lookupMarginal( someHiddenVar ).dumpToString() )
    )
  }

  override def toString =
    "  == HMM Parameters == \n" +
    "\nInitialProbabilities" +
    initialStateProbabilities +
    "\nTransitions:" +
    transitionMatrix +
    "\nEmissions" +
    emissionMatrix
}

