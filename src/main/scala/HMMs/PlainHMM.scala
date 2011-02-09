package ProsodicParsing.HMMs

import ProsodicParsing.types._
import ProsodicParsing.util.Util
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.ForwardBackwardInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer._
import scala.collection.immutable.{HashMap,HashSet}
import scala.collection.mutable.{HashMap => MHashMap}

class PlainHMM( hiddenStateTypesSet:Set[HiddenState], observationTypesSet:Set[ObservedState] )
  extends AbstractHMM[HiddenState,ObservedState] (hiddenStateTypesSet, observationTypesSet) {

  val observationAlphabet = new LabelAlphabet()
  val hiddenStateAlphabet = new LabelAlphabet()

  observationTypes.foreach( observationAlphabet.lookupIndex( _, true ) )

  val hiddenStateIndexToLabel = new MHashMap[Int,HiddenState]
  hiddenStateTypes.foreach( q =>
    hiddenStateIndexToLabel( hiddenStateAlphabet.lookupIndex( q, true ) ) = q
  )

  //def hiddenIndexToLabel( index:Int ) = hiddenStateIndexToLabel( index )

  def assignmentToViterbiString( maxAssn:Assignment ) =
    hiddenVariables.map{ hiddenVar =>
      hiddenStateIndexToLabel( maxAssn.get( hiddenVar ) )
    }.toList


  object TransitionMatrix extends ConditionalProbabilityDistribution[HiddenState,HiddenState] {
    // For now we'll initialize to a uniform transition matrix and define a
    // randomize method for people to have a random initialization whenever they
    // like
    var cpt = HashMap(
      hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              hiddenStateTypes.map( toStateName =>
                toStateName -> 1D/numHiddenStates ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object EmissionMatrix extends ConditionalProbabilityDistribution[HiddenState,ObservedState] {
    var cpt = HashMap(
      hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              observationTypes.map( toStateName =>
                toStateName -> 1D/observationTypes.size 
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object InitialStateProbabilities extends ProbabilityDistribution[HiddenState] {
    var pt = HashMap(
      hiddenStateTypes.map( thisStateName =>
        thisStateName -> 1D/hiddenStateTypes.size
      ).toSeq: _*
    )
  }

      // var parameters:Parameters = PlainHMMParameters(
      //   InitialStateProbabilities.pt,
      //   TransitionMatrix.cpt,
      //   EmissionMatrix.cpt
      // )
  var parameters = List( InitialStateProbabilities, TransitionMatrix, EmissionMatrix )

  def packageParameters = PlainHMMParameters(
    InitialStateProbabilities.pt,
    TransitionMatrix.cpt,
    EmissionMatrix.cpt
  )

  def setTransitionMatrix( newTransitions:HashMap[HiddenState,HashMap[HiddenState,Double]] ) {
    TransitionMatrix.setCPT( newTransitions )
  }

  def setEmissionMatrix( newEmissions:HashMap[HiddenState,HashMap[ObservedState,Double]] ) {
    EmissionMatrix.setCPT( newEmissions )
  }

  def setInitialProbs( newInitialProbs:HashMap[HiddenState,Double] ) {
    InitialStateProbabilities.setPT( newInitialProbs )
  }

  def setParams( newParams:Parameters ) {
    val PlainHMMParameters(
      initialProbs,
      transitions,
      emissions
    ) = newParams
    setInitialProbs( initialProbs )
    setTransitionMatrix( transitions )
    setEmissionMatrix( emissions )
  }

          // def randomize(n:Int) {
          //   TransitionMatrix.randomize(n)
          //   EmissionMatrix.randomize(n)
          //   InitialStateProbabilities.randomize(n)
          // }

          // def normalize {
          //   TransitionMatrix.normalize
          //   EmissionMatrix.normalize
          //   InitialStateProbabilities.normalize
          // }

          // var hmm = new DirectedModel()
          // var hiddenVariables:Array[Variable] = Array()
          // var observations:Array[Variable] = Array()

  def buildSlicedHMM( tokens:List[ObservedState] ) {
    // clear hmm this way; hmm.clear() breaks something.
    //hmm = new DirectedModel()
    hmm = new DynamicBayesNet(tokens.size)


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }


    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          (InitialStateProbabilities * TransitionMatrix ).toArray
        ),
        hiddenVariables(1)
      ),
      0
    )
    // state transitions
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
              TransitionMatrix.toArray
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
          new TableFactor(
            Array( hiddenVariables(i), observations(i) ),
            EmissionMatrix.toArray
          ),
          observations(i),
          thisObservation
        ),
        i
      )
    }
  }

  def buildHMM( tokens:List[ObservedState] ) {
    // clear hmm this way; hmm.clear() breaks something.
    //hmm = new DirectedModel()
    hmm = new DynamicBayesNet(tokens.size)


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }

    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          (InitialStateProbabilities * TransitionMatrix ).toArray
        ),
        hiddenVariables(1)
      ),
      0
    )
    // state transitions
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
              TransitionMatrix.toArray
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
          new TableFactor(
            Array( hiddenVariables(i), observations(i) ),
            EmissionMatrix.toArray
          ),
          observations(i)
        ),
        i
      )
    }
  }


  def generateObservationSequence( tokens:List[ObservedState] ) = new Assignment(
      observations, tokens.map( w => observationAlphabet.lookupIndex( w ) ).toArray
    )


  def computePartialCounts( sequence:List[ObservedState] ) = {
    val stringLogProb = generalProbability( sequence )
    buildSlicedHMM( sequence )


    val inferencer = new ForwardBackwardInferencer()
    //println( "calling computeMarginals from within computePartialCounts directly" );
    inferencer.computeMarginals( hmm )

    val initialStateCounts = MHashMap(
      hiddenStateTypes.map{ _ -> Double.NegativeInfinity }.toSeq:_*
    )

    val emissionCounts = MHashMap(
      hiddenStateTypes.map{ q =>
        q -> MHashMap(
          observationTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCounts = MHashMap(
      hiddenStateTypes.map{ qFrom =>
        qFrom -> MHashMap(
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


          transitionCounts(qFrom)(qTo) = Util.log_add(
            List(
              transitionCounts(qFrom)(qTo),
              thisTransitionCount
            )
          )

          if( i == 0 )
            initialStateCounts(qFrom) = Util.log_add(
              List(
                initialStateCounts(qFrom),
                thisTransitionCount
              )
            )


          emissionCounts(qFrom)(sequence(i)) = Util.log_add(
            List(
              emissionCounts(qFrom)(sequence(i)),
              thisTransitionCount
            )
          )

          if( i == (hiddenVariables.size-2) ) {
            emissionCounts(qTo)(sequence(i+1)) = Util.log_add(
              List(
                emissionCounts(qTo)(sequence(i+1)),
                thisTransitionCount
              )
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
    val corpusInitialStateCounts = new MHashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpusTransitionCounts = new MHashMap[HiddenState,MHashMap[HiddenState,Double]]{
      override def default( qFrom:HiddenState ) = {
        this += Pair(
          qFrom,
          new MHashMap[HiddenState,Double]{
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qFrom)
      }
    }

    val corpusTransitionDenominator = new MHashMap[HiddenState,Double]{
      override def default( qFrom:HiddenState ) = Double.NegativeInfinity
    }

    val corpusEmissionCounts = new MHashMap[HiddenState,MHashMap[ObservedState,Double]]{
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

    val corpusEmissionDenominator = new MHashMap[HiddenState,Double]{
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

      val stringInitialStateCounts = new MHashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      val stringTransitionCounts = new MHashMap[HiddenState,MHashMap[HiddenState,Double]]{
        override def default( qFrom:HiddenState ) = {
          this += Pair(
            qFrom,
            new MHashMap[HiddenState,Double]{
              override def default( qTo:HiddenState ) = {
                this += Pair( qTo, Double.NegativeInfinity )
                this(qTo)
              }
            }
          )
          this(qFrom)
        }
      }

      val stringTransitionDenominator = new MHashMap[HiddenState,Double]{
        override def default( qFrom:HiddenState ) = Double.NegativeInfinity
      }

      val stringEmissionCounts = new MHashMap[HiddenState,MHashMap[ObservedState,Double]]{
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

      val stringEmissionDenominator = new MHashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      // sum over strings, scaling the numerator and denominator summands by the string probability
      // initialStateCounts.keySet.foreach{ q =>
      // }

      transitionCounts.keySet.foreach{ qFrom =>
        transitionCounts(qFrom).keySet.foreach{ qTo =>
          stringTransitionDenominator(qFrom) = Util.log_add(
            List(
              stringTransitionDenominator(qFrom),
              transitionCounts(qFrom)(qTo)
            )
          )
          stringTransitionCounts(qFrom)(qTo) = Util.log_add(
            List(
              stringTransitionCounts(qFrom)(qTo),
              transitionCounts(qFrom)(qTo)
            )
          )
        }
        stringInitialStateDenominator = Util.log_add(
          List(
            stringInitialStateDenominator,
            initialStateCounts(qFrom)
          )
        )
        stringInitialStateCounts(qFrom) = Util.log_add(
          List(
            stringInitialStateCounts(qFrom),
            initialStateCounts(qFrom)
          )
        )
        emissionCounts(qFrom).keySet.foreach{ o =>
          stringEmissionDenominator(qFrom) = Util.log_add(
            List(
              stringEmissionDenominator(qFrom),
              emissionCounts(qFrom)(o)
            )
          )
          stringEmissionCounts(qFrom)(o) = Util.log_add(
            List(
              stringEmissionCounts(qFrom)(o),
              emissionCounts(qFrom)(o)
            )
          )
        }
      }

      // emissionCounts.keySet.foreach{ q =>
      // }




      // stringInitialStateCounts.keySet.foreach{ q =>
      // }

      corpusInitialStateDenominator = Util.log_add(
        List(
          corpusInitialStateDenominator,
          stringInitialStateDenominator //- stringLogProb
        )
      )


      stringTransitionCounts.keySet.foreach{ qFrom =>
        stringTransitionCounts(qFrom).keySet.foreach{ qTo =>
          corpusTransitionCounts(qFrom)(qTo) = Util.log_add(
            List(
              corpusTransitionCounts(qFrom)(qTo),
              stringTransitionCounts(qFrom)(qTo) - stringLogProb
            )
          )
        }
        corpusInitialStateCounts(qFrom) = Util.log_add(
          List(
            corpusInitialStateCounts(qFrom),
            stringInitialStateCounts(qFrom) //- stringLogProb
          )
        )
        corpusTransitionDenominator(qFrom) = Util.log_add(
          List(
            corpusTransitionDenominator(qFrom),
            stringTransitionDenominator(qFrom) - stringLogProb
          )
        )
        stringEmissionCounts(qFrom).keySet.foreach{ obs =>
          corpusEmissionCounts(qFrom)(obs) = Util.log_add(
            List(
              corpusEmissionCounts(qFrom)(obs),
              stringEmissionCounts(qFrom)(obs) //- stringLogProb
            )
          )
        }
        corpusEmissionDenominator(qFrom) = Util.log_add(
          List(
            corpusEmissionDenominator(qFrom),
            stringEmissionDenominator(qFrom) //- stringLogProb
          )
        )
      }

      // stringTransitionDenominator.keySet.foreach{ qFrom =>
      // }


      // stringEmissionCounts.keySet.foreach{ q =>
      // }

      // stringEmissionDenominator.keySet.foreach{ qFrom =>
      // }

      totalCorpusLogProb += stringLogProb
    }

    //val corpusInitialStateCountsTotal = corpusInitialStateCounts.values.sum
    val stateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ q =>
        q -> (
          exp( corpusInitialStateCounts( q ) - corpusInitialStateDenominator )
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      corpusTransitionCounts.keySet.map{ qFrom =>
        qFrom -> HashMap(
          corpusTransitionCounts(qFrom).keySet.map{ qTo =>
            qTo -> (
              exp( corpusTransitionCounts( qFrom )( qTo ) - corpusTransitionDenominator( qFrom ) )
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
              exp( corpusEmissionCounts( q )( obs ) - corpusEmissionDenominator( q ) )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    setEmissionMatrix( emissionProbs )
    setTransitionMatrix( transitionProbs )
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


    val initialStateCountsTotal = initialStateCounts.values.reduceLeft( (a,b) => Util.log_add( a::b::Nil ) )
    val stateProbs = HashMap(
      initialStateCounts.keySet.map{ q =>
        q -> (
          exp( initialStateCounts( q ) - initialStateCountsTotal )
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      transitionCounts.keySet.map{ qFrom =>
        val qFromTotal = transitionCounts(qFrom).values.reduceLeft( (a,b) => Util.log_add( a::b::Nil ) )
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
        val qTotal = emissionCounts(q).values.reduceLeft( (a,b) => Util.log_add( a::b::Nil ) )
        q -> HashMap(
          emissionCounts(q).keySet.map{ obs =>
            obs -> (
              exp( emissionCounts( q )( obs ) -  qTotal )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    setEmissionMatrix( emissionProbs )
    setTransitionMatrix( transitionProbs )
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
        q -> InitialStateProbabilities(q) * EmissionMatrix(q)(string.head)
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
                  qFrom -> TransitionMatrix(qFrom)(qTo) * lastDelta(qFrom)
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
              lastDelta(qFrom) * TransitionMatrix(qFrom)(qTo)
            }
          ).max * EmissionMatrix(qTo)(w)
        }.toSeq:_*
      )
      println( "simple argmax at w = "+w+": " + argmax( lastDelta ) )
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

  def viterbi_old( s: List[ObservedState] ) = {
    def argmax( h:HashMap[HiddenState,Double] ):HiddenState =
      h.keySet.reduceLeft{ (p, q) => if( h(p) > h(q) ) p else q }

    def viterbi_aux(
      backtrace:List[HashMap[HiddenState,HiddenState]], // psi
      previousState:HashMap[HiddenState,Double], // delta
      remaining:List[ObservedState]
    ):List[HashMap[HiddenState,HiddenState]] = {
      if( remaining == Nil )
        backtrace :+
         HashMap(
           argmax( previousState ) -> argmax( previousState )
         )
      else
        viterbi_aux(
          backtrace :+
          HashMap(
            hiddenStateTypes.map{ to =>
              to -> argmax(
                HashMap(
                  previousState.keySet.map{ from =>
                    from -> previousState(from) * TransitionMatrix(from)(to)
                  }.toSeq:_*
                )
              )
            }.toSeq:_*
          ),
          HashMap(
            hiddenStateTypes.map{ to =>
              to -> (
                previousState.keySet.map{ from =>
                  previousState(from) * TransitionMatrix(from)(to)
                }.max
              ) * EmissionMatrix(to)(remaining.head)
            }.toSeq:_*
          ),
          remaining.tail
        )
    }

    def backtraceReadout( backtrace: List[HashMap[HiddenState,HiddenState]] ) = {
      def backtraceReadout_aux(
        optimalSequence:List[HiddenState],
        bestToState:HiddenState,
        bt_remaining:List[HashMap[HiddenState,HiddenState]]
      ):List[HiddenState] =
        if( bt_remaining == Nil )
          bestToState::optimalSequence
        else
          backtraceReadout_aux(
            bestToState::optimalSequence,
            bt_remaining.last(bestToState),
            bt_remaining.init
          )

        assert( backtrace.last.keySet.toList.size == 1 )
        backtraceReadout_aux(
          Nil,
          backtrace.last.keySet.toList(0),
          backtrace.init
        )
    }

    backtraceReadout(
      viterbi_aux(
        Nil,
        HashMap(
          hiddenStateTypes.map{ q =>
            q -> InitialStateProbabilities(q) * EmissionMatrix(q)(s.head)
          }.toSeq:_*
        ),
        s.tail
      )
    )
  }


  def totalProbability( allObservations:List[ObservedState] ):Double = {
    def forwardPass( allObservations:List[ObservedState] ) = {
      var lastAlphas =
        HashMap(
          hiddenStateTypes.map{ q =>
             q -> InitialStateProbabilities( q ) * EmissionMatrix(q)(allObservations(0))
          }.toSeq:_*
        )

      (allObservations.tail).foreach{ obs =>
        lastAlphas = HashMap(
          hiddenStateTypes.map{ qTo =>
            qTo -> {
              hiddenStateTypes.map{ qFrom =>
                TransitionMatrix(qFrom)(qTo) * lastAlphas(qFrom)
              }.sum
            } *
            EmissionMatrix(qTo)(obs)
          }.toSeq:_*
        )
      }
      lastAlphas
    }

    forwardPass( allObservations ).values.sum
  }

  def marginalsForString( string:List[ObservedState] ) {
    buildSlicedHMM( string )
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
    InitialStateProbabilities +
    "\nTransitions:" +
    TransitionMatrix +
    "\nEmissions" +
    EmissionMatrix
}

