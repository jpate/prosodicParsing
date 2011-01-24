package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.JunctionTreeInferencer
import scala.collection.immutable.{HashMap,HashSet}
import scala.collection.mutable.{HashMap => MHashMap}

class PlainHMM( hiddenStateTypesSet:Set[HiddenState], observationTypesSet:Set[ObservedState] )
  extends AbstractHMM[HiddenState,ObservedState] {
  val observationTypes = observationTypesSet.toList.sortWith( (a,b) => a < b )
  observationTypes.foreach( observationAlphabet.lookupIndex( _, true ) )

  val hiddenStateTypes = hiddenStateTypesSet.toList.sortWith( (a,b) => a < b )
  hiddenStateTypes.foreach( hiddenStateAlphabet.lookupIndex( _, true ) )

  val numHiddenStates = hiddenStateTypes.size

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

  var parameters = Set( InitialStateProbabilities, TransitionMatrix, EmissionMatrix )

  def setTransitionMatrix( newTransitions:HashMap[HiddenState,HashMap[HiddenState,Double]] ) {
    TransitionMatrix.setCPT( newTransitions )
  }

  def setEmissionMatrix( newEmissions:HashMap[HiddenState,HashMap[ObservedState,Double]] ) {
    EmissionMatrix.setCPT( newEmissions )
  }

  def setInitialProbs( newInitialProbs:HashMap[HiddenState,Double] ) {
    InitialStateProbabilities.setPT( newInitialProbs )
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
    hmm = new DirectedModel()


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }


    // initial states:
    hmm.addFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          (InitialStateProbabilities * TransitionMatrix ).toArray
        ),
        hiddenVariables(1)
      )
    )
    // state transitions
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
              TransitionMatrix.toArray
          ),
          hiddenVariables(i)
        )
      )
    }

    // emissions
    ( 0 to tokens.size-1 ) foreach { i =>
      val thisObservation = new Assignment(
        observations(i),
        observationAlphabet.lookupIndex( tokens(i) )
      )

      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i), observations(i) ),
            EmissionMatrix.toArray
          ),
          observations(i),
          thisObservation
        )
      )
    }
  }

  def buildHMM( tokens:List[ObservedState] ) {
    // clear hmm this way; hmm.clear() breaks something.
    hmm = new DirectedModel()


    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( hiddenStateAlphabet ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( observationAlphabet ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }

    // initial states:
    hmm.addFactor(
      new CPT(
        new TableFactor(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          (InitialStateProbabilities * TransitionMatrix ).toArray
        ),
        hiddenVariables(1)
      )
    )
    // state transitions
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
              TransitionMatrix.toArray
          ),
          hiddenVariables(i)
        )
      )
    }

    // emissions
    ( 0 to tokens.size-1 ) foreach { i =>
      val thisObservation = new Assignment(
        observations(i),
        observationAlphabet.lookupIndex( tokens(i) )
      )

      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array( hiddenVariables(i), observations(i) ),
            EmissionMatrix.toArray
          ),
          observations(i)
        )
      )
    }
  }

  def computePartialCounts( sequence:List[ObservedState] ) = {
    import math.log
    buildSlicedHMM( sequence )


    val inferencer = new JunctionTreeInferencer()
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


          transitionCounts(qFrom)(qTo) = log_add(
            List(
              transitionCounts(qFrom)(qTo),
              thisTransitionCount
            )
          )

          if( i == 0 )
            initialStateCounts(qFrom) = log_add(
              List(
                initialStateCounts(qFrom),
                thisTransitionCount
              )
            )




          emissionCounts(qFrom)(sequence(i)) = log_add(
            List(
              emissionCounts(qFrom)(sequence(i)),
              thisTransitionCount
            )
          )

          if( i ==  (hiddenVariables.size-2) ) {
            emissionCounts(qTo)(sequence(i+1)) = log_add(
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
          stringTransitionDenominator(qFrom) = log_add(
            List(
              stringTransitionDenominator(qFrom),
              transitionCounts(qFrom)(qTo)
            )
          )
          stringTransitionCounts(qFrom)(qTo) = log_add(
            List(
              stringTransitionCounts(qFrom)(qTo),
              transitionCounts(qFrom)(qTo)
            )
          )
        }
        stringInitialStateDenominator = log_add(
          List(
            stringInitialStateDenominator,
            initialStateCounts(qFrom)
          )
        )
        stringInitialStateCounts(qFrom) = log_add(
          List(
            stringInitialStateCounts(qFrom),
            initialStateCounts(qFrom)
          )
        )
        emissionCounts(qFrom).keySet.foreach{ o =>
          stringEmissionDenominator(qFrom) = log_add(
            List(
              stringEmissionDenominator(qFrom),
              emissionCounts(qFrom)(o)
            )
          )
          stringEmissionCounts(qFrom)(o) = log_add(
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

      corpusInitialStateDenominator = log_add(
        List(
          corpusInitialStateDenominator,
          stringInitialStateDenominator //- stringLogProb
        )
      )


      stringTransitionCounts.keySet.foreach{ qFrom =>
        stringTransitionCounts(qFrom).keySet.foreach{ qTo =>
          corpusTransitionCounts(qFrom)(qTo) = log_add(
            List(
              corpusTransitionCounts(qFrom)(qTo),
              stringTransitionCounts(qFrom)(qTo) //- stringLogProb
            )
          )
        }
        corpusInitialStateCounts(qFrom) = log_add(
          List(
            corpusInitialStateCounts(qFrom),
            stringInitialStateCounts(qFrom) //- stringLogProb
          )
        )
        corpusTransitionDenominator(qFrom) = log_add(
          List(
            corpusTransitionDenominator(qFrom),
            stringTransitionDenominator(qFrom) //- stringLogProb
          )
        )
        stringEmissionCounts(qFrom).keySet.foreach{ obs =>
          corpusEmissionCounts(qFrom)(obs) = log_add(
            List(
              corpusEmissionCounts(qFrom)(obs),
              stringEmissionCounts(qFrom)(obs) //- stringLogProb
            )
          )
        }
        corpusEmissionDenominator(qFrom) = log_add(
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
        //val qFromTotal = corpusTransitionCounts(qFrom).values.sum
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
        //val qTotal = corpusEmissionCounts(q).values.sum
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


    val initialStateCountsTotal = initialStateCounts.values.reduceLeft( (a,b) => log_add( a::b::Nil ) )
    val stateProbs = HashMap(
      initialStateCounts.keySet.map{ q =>
        q -> (
          exp( initialStateCounts( q ) - initialStateCountsTotal )
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      transitionCounts.keySet.map{ qFrom =>
        val qFromTotal = transitionCounts(qFrom).values.reduceLeft( (a,b) => log_add( a::b::Nil ) )
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
        val qTotal = emissionCounts(q).values.reduceLeft( (a,b) => log_add( a::b::Nil ) )
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


  def viterbi( s: List[ObservedState] ) = {
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


  def seeMarginals() {
    val inferencer = new JunctionTreeInferencer()
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

