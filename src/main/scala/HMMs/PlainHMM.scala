
package prosodicParsing.HMMs

import collection.immutable.HashMap
import prosodicParsing.types._

class PlainHMM extends AbstractHMMActor[HiddenState,Observation,PlainHMMParameters] {

  var initialStateProbs = new HashMap[HiddenState, Double] {
    override def default( q:HiddenState ) = 0D
  }

  var transitions = new HashMap[ HiddenState, HashMap[HiddenState, Double] ] {
    override def default( from:HiddenState ) =
      new HashMap[ HiddenState, Double ] {
        override def default( to:HiddenState ) = 0D
      }
  }

  var emissions = new HashMap[ HiddenState, HashMap[Observation, Double] ] {
    override def default( from:HiddenState ) =
      new HashMap[ Observation, Double ] {
        override def default( to:Observation ) = 0D
      }
  }


  def setInitialStateProbs( newInitialProbs:HashMap[HiddenState,Double] ) {
    initialStateProbs = newInitialProbs
  }

  def setTransitions( newTransitions:HashMap[HiddenState,HashMap[HiddenState,Double]] ) {
    transitions = newTransitions
  }

  def setEmissions( newObservations:HashMap[HiddenState,HashMap[Observation,Double]] ) {
    emissions = newObservations
  }

  def setParameters( newParams:PlainHMMParameters ) {
    val PlainHMMParameters(
      initialProbs,
      transitionProbs,
      emissionProbs
    ) = newParams

    setInitialStateProbs( initialProbs )
    setTransitions( transitionProbs )
    setEmissions( emissionProbs )
  }

  def numHiddenStates = transitions.keySet.size


  def uniformHMM( hiddenStates:Set[HiddenState], observations:Set[Observation] ) {
    val uniformTransitionProbablity = 1D/(hiddenStates.size)

    setInitialStateProbs(
      HashMap(
        hiddenStates.map{ q =>
          q -> uniformTransitionProbablity
        }.toSeq:_*
      )
    )

    setTransitions(
      HashMap(
        hiddenStates.map{ from =>
          from -> HashMap(
            hiddenStates.map{ to =>
              to -> uniformTransitionProbablity
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )

    val uniformEmissionProbablity = 1D/(observations.size)
    setEmissions(
      HashMap(
        hiddenStates.map{ from =>
          from -> HashMap(
            observations.map{ to =>
              to -> uniformEmissionProbablity
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }


  def normalizeDist[T<:Letter]( dist:HashMap[T,Double] ):HashMap[T,Double] = {
    val totalValue = dist.values.sum
    HashMap(
      dist.keySet.map{ left =>
        left -> { dist(left)/totalValue }
      }.toSeq:_*
    )
  }

  def randomHMM(
    hiddenStates:Set[HiddenState],
    observations:Set[Observation],
    randomSeed:Int,
    centeredOn:Int
  ) {
    import util.Random

    val r = new Random( randomSeed )
    setTransitions(
      HashMap(
        hiddenStates.map{ from =>
          from ->
            normalizeDist(
              HashMap(
                hiddenStates.map{ to =>
                  to -> ( r.nextDouble + centeredOn )
                }.toSeq:_*
              )
            )
        }.toSeq:_*
      )
    )

    setInitialStateProbs(
      normalizeDist(
        HashMap(
          hiddenStates.map{ q =>
            q -> ( r.nextDouble + centeredOn )
          }.toSeq:_*
        )
      )
    )

    setEmissions(
      HashMap(
        hiddenStates.map{ hiddenState =>
          hiddenState ->
            normalizeDist(
              HashMap(
                observations.map{ observation =>
                  observation -> ( r.nextDouble + centeredOn )
                }.toSeq:_*
              )
            )
        }.toSeq:_*
      )
    )
  }


  def viterbi( s: List[Observation] ) = {
    def argmax( h:HashMap[HiddenState,Double] ):HiddenState =
      h.keySet.reduceLeft{ (p, q) => if( h(p) > h(q) ) p else q }

    def viterbi_aux(
      backtrace:List[HashMap[HiddenState,HiddenState]], // psi
      previousState:HashMap[HiddenState,Double],        // delta
      remaining:List[Observation]
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
            transitions.keySet.map{ to =>
              to -> argmax(
                HashMap(
                  previousState.keySet.map{ from =>
                    from -> previousState(from) * transitions(from)(to)
                  }.toSeq:_*
                )
              )
            }.toSeq:_*
          ),
          HashMap(
            transitions.keySet.map{ to =>
              to -> (
                previousState.keySet.map{ from =>
                  previousState(from) * transitions(from)(to)
                }.max
              ) * emissions(to)(remaining.head)
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
          initialStateProbs.keySet.map{ q =>
            q -> initialStateProbs(q) * emissions(q)(s.head)
          }.toSeq:_*
        ),
        s.tail
      )
    )
  }

  /**
  * For the plain HMM, this is just Baum-Welch. NOTE: I'm doing state-emissions, not arc-emissions
  * as we see in Manning and Schuetze. I've picked a more generic function name in anticipation of
  * using the frontier algorithm here for coupled HMMs....
  */
  def computeExpectations( s: List[Observation] ) = {
    def forwardPass( allObservations:List[Observation] ):List[HashMap[HiddenState,Double]] = {
      def forwardPass_aux(
        computed:List[HashMap[HiddenState,Double]],
        remaining: List[Observation]
      ):List[HashMap[HiddenState,Double]] =
        if( remaining == Nil )
          computed
        else
          forwardPass_aux(
            computed ++
            List(
              HashMap(
                transitions.keySet.map{ to =>
                  to -> (
                    computed.last.keySet.map{ from =>
                      computed.last( from ) *
                      transitions( from )( to )
                    }.sum *
                    emissions( to )( remaining.head )
                  )
                }.toSeq:_*
              )
            ),
            remaining.tail
          )

      forwardPass_aux(
        HashMap(
          initialStateProbs.keySet.map{ q =>
            q -> initialStateProbs(q) * emissions(q)(allObservations.head)
          }.toSeq:_*
        )::Nil,
        allObservations.tail
      )
    }

    def backwardPass( allObservations:List[Observation] ):List[HashMap[HiddenState,Double]] = {
      def backwardPass_aux(
        computed:List[HashMap[HiddenState,Double]],
        remaining: List[Observation]
      ):List[HashMap[HiddenState,Double]] =
        if( remaining == Nil )
          computed.reverse
        else
          backwardPass_aux(
            computed ++
            List(
              HashMap(
                transitions.keySet.map{ from =>
                  from -> (
                    computed.last.keySet.map{ to =>
                      computed.last( to ) *
                      transitions( from )( to )
                    }.sum *
                    emissions( from )( remaining.head )
                  )
                }.toSeq:_*
              )
            ),
            remaining.tail
          )

      backwardPass_aux(
        HashMap( transitions.keySet.map{ q => q -> 1D }.toSeq:_* )::Nil,
        allObservations.reverse.tail
      )
    }

    val alphas = forwardPass( s )
    val betas = backwardPass( s )

    assert( alphas.flatMap{_.keySet} == betas.flatMap{_.keySet} )

    val alphaBetaStateProducts =
      ( alphas zip betas ).map{ case( alphaStates, betaStates ) =>
        HashMap(
          alphaStates.keySet.map{ q => q -> alphaStates(q) * betaStates(q) }.toSeq:_*
        )
      }

    val stringProb = alphaBetaStateProducts.map{ t =>
      t.values.sum
    }.sum


    val gamma =
      alphaBetaStateProducts.map{ t =>
        val stateTotal = t.values.sum
        HashMap(
          t.keySet.map{ q =>
            q -> t(q)/stateTotal
          }.toSeq:_*
        )
      }


    // This is using the clever definition of xi in terms of beta and gamma I found in the class
    // notes from that berkeley guy
    val xi = (gamma.init zip betas.init zip betas.tail zip s.tail).map{ case ( ((g, b0), b1), o) =>
      HashMap(
        transitions.keySet.map{ from =>
          from -> HashMap(
            transitions.keySet.map{ to =>
              to -> (
                g(from) * transitions(from)(to) * emissions(to)(o) * b1(to) /
                b0(to)
              )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    }


    val scaledExpectedTransitionCount =
      HashMap(
        transitions.keySet.map{ from =>
          from -> HashMap(
            transitions.keySet.map{ to =>
              to -> xi.map{ t =>
                t(from)(to)
              }.sum /
              stringProb
            }.toSeq:_*
          )
        }.toSeq:_*
      )

    val scaledExpectedStateCount =
      HashMap(
        transitions.keySet.map{ from =>
          from ->
            gamma.map{ t =>
              t(from)
            }.sum /
            stringProb
        }.toSeq:_*
      )

    val scaledExpectedEmissionsCount =
      HashMap(
        emissions.keySet.map{ q =>
          q -> HashMap(
            ( gamma zipWithIndex ).groupBy{ case( _, n ) => s(n) }.map{ case( o, indexedGs ) =>
              o -> ( (indexedGs.map{ case( g, _ ) => g(q) }.sum) / stringProb )
            }.toSeq:_*
          )
        }.toSeq:_*
      )

    PartialCounts(
      stringProb,
      scaledExpectedStateCount,
      scaledExpectedTransitionCount,
      scaledExpectedEmissionsCount
    )
  }


  override def toString = 
    "Initial State Probabilities:\n\t" +
    initialStateProbs.keySet.map{ q =>
      q + ": " + initialStateProbs(q)
    }.mkString( "", "\n\t", "\n" ) +
    "\nTransitions:\n\t" +
    transitions.keySet.map{ from =>
      transitions(from).keySet.map{ to =>
        from + " -> " + to + ": " + transitions(from)(to)
      }.mkString("","\n\t","\n")
    }.mkString("","\n\t","") +
    "\nEmissions:\n\t" +
    emissions.keySet.map{ hiddenstate =>
      emissions(hiddenstate).keySet.map{ observation =>
        hiddenstate + " -> " + observation + ": " + emissions(hiddenstate)(observation)
      }.mkString("","\n\t","\n")
    }.mkString("","\n\t","")

}

