
package prosodicParsing.HMMs

import collection.immutable.HashMap
import prosodicParsing.types._

class PlainHMM {


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


  def baumWelch( s: Array[Observation] ) {
    
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

