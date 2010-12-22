
package prosodicParsing.types

import collection.immutable.HashMap

abstract class Expectation

case class PartialCounts(
  stringProb: Double,
  scaledStateCount: HashMap[HiddenState,Double],
  scaledTransitionCount: HashMap[HiddenState,HashMap[HiddenState,Double]],
  scaledEmissionsCount: HashMap[HiddenState,HashMap[Observation,Double]]
) extends Expectation {
  def prob:Double = stringProb

  def +( pc2:PartialCounts ) = {

    println( "      " + stringProb )
    PartialCounts(
      stringProb + pc2.stringProb,
      HashMap(
        scaledStateCount.keySet.map{ q =>
          q -> ( scaledStateCount(q) + pc2.scaledStateCount(q) )
        }.toSeq:_*
      ),
      HashMap(
        scaledTransitionCount.keySet.map{ from =>
          from -> HashMap(
            scaledTransitionCount(from).keySet.map{ to =>
              to -> ( scaledTransitionCount(from)(to) + pc2.scaledTransitionCount(from)(to) )
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        scaledEmissionsCount.keySet.map{ q =>
          q -> HashMap(
            scaledEmissionsCount(q).keySet.map{ obs =>
              obs -> ( scaledEmissionsCount(q)(obs) + pc2.scaledEmissionsCount(q).getOrElse(obs,0D) )
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )

  }
}

case class NewParams(
  stringProb: Double,
  newStateProbs: HashMap[HiddenState,Double],
  newTransitions: HashMap[HiddenState,HashMap[HiddenState,Double]],
  newEmissions: HashMap[HiddenState,HashMap[Observation,Double]]
)
