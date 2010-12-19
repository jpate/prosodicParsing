
package prosodicParsing.types

import collection.immutable.HashMap

abstract class Expectation

case class PartialCounts(
  stringProb: Double,
  scaledStateCount: HashMap[HiddenState,Double],
  scaledTransitionCount: HashMap[HiddenState,HashMap[HiddenState,Double]],
  scaledEmissionsCount: HashMap[HiddenState,HashMap[Observation,Double]]
) extends Expectation

