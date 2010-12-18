
package prosodicParsing.runners

import prosodicParsing.HMMs.PlainHMM
import prosodicParsing.types._

object HMMDevelopment {
  def main( args: Array[String] ) {

    val hmm = new PlainHMM

    val hiddenStates =
      { (0 to 4).map{ n => new HiddenState( "Q"+n ) } }.toSet

    val observations =
      List("wait", "whoa", "what").map{ s => new Observation( s ) }.toSet

    hmm.uniformHMM( hiddenStates, observations )

    println( hmm )

    println( "====" )

    hmm.randomHMM( hiddenStates, observations, 16, 100 )

    println( hmm )

  }
}

