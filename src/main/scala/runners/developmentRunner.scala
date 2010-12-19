
package prosodicParsing.runners

import prosodicParsing.HMMs.PlainHMM
import prosodicParsing.types._

object HMMDevelopment {
  def main( args: Array[String] ) {

    val hmm = new PlainHMM

    val hiddenStates =
      { (0 to 2).map{ n => new HiddenState( "Q"+n ) } }.toSet

    val observations =
      List("wait", "whoa", "what").map{ s => Observation( s ) }.toSet

    val exampleString = List( "wait", "wait", "what" ).map{ s => Observation(s) }
    hmm.uniformHMM( hiddenStates, observations )

    println( hmm )

    hmm.computeExpectations( exampleString )

    println( "====" )

    hmm.randomHMM( hiddenStates, observations, 16, 1 )

    hmm.computeExpectations( exampleString )

    val longExampleString = List( "wait", "whoa", "wait", "wait", "wait", "what" ).map{ s => Observation(s) }

    println( hmm )
    
    (0 to 1000) foreach( i =>
      hmm.computeExpectations( longExampleString )
    )

    println( hmm.viterbi( longExampleString ) )

  }
}

