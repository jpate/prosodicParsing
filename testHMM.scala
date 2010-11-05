package ProsodicParsing.examples;

import collection.immutable.HashSet
import ProsodicParsing.Types.HMM


object testHMM {
  def main( args:Array[String] ) {
    val t = new HMM( 3 , HashSet("whoa","wait","what","?") )

    println( t.TransitionMatrix("Q1")("Q2") )
    t.buildHMM( Array("whoa","wait","what","?")  )
    t.randomize(1)
    t.buildHMM( Array("whoa","wait","what","?") )
  }
}

