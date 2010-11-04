package ProsodicParsing.examples;

import ProsodicParsing.Types.HMM


object testHMM {
  def main( args:Array[String] ) {
    val t = new HMM( 5 )

    println( t.TransitionMatrix(0)(1) )
    t.buildHMM( Array("whoa","wait","what","?") )
  }
}

