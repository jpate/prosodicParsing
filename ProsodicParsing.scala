package ProsodicParsing

// import scala.collection.JavaConversions._
// import scala.io.Source
// import scala.util.Random
// import cc.mallet.grmm
// import cc.mallet.grmm.inference.Inferencer
// import cc.mallet.grmm.inference.JunctionTreeInferencer
// import cc.mallet.grmm.types._


import ProsodicParsing.directedModels.Types.HMM


object SimpleHMM {

  def main( args: Array[String] ) {

    val test = new HMM( Set( "the", "doggy" ), 10 )

    println( test.transitionMatrixToString )

  }
}



