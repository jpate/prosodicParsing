package ProsodicParsing

//import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Random
import cc.mallet.grmm
import cc.mallet.grmm.inference.Inferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.types._




object SimpleFactorGraph {

  def main( args: Array[String] ) {

    //println( allVars.mkString("",",","") )

    val mdl = new FactorGraph

    val vars = Array(
      new Variable (2),
      new Variable (2),
      new Variable (3),
      new Variable (2),
      new Variable (2)
    )


    val arr = Array( 0.6, 1.3, 0.3, 2.3 )

    mdl.addFactor ( vars(0), vars(1), arr )

    System.out.println ("Model with one edge potential:")
    mdl.dump ()

    val arr2 = Array[Double]( 1, 2, 3, 4, 11, 12, 13, 14, 21, 22, 23, 24 )
    val varSet = new HashVarSet(Array( vars(2), vars(3), vars(4) ))

    val ptl = new TableFactor (varSet, arr2);
    mdl.addFactor (ptl);

    System.out.println ("Model with a 3-clique added:");
    mdl.dump ();

  }
}



