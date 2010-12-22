
package prosodicParsing.runners

import prosodicParsing.HMMs.PlainHMM
import prosodicParsing.types._
import prosodicParsing.HMMs.BasicBaumWelchManager

import java.util.concurrent._
import scalaz.concurrent._
import scalaz.concurrent.Promise
import scalaz.concurrent.Promise._

object HMMDevelopment {
  def main( args: Array[String] ) {

    val hmm = new PlainHMM

    val hiddenStates =
      { (0 to 2).map{ n => new HiddenState( "Q"+n ) } }.toSet

    val observations =
      List("wait", "whoa", "what").map{ s => Observation( s ) }.toSet

    val exampleString = List( "wait", "wait", "what" ).map{ s => Observation(s) }
    // hmm.uniformHMM( hiddenStates, observations )

    // // println( hmm )

    // hmm.computeExpectations( exampleString )

    // println( "====" )

    hmm.randomHMM( hiddenStates, observations, 16, 1 )

    // hmm.computeExpectations( exampleString )

    val longExampleString = List( "wait", "whoa", "wait", "wait", "wait", "what" ).map{ s => Observation(s) }


    object Manager extends BasicBaumWelchManager {
      val hmm = new PlainHMM
      val emThreshold = 0.00001
      val obs = longExampleString::exampleString::Nil

      val obsTypes = observations
      val qTypes = hiddenStates
      val randomSeed = 16
      val centeredOn = 10
    }


    println( Manager.maximize )

    // println( hmm )

    // val max = 10000
    // 
    // var forTotal = 0D
    // ( 1 to max ) foreach( i =>
    //   forTotal += {
    //     hmm.computeExpectations( longExampleString )
    //   }.prob
    // )

    // println("done using for ("+ forTotal +")")



    /*
    implicit val pool = Executors.newFixedThreadPool(2)
    implicit val s = Strategy.Executor
    */

    /*
    val oneThousandResults =
      Array.fill( max ){ longExampleString }.map{ s => hmm.concurrentExpectations( s ) } 
      // promise( Array.fill(10000){ longExampleString }.map{s =>
      //   hmm.computeExpectations(longExampleString)
      // } )

    val total = oneThousandResults.map{ _.get.prob }.sum
    // println( oneThousandResults.slice(0,10).map{ _.get.prob } )
    println( "done using promise (" + total + ")" )
    */

    println( hmm.viterbi( longExampleString ) )

  }
}

