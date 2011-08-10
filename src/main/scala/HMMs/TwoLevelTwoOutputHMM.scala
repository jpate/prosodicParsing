/*
package ProsodicParsing.HMMs

import ProsodicParsing.types._
// import ProsodicParsing.types.distributions._
// import ProsodicParsing.types.parameters._
// import ProsodicParsing.types.partialCounts._
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.util.Maths
import collection.mutable.HashMap

class TwoLevelTwoOutputHMM(
  hiddenStateTypesSet:Set[HiddenStatePair],
  observationTypesSet:Set[ObservedStatePair],
  idString:String
) extends AbstractHMM[HiddenStatePair,ObservedStatePair] (hiddenStateTypesSet,observationTypesSet) {
  val hmmID = idString

  val Tuple2( obsATypes, obsBTypes ) =
    observationTypesSet.map{ case ObservedStatePair( obs1, obs2 ) =>
      Tuple2( ObservedState( obs1 ), ObservedState( obs2 ) )
    }.foldLeft(Tuple2[Set[ObservedState],Set[ObservedState]](Set(),Set())){ (a,b) =>
      val Tuple2( obs1s, obs2s ) = a
      val Tuple2( obs1, obs2 ) = b
      Tuple2( obs1s + obs1, obs2s + obs2 )
    }

  val Tuple2( hiddATypes, hiddBTypes ) =
    hiddenStateTypesSet.map{ case HiddenStatePair( hidd1, hidd2 ) =>
      Tuple2( HiddenState( hidd1 ), HiddenState( hidd2 ) )
    }.foldLeft(Tuple2[Set[HiddenState],Set[HiddenState]](Set(),Set())){ (a,b) =>
      val Tuple2( hidd1s, hidd2s ) = a
      val Tuple2( hidd1, hidd2 ) = b
      Tuple2( hidd1s + hidd1, hidd2s + hidd2 )
    }

  val obsAlphA = new LabelAlphabet()
  val obsAlphB = new LabelAlphabet()
  obsATypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphA.lookupIndex( _,true ) )
  obsBTypes.toList.sortWith( (a,b) => a < b ).foreach( obsAlphB.lookupIndex( _,true ) )

  val hiddAlphA = new LabelAlphabet()
  val hiddAlphB = new LabelAlphabet()
  val hiddAIndexToLabel = new HashMap[Int,HiddenState]
  val hiddBIndexToLabel = new HashMap[Int,HiddenState]
  hiddATypes.toList.sortWith( (a,b) => a < b ).foreach( qA =>
    hiddAIndexToLabel( hiddAlphA.lookupIndex( qA , true ) ) = qA
  )
  hiddBTypes.toList.sortWith( (a,b) => a < b ).foreach( qB =>
    hiddBIndexToLabel( hiddAlphB.lookupIndex( qB, true ) ) = qB
  )

  def assignmentToViterbiString( maxAssn:Assignment ) =
    (0 to ( stringLength -1 ) ).map{ t =>
      val hiddA = hiddAIndexToLabel( maxAssn.get( hiddenVariables( t ) ) )
      val hiddB =
        hiddBIndexToLabel( maxAssn.get( hiddenVariables( stringLength + t ) ) )

      HiddenStatePair(
        hiddA toString,
        hiddB toString
      )
    }.toList


  val initialStateProbabilitiesA =
    new LogProbabilityDistribution[HiddenState]( hiddATypes.toSet )
  val initialStateProbabilitiesB =
    new ConditionalLogProbabilityDistribution( hiddATypes.toSet, hiddBTypes.toSet )

  val transitionMatrixA =
    new ConditionalLogProbabilityDistribution( hiddATypes.toSet, hiddATypes.toSet )
  val transitionMatrixB =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddBTypes )

  var emissionMatrixA =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, obsATypes.toSet )
  var emissionMatrixB =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, obsBTypes.toSet )

  def parameters = List(
    initialStateProbabilitiesA,
    initialStateProbabilitiesB,
    transitionMatrixA,
    transitionMatrixB,
    emissionMatrixA,
    emissionMatrixB
  )

  def packageParameters = TLTOParameters(
    initialStateProbabilitiesA.pt,
    initialStateProbabilitiesB.cpt,
    transitionMatrixA.cpt,
    transitionMatrixB.cpt,
    emissionMatrixA.cpt,
    emissionMatrixB.cpt
  )

  def setParams( newParams:Parameters ) {
    val TLTOParameters(
      initialProbsA,
      initialProbsB,
      transitionsA,
      transitionsB,
      emissionsA,
      emissionsB
    ) = newParams

    initialStateProbabilitiesA.setPT( initialProbsA )
    initialStateProbabilitiesB.setCPT( initialProbsB )
    transitionMatrixA.setCPT( transitionsA )
    transitionMatrixB.setCPT( transitionsB )
    emissionMatrixA.setCPT( emissionsA )
    emissionMatrixB.setCPT( emissionsB )
  }

  def initialPartialCounts = TLTOPartialCounts(
    0D,
    HashMap(
      hiddATypes.map( thisStateName =>
        thisStateName -> Double.NegativeInfinity
      ).toSeq: _*
    ),
    HashMap(
      hiddATypes.map( qFromA =>
        qFromA -> HashMap(
          hiddBTypes.map( thisStateName =>
            thisStateName -> Double.NegativeInfinity
          ).toSeq: _*
        )
      ).toSeq:_*
    ),
    HashMap(
      hiddATypes.map( fromStateName =>
        fromStateName -> (
          HashMap(
            hiddATypes.map( toStateName =>
              toStateName -> Double.NegativeInfinity
            ).toSeq: _*
          )
        )
      ).toSeq: _*
    ),
    HashMap(
      hiddenStateTypes.map( qsFrom =>
        qsFrom -> (
          HashMap(
            hiddBTypes.map( toStateName =>
              toStateName -> Double.NegativeInfinity
            ).toSeq: _*
          )
        )
      ).toSeq: _*
    ),
    HashMap(
      hiddenStateTypes.map( qsFrom =>
        qsFrom -> (
          HashMap(
            obsATypes.map( obs =>
              obs -> Double.NegativeInfinity
            ).toSeq: _*
          )
        )
      ).toSeq: _*
    ),
    HashMap(
      hiddenStateTypes.map( fromStateName =>
        fromStateName -> (
          HashMap(
            obsBTypes.map( obs =>
              obs -> Double.NegativeInfinity
            ).toSeq: _*
          )
        )
      ).toSeq: _*
    )
  )

  def buildHMM( tokens: List[ObservedStatePair] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet( tokens.size )

    stringLength = tokens.size

    val hiddenVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphA, t ) )
    val hiddenVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphB, t ) )

    hiddenVariables = hiddenVarA ++ hiddenVarB

    val obsVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphA, t ) )
    val obsVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphB, t ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVarA( i ).setLabel("hidden.A"+i)
      hiddenVarB( i ).setLabel("hidden.B"+i)
      obsVarA( i ).setLabel("observation.A"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }


    // initial states:
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVarA(0) ),
        initialStateProbabilitiesA.toLogArray
      )
    )
    hmm.addInitialStateProbabilities(
      new CPT(
        LogTableFactor.makeFromLogValues(
          Array( hiddenVarA(0), hiddenVarB(0) ),
          initialStateProbabilitiesB.toLogArray
        ),
        hiddenVarB(0)
      )
    )

    ( 1 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i-1), hiddenVarA(i) ),
            transitionMatrixA.toLogArray
          ),
          hiddenVarA(i)
        )
      )
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i), hiddenVarB(i-1), hiddenVarB(i) ),
            transitionMatrixB.toLogArray
          ),
          hiddenVarB(i)
        )
      )
    }

    // emissions
    ( 0 to tokens.size-1) foreach{ i =>
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i), hiddenVarB(i), obsVarA(i) ),
            emissionMatrixA.toLogArray
          ),
          obsVarA(i)
        )
      )
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i), hiddenVarB(i), obsVarB(i) ),
            emissionMatrixB.toLogArray
          ),
          obsVarB(i)
        )
      )
    }
  }

  def generateObservationSequence( tokens:List[ObservedStatePair] ) = {
    val theSequence = new Assignment(
      observations,
      tokens.map{ case ObservedStatePair( obsA, _ ) =>
        obsAlphA.lookupIndex( ObservedState( obsA ) )
      }.toArray ++ tokens.map{ case ObservedStatePair( _, obsB ) =>
        obsAlphB.lookupIndex( ObservedState( obsB ) )
      }
    )
    //println( "sequence is: " + theSequence )
    theSequence
  }

  def computePartialCounts( tokens:List[ObservedStatePair] ) = {
    buildSlicedHMM( tokens )

    inferencer.computeMarginals( hmm )

    val initialStateCountsA = HashMap(
      hiddATypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )

    val initialStateCountsB = HashMap(
      hiddATypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddBTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )


    val emissionCountsA = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          obsATypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionCountsB = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          obsBTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsA = HashMap(
      hiddATypes.map{ qAFrom =>
        qAFrom -> HashMap(
          hiddATypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsB = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddBTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    //( 0 to tokens.size-2) foreach{ t =>
    ( 0 to tokens.size-2) foreach{ t =>
      val( fromVarA, fromVarB, toVarA, toVarB ) = (
        hiddenVariables(t), hiddenVariables(tokens.size+t),
        hiddenVariables(t+1), hiddenVariables(tokens.size+t+1)
      )

      val ObservedStatePair( obsA, obsB ) = tokens(t)


      val transitionA = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, toVarA ) )
      )

      hiddATypes.foreach{ qFromA =>
        hiddATypes.foreach{ qToA =>
          val transitionACount = transitionA.logValue(
            new Assignment(
              Array( fromVarA, toVarA ),
              Array(
                hiddAlphA.lookupIndex( qFromA ),
                hiddAlphA.lookupIndex( qToA )
              )
            )
          )

          transitionCountsA( qFromA )( qToA ) = Maths.sumLogProb(
            transitionCountsA( qFromA )( qToA ),
            transitionACount
          )

          if( t == 0 )
            initialStateCountsA( qFromA ) = Maths.sumLogProb(
              initialStateCountsA( qFromA ),
              transitionACount
            )
        }
      }

      val transitionB = inferencer.lookupMarginal(
        new HashVarSet( Array( toVarA, fromVarB, toVarB ) )
      )

      //hiddenStateTypes.foreach{ qsFrom =>
      hiddenStateTypes.map{ qsFrom =>
        val HiddenStatePair( parentAString, parentBString ) = qsFrom
        val qAParent = HiddenState( parentAString )
        val qBParent = HiddenState( parentBString )

        hiddBTypes.foreach{ qToB =>
          val transitionBCount = transitionB.logValue(
            new Assignment(
              Array(  toVarA, fromVarB, toVarB ),
              Array(
                hiddAlphA.lookupIndex( qAParent ),
                hiddAlphB.lookupIndex( qBParent ),
                hiddAlphB.lookupIndex( qToB )
              )
            )
          )

          transitionCountsB( qsFrom )( qToB ) = Maths.sumLogProb(
            transitionCountsB( qsFrom )( qToB ),
            transitionBCount
          )

          if( t == 0 ) {
            initialStateCountsB( qAParent )( qBParent ) = Maths.sumLogProb(
              initialStateCountsB( qAParent )( qBParent ),
              transitionBCount
            )
          }
        }
      }

      val emissions = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, fromVarB ) )
      )

      //hiddenStateTypes.foreach{ qsFrom =>
      hiddenStateTypes.map{ qsFrom =>
        val HiddenStatePair( parentAString, parentBString ) = qsFrom
        val qAParent = HiddenState( parentAString )
        val qBParent = HiddenState( parentBString )

        val emissionsCount = emissions.logValue(
          new Assignment(
            Array( fromVarA, fromVarB ),
            Array(
              hiddAlphB.lookupIndex( qBParent ),
              hiddAlphA.lookupIndex( qAParent )
            )
          )
        )

        emissionCountsA( qsFrom )( ObservedState( obsA ) ) = Maths.sumLogProb(
          emissionCountsA( qsFrom )( ObservedState( obsA ) ),
          emissionsCount
        )

        emissionCountsB( qsFrom )( ObservedState( obsB ) ) = Maths.sumLogProb(
          emissionCountsB( qsFrom )( ObservedState( obsB ) ),
          emissionsCount
        )
      }

      if( t == tokens.size - 2 ) {
        val ObservedStatePair( lastObsAString, lastObsBString ) = tokens(t+1)
        val lastHiddenStates = inferencer.lookupMarginal(
          new HashVarSet( Array( toVarA, toVarB ) )
        )
        hiddenStateTypes.map{ qsFrom =>
          val HiddenStatePair( parentAString, parentBString ) = qsFrom
          val qAParent = HiddenState( parentAString )
          val qBParent = HiddenState( parentBString )
          val lastHiddenStateCounts = lastHiddenStates.logValue(
            new Assignment(
              Array( toVarA, toVarB ),
              Array(
                hiddAlphB.lookupIndex( qBParent ),
                hiddAlphA.lookupIndex( qAParent )
              )
            )
          )

          emissionCountsA( qsFrom ) ( ObservedState( lastObsAString ) ) = Maths.sumLogProb(
            emissionCountsA( qsFrom ) ( ObservedState( lastObsAString ) ),
            lastHiddenStateCounts
          )

          emissionCountsB( qsFrom ) ( ObservedState( lastObsBString ) ) = Maths.sumLogProb(
            emissionCountsB( qsFrom ) ( ObservedState( lastObsBString ) ),
            lastHiddenStateCounts
          )

        }
      }
    }

    TLTOPartialCounts(
      generalProbability( tokens ),
      initialStateCountsA,
      initialStateCountsB,
      transitionCountsA,
      transitionCountsB,
      emissionCountsA,
      emissionCountsB
    )
  }


  def reestimate( corpus: List[List[ObservedStatePair]] ) = {

    val totalPartialCounts = corpus.map( computePartialCounts( _ ) ).reduceLeft(_+_)

    setParams( totalPartialCounts.toParameters )
    normalize
    totalPartialCounts.stringLogProb
  }

  override def toString =
    "  == HMM Parameters == \n" +
    "\nInitialProbabilitiesA" +
    initialStateProbabilitiesA +
    "\nInitialProbabilitiesB" +
    initialStateProbabilitiesB +
    "\ntransitionsA:" +
    transitionMatrixA +
    "\ntransitionsB:" +
    transitionMatrixB +
    "\nemissionsA" +
    emissionMatrixA +
    "\nemissionsB" +
    emissionMatrixB
}
*/

