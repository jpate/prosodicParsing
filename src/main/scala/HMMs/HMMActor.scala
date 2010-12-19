package prosodicParsing.HMMs

import prosodicParsing.types._
import scala.actors._
import scala.actors.Actor

abstract class AbstractHMMActor[Q<:AbstractHiddenState,O<:AbstractObservation,P<:AbstractHMMParameters]
  extends AbstractHMM[Q,O,P] with Actor{
  def act() {
    loop {
      react{
        case parameters:P => { setParameters(parameters) }
        case Estimate( obs:List[O] ) => reply( computeExpectations(obs) )
        case Viterbi( obs:List[O] ) => reply( viterbi( obs ) )
        case Stop => exit()
      }
    }
  }
}

case object Stop

