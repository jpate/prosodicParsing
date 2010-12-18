
package prosodicParsing.types


abstract class Letter( s:String ) {
  def apply = s
  override def toString = s
}

class HiddenState(s:String) extends Letter(s)
class Observation(s:String) extends Letter(s)


