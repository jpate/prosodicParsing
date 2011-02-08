import sbt._

class ProsodicParsingProject(info: ProjectInfo) extends DefaultProject(info)
{
    override def compileOptions = Unchecked :: super.compileOptions.toList
}

