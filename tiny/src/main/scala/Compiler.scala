package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.ast.Trees
import tiny.ast.TreeGen
import tiny.util.CompilationUnits
import tiny.types.Types
import tiny.passes.Phases


trait CompilerApi {
  self: Parsers with CompilationUnits with Phases =>


  def standardPhases: List[Phase]

  def sourceReader: SourceReader


  def start(files: List[String]): List[CompilationUnit] = {
    val sources: List[SourceFile] = sourceReader.readSources(files)
    sources.map(parse(_)) 
  }
}




