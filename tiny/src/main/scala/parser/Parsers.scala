package ch.usi.inf.l3.sana.tiny.parser

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.CompilationUnits
import tiny.source.SourceFile
import tiny.source.Position
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor

trait Parsers {
  self: Trees with CompilationUnits =>



  def parse(source: SourceFile): CompilationUnit
}


