package ch.usi.inf.l3.sana.tiny

import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.ast.TreeGen
import tiny.util._
import tiny.types.Types
import tiny.report._
import tiny.debug.Logger




trait Global extends Trees with
        Types with
        CompilationUnits with 
        TreeContexts with
        Reporting with
        MonadUtils {
  val logger: Logger
}
