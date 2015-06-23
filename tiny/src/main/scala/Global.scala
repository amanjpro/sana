package ch.usi.inf.l3.sana.tiny

import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.{TreeContexts, TreeInfos}
import tiny.ast.{Trees, TreeUtils, TreeGen}
import tiny.util._
import tiny.types.Types
import tiny.report._
import tiny.debug.Logger




trait Global extends Trees with
        TreeUtils with
        Types with
        CompilationUnits with 
        TreeContexts with
        TreeInfos with
        Reporting with
        Definitions with
        MonadUtils {


}
