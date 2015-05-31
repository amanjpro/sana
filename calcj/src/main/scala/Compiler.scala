package ch.usi.inf.l3.sana.calcj


import ch.usi.inf.l3.sana
import sana.tiny
import tiny.ast.TreeGen
import tiny.util._
import tiny.passes.Phases
import tiny.report._
import tiny.settings.Configurations
import tiny.contexts.TreeContexts
import tiny.source.SourceReader
import tiny.source.SourceFile
import typechecker.Typers
import ast.Trees
import antlr._
import types.Types
import ast.Constants
import parser.Parsers

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

import scalaz.{Failure => Fail, Success => Pass, _}
import Scalaz._


trait Compiler extends tiny.CompilerApi
  with Parsers
  with Trees
  with Constants
  with Types
  with Phases
  with Typers
  with TreeContexts
  with TreeGen 
  with MonadUtils
  with Reporting
  with CompilationUnits {



  def sourceReader: SourceReader = new SourceReader {
    type P = CalcjParser
    def newLexer(is: ANTLRInputStream): Lexer = new CalcjLexer(is)
    def newParser(tokens: CommonTokenStream): CalcjParser = new CalcjParser(tokens)
    def parserStart(parser: CalcjParser): ParseTree = parser.program
  }



  val standardPhases: List[Phase] = List(new Typer {})

  
}

