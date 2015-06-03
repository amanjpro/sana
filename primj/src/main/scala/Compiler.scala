package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast.TreeGen
import tiny.util._
import tiny.passes.Phases
import tiny.report._
import tiny.settings.Configurations
import tiny.contexts.TreeContexts
import tiny.source.SourceReader
import tiny.source.SourceFile
import calcj.ast.Constants
import typechecker.Typers
import names.{Namers, AssignOwners}
import ast.Trees
import antlr._
import types.Types
import parser.Parsers

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

import scalaz.{Failure => Fail, Success => Pass, _}
import Scalaz._


trait Compiler extends tiny.CompilerApi {
  self: Phases with 
        Parsers with
        Typers with
        Namers with
        AssignOwners =>


  val global: Global

  def sourceReader: SourceReader = new SourceReader {
    type P = PrimjParser
    def newLexer(is: ANTLRInputStream): Lexer = new PrimjLexer(is)
    def newParser(tokens: CommonTokenStream): PrimjParser = new PrimjParser(tokens)
    def parserStart(parser: PrimjParser): ParseTree = parser.program
  }

  val standardPhases: List[Phase] = List(new Typer {})
}
