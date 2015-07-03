package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana

import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj

import tiny.ast.TreeGen
import tiny.util._
import tiny.passes.Phases
import tiny.report._
import tiny.source.SourceReader
import tiny.source.SourceFile
import calcj.ast.Constants
import typechecker.Typers
import names.{Namers, IDAssigners}
import ast.Trees
import antlr._
import types.Types
import parser.Parsers
import contexts.TreeContexts
import tiny.io.ClassPathCatalog

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

import scalaz.{Failure => Fail, Success => Pass, _}
import Scalaz._


trait Compiler extends tiny.CompilerApi {
  self: Phases with
        Parsers with
        Typers  with
        IDAssigners with
        Namers =>


  val global: Global

  def sourceReader: SourceReader = new SourceReader {
    type P = Java1Parser
    def newLexer(is: ANTLRInputStream): Lexer = new Java1Lexer(is)
    def newParser(tokens: CommonTokenStream): Java1Parser =
      new Java1Parser(tokens)
    def parserStart(parser: Java1Parser): ParseTree = parser.compilationUnit
  }

  def catalog: ClassPathCatalog
  def idassigner: IDAssigner = new IDAssigner {}
  def namer: Namer = new Namer {}
  def typer: Typer = new Typer {}

  val standardPhases: List[Phase] = List(
      idassigner, namer, typer
    )
}
