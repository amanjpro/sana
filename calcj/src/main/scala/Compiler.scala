package ch.usi.inf.l3.sana.calcj


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast.TreeGen
import tiny.util.CompilationUnits
import tiny.util.MonadUtils
import tiny.passes.Phases
import tiny.report._
import tiny.contexts.TreeContexts
import tiny.source.SourceReader
import tiny.source.SourceFile
import calcj.typechecker.Typers
import calcj.ast.Trees
import calcj.antlr._
import calcj.types.Types
import calcj.ast.Constants
import calcj.parser.Parsers

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

import scalaz.{Failure => Fail, Success => Pass, _}
import Scalaz._


class Compiler extends tiny.CompilerApi
  with Parsers
  with Trees
  with Constants
  with Types
  with Phases
  with Typers
  with TreeContexts
  with TreeGen 
  with MonadUtils
  with CompilationUnits {


  def sourceReader: SourceReader = new SourceReader {
    type P = CalcjParser
    def newLexer(is: ANTLRInputStream): Lexer = new CalcjLexer(is)
    def newParser(tokens: CommonTokenStream): CalcjParser = new CalcjParser(tokens)
    def parserStart(parser: CalcjParser): ParseTree = parser.program
  }

  val phases: List[Phase] = List(new Typer {})

  
}


object Compiler {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil        => println("No file to parse")
      case fs         => 
        val compiler = new Compiler()
        val (errors, units) = compiler.start(fs) 
        errors.foreach(println)
        // TODO: Here it should go to codegen
        units.foreach(println)
    }
  }
}
