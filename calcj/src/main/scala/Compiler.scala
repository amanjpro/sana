package ch.usi.inf.l3.sana.calcj


import ch.usi.inf.l3.sana
import sana.tiny
import tiny.ast.TreeGen
import tiny.util._
import tiny.passes.Phases
import tiny.report._
import tiny.settings._
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


class Compiler(val isTest: Boolean, 
               val config: SanaConfig) extends tiny.CompilerApi
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


  val langName: String = "calcj"
  val langVersion: String = "1.0"

  def sourceReader: SourceReader = new SourceReader {
    type P = CalcjParser
    def newLexer(is: ANTLRInputStream): Lexer = new CalcjLexer(is)
    def newParser(tokens: CommonTokenStream): CalcjParser = new CalcjParser(tokens)
    def parserStart(parser: CalcjParser): ParseTree = parser.program
  }



  val standardPhases: List[Phase] = List(new Typer {})

  
}


object Compiler {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil        => println("No file to parse")
      case fs         => 
        val compiler = new Compiler(true, null)
        val (errors, units) = compiler.start(fs) 
        errors.foreach(println)
        // TODO: Here it should go to codegen
        units.foreach(println)
    }
  }
}
