package ch.usi.inf.l3.sana.tiny.source

import org.antlr.v4.runtime.tree.ParseTree

trait SourceFile {
  def name: String
  def content: ParseTree
}


object SourceFile {
  private class SourceFileImpl(val name: String,
    val content: ParseTree) extends SourceFile

  def apply(n: String, c: ParseTree): SourceFile = new SourceFileImpl(n, c)
}

