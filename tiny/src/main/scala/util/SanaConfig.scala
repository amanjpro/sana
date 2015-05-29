package ch.usi.inf.l3.sana.tiny.util

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.source.Position
import tiny.ast.Trees
import tiny.contexts.TreeContexts

import scalaz.{Name => _, _}
import Scalaz._



trait SanaConfig {
  def isTest: Boolean = true
}
