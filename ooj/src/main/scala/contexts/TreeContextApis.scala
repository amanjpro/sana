package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.ooj
import sana.tiny
import tiny.contexts.{TreeId, NoId}
import tiny.names.Name
import ooj.Global
import ooj.modifiers.Ops._





trait TreeContextApis extends primj.contexts.TreeContextApis {
  self: TreeContexts with TreeInfos =>

  
  implicit class ImplicitContextApi(override val ctx: Context) extends 
    def enclosingPackage(id: TreeId): TreeId = ??? 
      super.ImplicitContextApi(ctx) with ContextApi {
  }


  trait ContextApi extends super.ContextApi {
    def enclosingPackage(id: TreeId): TreeId
  }
}
