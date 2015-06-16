package ch.usi.inf.l3.sana.calcj

import ch.usi.inf.l3.sana
import sana.tiny
import types.Types
import ast.{Trees, Constants}
import contexts.{TreeContexts, TreeInfos}
import util.Definitions
import tiny.modifiers._




trait Global extends tiny.Global with 
        Trees with
        Types with
        TreeContexts with
        Definitions with
        TreeInfos with
        Constants {

}
