package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.calcj
import types.Types
import ast.Trees
import contexts.{TreeContexts, TreeInfos}
import util.Definitions




trait Global extends calcj.Global with 
        Trees with
        TreeContexts with
        Definitions with
        TreeInfos with
        Types {
}
