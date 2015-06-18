package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.calcj
import types.Types
import ast.{Trees, TreeUtils}
import contexts.{TreeContexts, TreeInfos, TreeContextApis}
import util.Definitions




trait Global extends calcj.Global with 
        Trees with
        TreeUtils with
        TreeContexts with
        Definitions with
        TreeInfos with
        TreeContextApis with
        Types {


}
