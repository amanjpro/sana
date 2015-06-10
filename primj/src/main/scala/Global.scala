package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.calcj
import types.Types
import ast.Trees
import contexts.TreeContexts




trait Global extends calcj.Global with 
        Trees with
        TreeContexts with
        Types {
}
