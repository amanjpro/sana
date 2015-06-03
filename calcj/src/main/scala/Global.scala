package ch.usi.inf.l3.sana.calcj

import ch.usi.inf.l3.sana
import sana.tiny
import types.Types
import ast.{Trees, Constants}




trait Global extends tiny.Global with 
        Trees with
        Types with
        Constants {
}
