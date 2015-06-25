package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana
import sana.brokenj

import types.Types
import ast.{Trees, Constants}
// import contexts.{TreeContexts, TreeInfos}

import util.Definitions
import io.ClassFileParsers
import contexts.{TreeInfos, TreeContexts}




trait Global extends brokenj.Global with 
        Trees with
        Constants with
        Types with
        ClassFileParsers with
        TreeInfos with
        TreeContexts with
        Definitions {
        // TreeContexts with
        // Definitions with
        // TreeInfos with
        // Types {
}
