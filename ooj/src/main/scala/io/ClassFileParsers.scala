package ch.usi.inf.l3.sana.ooj.io



import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.names.Name
import tiny.contexts.NoId
import tiny.modifiers._
import tiny.io.ClassPathCatalog

import calcj.modifiers._

import primj.modifiers._
import primj.contexts.TreeContexts

import ooj.ast.Trees
import ooj.types.Types
import ooj.modifiers._
import ooj.modifiers.Ops._
import ooj.util.Definitions

import org.objectweb.asm._
import java.lang.{ClassLoader, Class => JClass}
import java.net.{URL, URI, URLClassLoader, URLEncoder}
import java.io.{IOException, ByteArrayOutputStream, 
                BufferedInputStream, InputStream, File => JFile}

import scala.collection.mutable

trait ClassFileParsers {
  self: Trees with Types with TreeContexts with Definitions =>


  def classPaths: List[JFile]
  lazy val urls: List[URL] = classPaths.map(_.toURI.toURL)
  def classLoader: SanaClassLoader = new SanaClassLoader(urls.toArray)


  def loadClass(name: String): ClassDef = {
    val classData = 
      classLoader.getResourceAsStream(name)
    val cr           = new ClassReader(classData)
    val reader       = new ClassFileParser(name)
    cr.accept(reader, 0)
    val innerClasses = reader.innerClasses.map(loadClass(_))
    val body         = Template(innerClasses ++ reader.clazz.body.members, NoId)
    ClassDef(reader.clazz.mods, NoId, reader.clazz.name, 
              reader.clazz.parents, body, None, NoId)
  }


  class SanaClassLoader(classpath: Array[URL]) 
    extends URLClassLoader(classpath, null) {

    private[this] val classes: mutable.Map[String, JClass[_]] = 
      mutable.Map.empty

    override protected def findClass(name: String): JClass[_] = 
      classes.get(name) match {
        case Some(cl)         => cl
        case None             =>
          val classData: Array[Byte] = try {
            loadClassData(name)
          } catch {
            case e: IOException =>
              throw new ClassNotFoundException(
                s"Class $name could not be found", e)
          }
          val c: JClass[_] = defineClass(name, classData, 0, classData.length)
          resolveClass(c)
          classes + (name -> c)
          c
      }


    override def getResourceAsStream(name: String): InputStream = {
      findResource(name.replace('.', '/') + ".class").openStream
    }

    private def loadClassData(name: String): Array[Byte] = {
      val in: BufferedInputStream = 
        new BufferedInputStream(getResourceAsStream(name))

      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      var i: Int = in.read

      while (i != -1) {
        out.write(i)
        i = in.read
      }

      in.close()
      val classData: Array[Byte] = out.toByteArray()
      out.close()
      classData
    }
      
    override def loadClass(className: String, resolveIt: Boolean): JClass[_] = {
      // Do not fallback to JVM's ClassLoader
      /* Check our local cache of classes */
      classes.get(className) match {
        case None            =>
          findClass(className)
        case Some(cl)        =>
          cl
      }
    }
  }

  protected class ClassFileParser private(version: Int,
    val className: String) extends ClassVisitor(version) with Opcodes {

    def this(className: String) = this(Opcodes.ASM4, className)
    private[this] val bytecodeClassName = className.replace('.', '/')

    var clazz: ClassDef = _
    var clazzFactory: Template => ClassDef = _
      
    var members: List[DefTree] = Nil
    var innerClasses: List[String] = Nil

    protected def chopOneParam(paramString: String): (String, String) = {
      if(paramString.startsWith("B") ||
         paramString.startsWith("C") ||
         paramString.startsWith("S") ||
         paramString.startsWith("I") ||
         paramString.startsWith("J") ||
         paramString.startsWith("F") ||
         paramString.startsWith("D") ||
         paramString.startsWith("V") ||
         paramString.startsWith("Z")) {

        val rest  = paramString.substring(1)
        val param = paramString(0).toString
        (param, rest)
      } else if(paramString.startsWith("[")) {
        val (fst, snd) = chopOneParam(paramString.substring(1))
        ("[" + fst, snd)
      } else {
        val endIndex = paramString.indexOf(';') + 1
        paramString.splitAt(endIndex)
      }
    }
    protected def chopParams(paramString: String, 
      acc: List[String]): List[String] = {
      if(paramString == "") acc
      else {
        val (fst, snd) = chopOneParam(paramString)
        chopParams(snd, fst::acc)
      }
    }

    protected def methodParams(paramString: String): List[ValDef] = {
      val params = chopParams(paramString, Nil).reverse

      params.zipWithIndex.map {
        case (paramTpe, index) =>
          ValDef(Flags(PARAM), NoId, stringToUseTree(paramTpe, false),
            Name("x" + index), Empty, None, NoId)
      }
    }

    protected def stringToUseTree(sig: String, 
                  classSig: Boolean): UseTree = {
      sig match {
        case "B"        => 
          TypeUse(NoId, Some(BYTE_TYPE_NAME.asString), None, NoId)
        case "C"        => 
          TypeUse(NoId, Some(CHAR_TYPE_NAME.asString), None, NoId)
        case "S"        => 
          TypeUse(NoId, Some(SHORT_TYPE_NAME.asString), None, NoId)
        case "I"        => 
          TypeUse(NoId, Some(INT_TYPE_NAME.asString), None, NoId)
        case "J"        => 
          TypeUse(NoId, Some(LONG_TYPE_NAME.asString), None, NoId)
        case "F"        => 
          TypeUse(NoId, Some(FLOAT_TYPE_NAME.asString), None, NoId)
        case "D"        => 
          TypeUse(NoId, Some(DOUBLE_TYPE_NAME.asString), None, NoId)
        case "Z"        => 
          TypeUse(NoId, Some(BOOLEAN_TYPE_NAME.asString), None, NoId)
        case "V"        => 
          TypeUse(NoId, Some(VOID_TYPE_NAME.asString), None, NoId)
        case "["        => 
          // Array type tree?
          // val elemTye = stringToUse.substring
          // TypeUse(NoId, Some(ARRAY_TYPE_NAME.asString), None, NoId)
          TypeUse(NoId, Some(VOID_TYPE_NAME.asString), None, NoId)
        case nme        =>
          val sig2 = if(classSig) nme
                     else nme.substring(1, nme.size -1)
          stringToUseTree(sig2.split("/").toList) match {
            case select@Select(qual, id: Ident)            =>
              val tuse = TypeUse(id.uses, id.nameAtParser, id.pos,
                                 id.owner)
              Select(qual, tuse, select.pos, select.owner)
            case use                                       =>
              use
          }
      }
    }

    private def stringToUseTree(sig: List[String]): UseTree = sig match {
      case List(x)                =>
        TypeUse(NoId, Some(x), None, NoId)
      case Nil                    =>
        throw new Exception("This should not happen")
      case xs                     =>
        val rest = stringToUseTree(xs.take(xs.size - 1))
        Select(rest, Ident(NoId, Some(xs.last), None, NoId), None, NoId)
    }


    protected def parseAccessFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_PUBLIC))         Flags(ACC_PUBLIC)
      else if(hasFlag(access, Opcodes.ACC_PROTECTED)) Flags(ACC_PROTECTED)
      else if(hasFlag(access, Opcodes.ACC_PRIVATE))   Flags(ACC_PRIVATE)
      else                                            Flags(ACC_PACKAGE)
    }

      
    protected def parseFinalFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_FINAL))      Flags(FINAL)
      else                                        noflags
    }


    protected def parseInterfaceFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_INTERFACE))  Flags(INTERFACE)
      else                                        noflags
    }

    protected def parseAbstractFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_ABSTRACT))   Flags(ABSTRACT)
      else                                        noflags
    }

    protected def parseStaticFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_STATIC))    Flags(STATIC)
      else                                       noflags
    }

    protected def hasFlag(allFlags: Int, 
        flag: Int): Boolean = (allFlags & flag) == flag


    override def visit(version: Int, access: Int, name: String,
     signature: String, superName: String, interfaces: Array[String]): Unit = {

      val acc           = parseAccessFlag(access)
      val isFinal       = parseFinalFlag(access)
      val isInterface   = parseInterfaceFlag(access)
      val isAbstract    = parseAbstractFlag(access)
      val mods          = acc | isFinal | isInterface | isAbstract | COMPILED



      val parents = {
        val intfs = interfaces.toList.map(stringToUseTree(_, true))
        if(superName == null)
          intfs
        else {
          val sup = stringToUseTree(superName, true)
          sup::intfs
        }
      }

      clazzFactory = (body: Template) => {
        val tpe = if(name == "java/lang/Object") ObjectType
                  else notype
        ClassDef(mods, NoId, Name(name), parents, body, 
                  None, NoId)
      }
    }

    override def visitSource(source: String, debug: String): Unit = {
    }

    override def visitOuterClass(owner: String, name: String, 
            desc: String): Unit = {

    }

    override def visitAnnotation(desc: String, 
            visible: Boolean): AnnotationVisitor =  {
      null
    }

    override def visitAttribute(attr: Attribute): Unit = {

    }

    override def visitInnerClass(name: String, outerName: String, 
            innerName: String, access: Int): Unit = {
      val acc   = parseAccessFlag(access)
      val sName = name.replace('/', '.')
      if(! acc.isPrivateAcc && outerName == bytecodeClassName)
        innerClasses = sName::innerClasses
      else
        ()
    }


    override def visitField(access: Int, name: String, desc: String,
          signature: String, value: Object): FieldVisitor = {
      val acc = parseAccessFlag(access)
      val isFinal = parseFinalFlag(access)
      val isStatic = parseStaticFlag(access)
      val mods = acc | isFinal | isStatic | FIELD | COMPILED


      val tpt  = stringToUseTree(desc, false)
      val vdef = ValDef(mods, NoId, tpt, Name(name), Empty, None, NoId)
      members = vdef::members
      null
    }

    override def visitMethod(access: Int, name: String, desc: String,
          signature: String, exceptions: Array[String]): MethodVisitor = {

      val acc           = parseAccessFlag(access)
      val isFinal       = parseFinalFlag(access)
      val isStatic      = parseStaticFlag(access)
      val isAbstract    = parseAbstractFlag(access)
      val isConstructor = if(name == CONSTRUCTOR_NAME.asString) {
        Flags(CONSTRUCTOR)
      } else noflags
      val mods          = 
        acc | isFinal | isStatic | isAbstract | isConstructor | COMPILED

      val (paramString, retString) = {
        val endParamIndex = desc.indexOf(')')
        val (fst, snd)    = desc.splitAt(endParamIndex)
        (fst.substring(1), snd.substring(1))
      }

      val ret    = stringToUseTree(retString, false)
      val params = methodParams(paramString)

      val meth = MethodDef(mods, NoId, ret, Name(name), params, 
        Empty, None, NoId)
      members = meth::members
      null
    }

    override def visitEnd(): Unit = {

      val body = Template(members, NoId)
      clazz = clazzFactory(body)
    }
  }
}

