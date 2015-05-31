package ch.usi.inf.l3.sana.tiny.util

class SanaConfig {
  /**
    * @group Testing and Debugging
    */
  var printTrees: Option[String] = None
  /**
    * @group Testing and Debugging
    */
  var isTest: Boolean = false
  /**
    * @group Testing and Debugging
    */
  var isVerbose: Boolean = false


  /**
    * @group Plugins
    */
  var plugins: List[String] = Nil

  /**
    * @group Compilation Options
    */
  var classpath: List[String] = Nil
  /**
    * @group Compilation Options
    */
  var files: Vector[String] = Vector()
  /**
    * @group Compilation Options
    */
  var destination: Option[String] = None
}
