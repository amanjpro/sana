package ch.usi.inf.l3.sana.tiny.debug

import java.util.logging.{Logger => JLogger, Level, 
  FileHandler, SimpleFormatter}


class Logger(val level: Level, destination: String) {
  private[this] val logger = JLogger.getLogger(JLogger.GLOBAL_LOGGER_NAME)
  private[this] val handler = new FileHandler(destination)
  private[this] val formatter = new SimpleFormatter

  handler.setLevel(level)
  handler.setFormatter(formatter)
  logger.setLevel(level)
  logger.addHandler(handler)


  def info(msg: String): Unit = {
    logger.log(Level.INFO, msg)
  }
  def debug(msg: String): Unit = {
    logger.log(Level.ALL, msg)
  }
  def severe(msg: String): Unit = {
    logger.severe(msg)
  }
}

