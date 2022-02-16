package com.madgag.wordle

import com.madgag.wordle.WordFeedback.feedbackFor

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.util.Using

object LocalFileCache {
  def obtain[T](path: Path)(generate: => T): T = {
    println("Loading")
    val file = path.toFile
    if (file.exists()) {
      Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))) { s =>
        s.readObject().asInstanceOf[T]
      }
    } else {
      val tmpFile = File.createTempFile("temp", "lfc")
      val data: T = generate

      Using.resource(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(tmpFile)))) { s =>
        s.writeObject(data)
      }

      file.getParentFile.mkdirs()
      Files.move(tmpFile.toPath, path)
      println(s"Cached data stored to ${file.getAbsolutePath}")
      data
    }
  }
}
