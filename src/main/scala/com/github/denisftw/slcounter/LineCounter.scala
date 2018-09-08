package com.github.denisftw.slcounter

import java.io.File
import scala.io.Source
import scala.collection.mutable.Map
import java.text.DecimalFormat


case class SourceFile(extension: String, lines: Int)

object LineCounter
{
  private val formatter = new DecimalFormat("##.##")

  private def countLines(value: File): Int = {
    Source.fromFile(value).getLines().size
  }

  private def processFile(file: File): SourceFile = {
    val nameParts = file.getName().split("""\.""")
    val lines = countLines(file)
    val ext = if (nameParts.nonEmpty) nameParts(nameParts.length - 1) else "<no-ext>"
    SourceFile(ext, lines)
  }

  private def processDirectory(directory: File): Seq[SourceFile] = {
    directory.listFiles().foldLeft(Seq.empty[SourceFile]) { (acc, file) =>
      if (file.isDirectory()) {
        acc ++ processDirectory(file)
      } else {
        acc :+ processFile(file)
      }
    }
  }

  private def process(rootDir: File): Map[String, Int] = {
    if (!rootDir.isDirectory())
      throw new RuntimeException("Not a directory!")

    val countedFiles = processDirectory(rootDir)
    countedFiles.foldLeft(Map.empty[String, Int]) { (acc, source) =>
      val key = source.extension
      val lines: Int = acc.get(key).getOrElse(0)
      acc + (key -> (lines + source.lines))
    }
  }

  def main(args: Array[String]): Unit = {
    val rootDir = new File(args(0))
    val sourceCounter = process(rootDir)
    val total = sourceCounter.values.foldLeft(0)(_ + _)
    val list = sourceCounter.keys.map {
        x => x + ": " + sourceCounter(x) + " (" + formatter.format(sourceCounter(x) * 100.0 / total)  + "%)"
    }
    list.foreach( x => println(x) )
    println("total: " + total)
  }
}
