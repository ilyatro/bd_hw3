import scala.io.Source
import breeze.linalg.{Axis, DenseMatrix, DenseVector, copy, csvread, inv, normalize, pinv}
import breeze.stats.{mean, stddev}
import breeze.storage.ConfigurableDefault.fromV

import java.io.File
import scala.io.Source.fromFile
import java.io.PrintWriter
import scala.language.reflectiveCalls


object Main {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(path: String, data: String): Unit =
    using(new PrintWriter(path))(_.write(data))


  def main(args: Array[String]): Unit = {
    //    val data = fromFile("housing.csv").getLines.toArray.flatMap(_.split("\\s+")) //.map(_.toDouble)
    val data = csvread(new File("housing.csv"), ';')
    val n_cols = data.cols

    val target = data(::, n_cols - 1).copy
    data(::, n_cols - 1) := 1.0

    val lr_coef = pinv(data) * target

    val data_predict = data * lr_coef
    val residuals = target - data_predict

    val out0 = "Коэффициенты и СКО предсказания на train:"
    println(out0)
    val out1 = lr_coef.toArray.mkString("(",",",")")
    println(out1)
    val out2 = stddev(residuals).toString
    println(out2)

    val data_test = csvread(new File("housing_test.csv"), ';')
    val target_test = data_test(::, n_cols - 1).copy
    data_test(::, n_cols - 1) := 1.0
    val data_test_predict = data_test * lr_coef
    val residuals_test = target_test - data_test_predict


    val out3 = "Mean предсказания на test:"
    println(out3)
    val out4 = mean(residuals_test).toString
    println(out4)
    val out5 = "СКО предсказания на test:"
    println(out5)
    val out6 = stddev(residuals_test).toString
    println(out6)

    writeToFile("test.txt", List(out0, out1, out2, out3, out4, out5, out6).mkString("\n"))
  }
}