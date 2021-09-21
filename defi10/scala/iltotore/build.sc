import mill._, scalalib._

object main extends ScalaModule {

  def scalaVersion = "3.0.0"

  def ivyDeps = Agg(
    ivy"org.creativescala::doodle-core:0.9.25",
    ivy"org.creativescala::doodle-image:0.9.25"
  )
}