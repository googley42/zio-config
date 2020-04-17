package foo

import zio.config.ConfigDescriptor.{int, string}
import zio.config.{ConfigDescriptor, ReadErrors}
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}
import zio.{IO, config}

object ConfigSpec
    extends DefaultRunnableSpec(
      suite("config suite")(testM("optional Int example") {
//        val x: ProductBuilder[String, String, String, Option[Int]] = (string(
//          "DOMAIN"
//        ) |@| int("PORT").optional)

        case class ConfigInternal private (domain: String,
                                           maybePort: Option[Int])
        val envVars: Map[String, String] =
          Map("DOMAIN" -> "dom1" /*, "PORT" -> "XXXXXXXXXX"*/ )
        val configDescriptor: ConfigDescriptor[String, String, ConfigInternal] =
          (string("DOMAIN") |@| int("PORT").optional)(
            ConfigInternal.apply,
            ConfigInternal.unapply
          )

        val configM: IO[ReadErrors[Vector[String], String],
                        config.Config[ConfigInternal]] =
          zio.config.Config.fromMap(envVars, configDescriptor)

        for {
          c <- configM
          internal <- c.config.config
        } yield assert(internal, equalTo(ConfigInternal("dom1", None)))
        // succeeds but I would expect Optional[Int] field to fail when given data "NOT_AN_INT"
      })
    )
