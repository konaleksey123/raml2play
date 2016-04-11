package raml2play.model

import scala.util.parsing.input.Positional

/**
 * @author vadim
 * @since 05.04.16
 */
/**
 * A call to the handler.
 *
 * @param packageName The handlers package.
 * @param controller The controllers class name.
 * @param instantiate Whether the controller needs to be instantiated dynamically.
 * @param method The method to invoke on the controller.
 * @param parameters The parameters to pass to the method.
 */
case class HandlerCall(packageName: String, controller: String, instantiate: Boolean, method: String, 
                       parameters: Option[Seq[Parameter]]) extends Positional {
  val dynamic = if (instantiate) "@" else ""
  override def toString = dynamic + packageName + "." + controller + dynamic + "." + method + parameters.map { params =>
    "(" + params.mkString(", ") + ")"
  }.getOrElse("")
}
