package raml2play.model

/**
 * @author vadim
 * @since 05.04.16
 */
case class Dependency[+T <: Rule](ident: String, clazz: String, rule: T)
