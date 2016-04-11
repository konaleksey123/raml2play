package raml2play.model

/**
 * @author vadim
 * @since 01.04.16
 */
case class Rule(verb: HttpVerb, path: PathPattern, call: HandlerCall, comments: List[Comment] = List())
