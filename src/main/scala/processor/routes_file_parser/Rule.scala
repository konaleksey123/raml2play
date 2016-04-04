package processor.routes_file_parser

/**
 * @author vadim
 * @since 01.04.16
 */
case class Rule(isDinamic: Boolean, verb: String, uri: String, packageName: String, className: String,
                  methodName: String, arguments: Seq[Argument])
