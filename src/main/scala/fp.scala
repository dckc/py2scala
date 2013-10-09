

/**
 * Created by connolly on 10/8/13.
 */
object fp {
  def typed[T](x: T, t: String) = x
  // suppress python import of classOf?
  def classOf[T](c: Class) = c
}
