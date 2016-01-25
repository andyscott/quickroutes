/*
 * Copyright 2016 Andy Scott
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fail.sauce.quickroutes.spray

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import spray.routing._

final case class QuickRoutes() {
  def append(pf: PartialFunction[QuickRouteScrutinee.type, Unit]): QuickRoutes = macro QuickRouteMacros.append
  def asRoute: Route = {
    ???
  }
}

object QuickRoutes {
  implicit def asRoute(qrb: QuickRoutes): Route = qrb.asRoute
}

trait QuickRouteInterpolatorMethods {
  implicit class QuickRouteInterpolator(ctx: StringContext) {
    sealed trait api {
      def unapply(scrutinee: QuickRouteScrutinee.type): Any = macro QuickRouteMacros.apiUnapply
    }
    object delete extends api
    object get extends api
    object head extends api
    object options extends api
    object patch extends api
    object post extends api
    object put extends api
  }
}

object QuickRouteScrutinee

/** This is used internally as a placeholder during macro expansion.
  */
case class _QRPlaceholder(interpolator: String, parts: List[String]) {
  /** No implementation needed. */
  def unapply[T](x: QuickRouteScrutinee.type): T = ???
}
