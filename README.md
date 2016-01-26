# QuickRoutes #

QuickRoutes is a tool for building HTTP routes using a simple syntax.
The goal is to support various HTTP libraries. Initial support is played
for Spray routing.

It's still in its early phases-- so expect more information to come.

``` scala
// QuickRoutes for Spray routing layer
val myRoute: Route = QuickRoutes().append {
  case get"/users" ⇒
    complete("get all users")

  case get"/users/$id" ⇒
    complete(s"get user $id")

  case get"/users/$id/devices/${deviceId: DeviceId}" ⇒
    complete(s"get $id's device $deviceId")

  case post"/installations" ⇒
    complete("create a new installation")
}
```

## License ##

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
