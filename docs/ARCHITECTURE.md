# Architecture of Masnaع

The project is split in two main packages: The API description and the Server.

## API

The API description (`api/`) contains the API route types.
It is used by the Server (`server/`) and can be depended on by third-party Haskell clients.

Since Servant API definitions are very rich, it is easily possible to generate clients for various ecosystems:

* https://flora.pm/packages/@hackage/servant-typescript
* https://flora.pm/packages/@hackage/servant-elm
* https://flora.pm/packages/@hackage/servant-js

## Server

It uses the [`Effectful`][Effectful] system to show at the type level what kind of
interactions with the outside world are performed (logging, database, etc).
You can see the default available effects in `Masna3.Server.Effects`

### Configuration

Configuration is done according to the [12-factor app principles][12factor],
by storing it in the environment. We are open to switching to a more structured
configuration file format should the configuration need it. In which case, KDL will
be privileged.

### Data Storage

Data models are defined under the `Model/` directory, with a predictible naming scheme:

* `Model/XXX/Types.hs` is for the type declarations and creation;
* `Model/XXX/Query.hs` is for read-only database operations;
* `Model/XXX/Update.hs` is for read-write database operations.

We import these modules qualified like so:

```haskell
import Masna3.Server.Model.XXX.Query qualified as Query
import Masna3.Server.Model.XXX.Update qualified as Update
```

The directory hierarchy thus looks like this:

```
Model
├── File
│   ├── Query.hs
│   ├── Types.hs
│   └── Update.hs
└── Owner
    ├── Query.hs
    ├── Types.hs
    └── Update.hs
```

### Testing

The test suite spawns a server, which is queried at runtime to get the most realistic
interactions over the network. For the tests to run, you need access to the database.
A separate set of environment variables is available in the `environment.test.sh` file.

[12factor]: https://12factor.net/
[Effectful]: https://haskell-effectful.github.io/
