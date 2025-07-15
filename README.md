# Masna3 (مصنع) [![Made with Haskell](https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square)](https://haskell.org)

Masna3 (the 3 is for the letter ع) is a file store service. It acts as a synchronisation point between user-facing clients and object storage services.

To account for the diversity of file upload workflows, Masna3 does not perform the upload and downloads itself, but creates pre-signed URLs that allow clients to upload and download
files. These URLs refer to particular object, serve one specific purpose (upload or download), and have an expiration time.

## 🍅 Build

You need GHC 9.10.1 and `cabal-install` >= 3.14.

run `make build` to build the whole project, API and server.


## 🍉 What's the point?

The point of this project is to provide a high-quality Haskell codebase that uses state-of-the-art practices for software architecture and documentation.
The code must be as readable as possible for newcomers, and be usable as examples to display software engineering best practices.
