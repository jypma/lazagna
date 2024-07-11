# Creating a release

We use the [sbt-sonatype](https://github.com/xerial/sbt-sonatype) plugin to release to Maven Central, using its new _"Central Portal"_ interface and API.

- Bump the version number, commit and push and/or merge
- Start an `sbt` shell
- `publishSigned` to prepare the artifacts locally and PGP sign them
- `sonatypeCentralUpload` to upload the bundle to Sonatype
- Log on to Sonatype and click _"Publish"_ for the new version. This will set its status to _Publishing_.
- Wait for the status to change to _Published_ (can be 10 minutes or more).
