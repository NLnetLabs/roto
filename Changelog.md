# Changelog

## Unreleased new version

Breaking changes

New

* `set` method for AsPath type.
* Better error message for BytesRecord types.

  Errors on BytesMessage now return the actual type of BytesRecord we're talking about.

* `len`, `first` and `pop` methods for List types.
* Nlri type added, with `set`, `afi`, `safi` methods.

  Nlri is now a first-class citizen, that can be interrogated.

* `announcements` (returning [Nlri]) method on BgpMessage.

  announcements is a pass-through to the routecore method on a BgpUpdateMessage.

Bug fixes

* Parse all available type definitions.

  Several types that could be used inside blocks, weren't available in Type and Anonymous Record definitions.

Other changes


## 0.2.0

Released 2024-01-18.

First release.

Other changes

No changes compared to 0.1.0-rc0, but due to an error in the versioning of the
placeholder for this crate (0.1.0), we have to move to 0.2.0.

## 0.1.0-rc0

Released 2024-01-10.

First release candidate.
