# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [8.0.0] - 2026-03-20

### Breaking Changes
- `request` and `requestTask` now use `Http.request` / `Http.task` instead of
  `Http.riskyRequest` / `Http.riskyTask`. Cross-origin cookie sending
  (`withCredentials`) is no longer enabled by default. Auth via headers
  (e.g. `Api-Key`) and same-origin cookies are unaffected.

### Added
- `Neq` operator for not-equal comparisons in predicates. Serializes to
  `not_eq` query parameter, following StandardAPI conventions.

### Fixed
- URL encoding for `NotIn`, `Overlaps`, and `Contains` predicates.
- Response body now passed through in `expectWhatever` error handling.
- `errorToString` now adds a separator (`: `) between HTTP status description
  and error message body.

## [7.1.0] - 2021-10-01

### Added
- Expose `StandardApi.Url.Builder` module.

### Changed
- Querying improvements and simplifications.

## [7.0.0] - 2021-10-01

### Removed
- Removed `StandardApi.Parser` module.

### Changed
- Simplified interface with more Schema support.

## [6.1.0] - 2021-02-18

### Added
- `StandardApi.Url.Builder` (internal).
- `emptyBody` and `jsonBody` helpers (aliases for `Http.emptyBody` / `Http.jsonBody`).
- `jsonResolver` for use with `requestTask`.

### Changed
- `requestTask` accepts a `Resolver` instead of a `Decoder`.
- `request` and `requestTask` now accept an `Expect` / `Resolver` directly.

## [1.0.0] - 2019-12-04

### Added
- Initial release with `request`, `requestTask`, `schemaRequest`, `schemaRequestTask`.
- `Query`, `Operation`, `Include`, `Order`, `Limit`, `Offset` types.
- `Schema`, `Model`, `Attribute`, `Route` types for schema introspection.
- `Config` type for API configuration.
