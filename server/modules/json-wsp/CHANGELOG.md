# Changelog

## [1.3.0] -- 2022-02-15

### Added

- `onMissingField` options to allow providing a possible default values for missing fields in records.

## [1.2.0] -- 2021-10-24

### Added

- Expose `FaultCode`.
- `Functor` instances for `Request` and `Response` types.

### Changed

- Handlers now also provide an extra function to create a `Fault`. It allows for answering with a fault while preserving the reflection value. Seemingly, `Fault` also now have a `mirror` field.

### Removed

N/A

## [1.1.0] -- 2021-05-06

### Added

- `mkResponse` for constructing a JSON value from a WSP response.
- `Eq` stock instances for `Request` and `Response`

### Changed 

- Function now expect the `ServiceName` type-instances to be defined on `Request` instead of `Response`.

### Removed

N/A

## [1.0.0] -- 2020-10-23

### Added

- Initial release. Simple wrapper around the partial JSON-WSP specification. Provides 
  few helpers for constructing and parsing JSON-WSP compliant, as well as a way to handle
  requests that include a `mirror` attribute. 

### Changed 

N/A

### Removed

N/A
