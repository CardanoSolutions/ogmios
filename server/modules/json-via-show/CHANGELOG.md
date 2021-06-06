# Changelog

## [2.0.0] -- 2021-06-05

### Added

- Unit tests for `ToJSONViaShow`

### Changed

- Renamed `GenericToJsonViaShow` to `ToJSONViaShow` with a completely revised implementation. The previous implementation tried to do something using generics but turned out to hit a wall
  when it came to nested types. The new approach does actually parse the `show` output and transforms it into JSON which is a bit more involved at runtime, but should give better results for deeply nested types.

### Removed

- `ViaJson` data-type wrapper which was used to force `ToJSON` instance on nested types, no longer needed.

## [1.0.0] -- 2021-04-23

### Added

- Class `GenericToJsonViaShow` to generically derive JSON instance for a data-type, using `Show` for any unary constructor. 
- Provide a `ViaJson` transformer to force the generic deriving to encode the field using its 'ToJSON' instance instead of 'Show'.

### Changed 

N/A

### Removed

N/A
