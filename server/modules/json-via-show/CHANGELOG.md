# Changelog

## [1.0.0] -- 2021-04-23

### Added

- Class `GenericToJsonViaShow` to generically derive JSON instance for a data-type, using `Show` for any unary constructor. 
- Provide a `ViaJson` transformer to force the generic deriving to encode the field using its 'ToJSON' instance instead of 'Show'.

### Changed 

N/A

### Removed

N/A
