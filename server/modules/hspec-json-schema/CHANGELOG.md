# Changelog

## edge / unreleased

### Added

ø

### Changed

- The `SchemaRef` references are now resolved outside of the property, to avoid unnecessary file-system accesses and speed up the test execution (~40%). Instead, the schema and resolved reference have to be fetched prior to calling `prop_validateToJSON` using `unsafeReadSchemaRef`.

### Removed

- The errors no longer spit back the full schema, which turned out to be needlessly verbose (and unhelpful) for large schemas. The 'missing required property' error has also been removed when the
  found and required keys are identical. This happen when using `oneOf` but failing to match the schema and it only creates more noise in the error output.

## [1.0.0] -- 2020-10-23

### Added

- Initial release. The API is very simple and offer a single function 'validateToJSON' 
  which is as capable as `hjsonschema`. The output is not perfect but tries its best to 
  format errors generated by hjsonschema properly.

### Changed 

N/A

### Removed

N/A