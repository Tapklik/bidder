# Changelog
All notable changes to this project will be documented in this file.


## [Unreleased]
- Changed cache app with local bidder_cache ets based module

## [0.3.1] - 2018-04-21
### Added
- Kintetic: AWS Kinesis client

### Changed
- Logic to match bids with wins--now this happens through Kinesis on AWS

### Fixed
- Fixed hour of week filter

### Removed
- Need for caching bids
- Kafka


## [0.2.2] - 2018-04-15
### Added
- Add Time server
- Added Function filter

### Changed

### Fixed
- Fixed hour of week filter

### Removed



## [0.2.1] - 2018-01-22
### Added
- Add Fees calculation to bidder

### Changed
- Reduce "Variance" price to be between 70% - 110% to account for profit margin and fees

### Fixed

### Removed

