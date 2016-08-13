# GAR DEV

* "ga:" can be omited for id, dimensions, metrics, and sort arguments for gaRequest
* Added ability to pass dimensions and metrics as vectors.
* Added validation of argument requirements for gaRequest 
* Added additional arguments (samplingLevel,includeEmptyRows) to gaRequest function.  


# GAR 1.2

* Added function (getMeta) that retrieves a dimensions and metrics available for use in the Core Reporting API.
* Added containsSampledData to gaRequest results data.frame.
* Fixed bug where gaRequest would fail when totalResults equaled zero.

# GAR 1.1

*  Need to retrieve more than 10K records? gaRequest has been modified to paginate through all results. Pagination will occur if allResults is set to TRUE and max is set to 10000.

* Error handling bug (#1) in gaRequest fixed.
  