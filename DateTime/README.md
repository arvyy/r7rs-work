# Date-time reference implementation remarks

## Missing functionality

Reference implementation provides helper libraries for loading timezones from tzfile and leapseconds from leap-seconds.list (however implementation could decide to use a different mechanism).
It uses local files for timezones / leapseconds and hardcodes known timezone list, these should be changed to load from system (depends on platform).
On linux, timezone files are usually in `/usr/share/zoneinfo/`, the leapseconds file in `/usr/share/zoneinfo/leap-seconds.list`.

Reference implementation also doesn't do periodic reload of data. To force the reload, do `(set! tzmap '())` and `(set! leap-seconds-vec #f)`

Implementation places that need attention are marked with `TODO`

## Error signalling

Spec doesn't specify exact shape of errors; the `date-error` procedure should be changed to utilize the most appropriate error kind

## Performance

Implementation caches used timezones, but otherwise isn't steered for performance.

The `timestamp+` procedure pessimistically goes through `moment` conversion if `dt` has a seconds component to correctly handle leap seconds.

Alot of internal procedures call public functions (eg., `make-timestamp`) instead of internal (eg., `make-timestamp*`) which adds validation overhead for data that was already validated.

## Tests

The test suite assumes presence of `Europe/Vilnius` and `America/New_York` timezones.

The test suite also tests provided helper library for tzfile and leap seconds readers, which might be unnecessary if implementation decides to use different approach for loading data.
