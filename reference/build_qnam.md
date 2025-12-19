# Build the observations for a single QNAM

Build the observations for a single QNAM

## Usage

``` r
build_qnam(
  dataset,
  qnam,
  qlabel,
  idvar,
  qeval,
  qorig,
  verbose = c("message", "warn", "silent")
)
```

## Arguments

- dataset:

  Input dataset

- qnam:

  QNAM value

- qlabel:

  QLABEL value

- idvar:

  IDVAR variable name (provided as a string)

- qeval:

  QEVAL value to be populated for this QNAM

- qorig:

  QORIG value to be populated for this QNAM

- verbose:

  Character string controlling message verbosity. One of:

  `"message"`

  :   Show both warnings and messages (default)

  `"warn"`

  :   Show warnings but suppress messages

  `"silent"`

  :   Suppress all warnings and messages

## Value

Observations structured in SUPP format
