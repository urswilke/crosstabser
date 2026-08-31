# Questions row class

This is not supposed to be used directly. When creating a "Tabula"
object, this will generate a list of `Qrow` objects in its `$qrows`
field.

## Public fields

- `p`:

  parameters extracted from `df_qrow`

- `m`:

  `Tabula` object

- `qtabs`:

  list of `Qtabs` objects

- `log`:

  log entries

- `ditw`:

  This is the "dust in the wind" list object field that stores data that
  didn't make it into their own field. For developers only! For
  reproducible code you should NEVER rely on this field as it might be
  subject to change without any warning.

## Methods

### Public methods

- [`Qrow$new()`](#method-Qrow-initialize)

- [`Qrow$clone()`](#method-Qrow-clone)

------------------------------------------------------------------------

### `Qrow$new()`

#### Usage

    Qrow$new(df_qrow, mapping, ...)

#### Arguments

- `df_qrow`:

  row of the Questions dataframe

- `mapping`:

  `Tabula` object

- `...`:

  Not used at the moment.

------------------------------------------------------------------------

### `Qrow$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Qrow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# see `?Tabula`
```
