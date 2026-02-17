# Qtab class

This is not supposed to be used directly. When creating a "Tabula"
object, this will generate a list of `Qrow` objects in its `$qrows`
field, themselves each containing a list of `Qtab` objects in their
`$qtabs` fields.

## Details

`Qtab` objects have a custom print method `print.Qtab` (see examples in
[`?Tabula`](https://urswilke.codeberg.page/crosstabser/reference/Tabula.md)).

## Public fields

- `p`:

  parameters

- `d`:

  data

- `m`:

  `Tabula` object

## Methods

### Public methods

- [`Qtab$new()`](#method-Qtab-new)

- [`Qtab$clone()`](#method-Qtab-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Qtab$new(params, mapping, ...)

#### Arguments

- `params`:

  Parameters from `Qrow` object

- `mapping`:

  `Tabula` object

- `...`:

  Not used at the moment.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Qtab$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# see `?Tabula`
```
