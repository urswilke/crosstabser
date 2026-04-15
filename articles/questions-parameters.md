# Parameters in the Questions sheet

This vignette lists the options how to modify the crosstabs generated
with crosstabser. The items in the bullet points correspond to the
column headers of the table in the Questions sheet:

## Mandatory arguments

The arguments `Type` and `RowVar` need to be specified for all
crosstabs:

- `Type`: Defining the type of question; there are 4 possible cases:
  - `cat`: for single-choice questions; if multiple `RowVar`s are
    specified, this will generate multiple crosstabs (along the y-axis)
    with the variable labels of the `RowVar`s as a sub-title
  - `mw`: for item batteries questions, e.g. “Rank each item on a scale
    from 1 – 5.”; First a table is created showing the mean values of
    the `RowVar` variables in its lines, the “Summary of means” table.
    Then, a series of tables showing the frequency counts of each of the
    categorical variables is generated similar to the case `cat`, the
    “detail tables”.
  - `mdg`: multiple dichotomy group (called MDGROUP in SPSS);
    multiple-choice question; For each answer option there is one
    variable; the text of the choice is in the variable label of the
    accdording variable; If this answer was selected the variable
    usually has the value 1.
  - `mcg` : multiple category group (called MCGROUP in SPSS);
    multiple-choice question; This is an alternative way to `mdg` to
    code multiple choice questions. The answer possibilities are stored
    in the value labels here (every variable has the same value labels).
    This means, you need at least as many variables as the maximum
    number of choices that were selected by the respondents.
- `RowVar`: variable names to be tabulated (space-separated);
  alternatively, if the string starts with the prefix `"ts: "` (without
  the quotes), you can use
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  syntax afterwards

## Optional arguments

### Optional arguments for all 4 `Type`s

- `Title`: Question title that will be shown in the table; A blank in
  single quotes (`' '`) inserts a line break

- `Abbreviation`: An identifier string for internal use; if duplicated
  or not specified, `"_row_<ROW VALUE>"` will be appended (where
  `<ROW VALUE>` is the row number)

- `Unguelt`: values / variable names (for `"mdg"`) representing the
  unvalid cases

- `Filter`: logical expression for the cases that should be filtered;
  you can also use different expressions if there are multiple
  `RowVar`s; this translates a string containing (multiple occurences
  of):

  - `prefix_{[A|B|...]}_suffix` into multiple strings `prefix_A_suffix`,
    `prefix_B_suffix`, `prefix_..._suffix`
  - `prefix_{rowvar}_suffix` into multiple strings with each of the
    `RowVar`s replacing the `"{rowvar}"` part(s) of the string
  - Multiple `SelVar`s AND multiple `Filter`s are not allowed and this
    will throw an error.

- `SelVar` & `SelVal`: calculates the table(s) conditionally for every
  value in `SelVal` that the Variable `SelVar` takes, the variables in
  RowVar have to be mentioned as many times as the number of variables
  in `SelVar`. Format of `SelVal`: “1 2 3 4:Name_it_different
  2-97:All_others” to produce a table for all values 1 to 4 and one
  where 2-97 are combined and labelled “All others”.

- `Mult`: if `TRUE` in `SelVar`/`SelVal` interviews should be counted
  double, if `SelVar` is `SelVal` more than once (i.e. if in `SelVar`
  two “Other manufacturers” mentioned, they will be seen as two answers)

- `Weight`: variable with which the values are weighted (will overwrite
  the one that is set in the sheet Macro if existing)

### Optional arguments for `Type = cat`

- `CatRec` & `CatLab`:
  - `CatRec`: Possibility to add to the table of a categorical variable
    (`Type = cat OR mw`) the statistics of a recoded variable
    summarizing categories
  - `CatLab`: Definition of the labels of the recoded summary variable
    in `CatRec`
- `MWRec`: values can be recoded for the calculation of the mean value
  with the equal syntax as in `CatRec`.
- `UngueltMW`: values that are excluded for the mean value calculation
  (if empty, `Unguelt` ist taken)
- `MetrMac`: Possibility to add one or more of the following lines to
  the table showing statistics of the variable:
  - `Sx`: Mean with x decimals
  - `Mx`: Median with x decimals
  - `Tx`: Total with x decimals
  - `Ex`: Standard error with x decimals
  - `Ix`: Minimum with x decimals
  - `Ax`: Maximum with x decimals
  - `Pnnx`: `nn`th Percentile with x decimals

&nbsp;

- `Einzelauspraegung`: if set to 0 the rows in the table corresponding
  to the values of the variable are removed from the table
  (`Type = "cat"`)

### Optional arguments for `Type = mw`

- `MW`: if 0 the “Summary of means” table is not calculated for
  `Type = "mw"`
- `Freq`: If 0 the detail tables are not calculated for `Type = "mw"`
- `MeanOverviewLabel`: label for summary of means table (normally
  “Summary of means”, cf. lexikon `cTabMeanOV`)
- `ZsfgMW`: Function name to use in `Type = "mw"`; possible values are
  `se`, `median`, `mean`, `percentile`, `min`, `max` and `sum`.
- `RepOV`: tbd

### Optional arguments for `Type = mdg`

- `MdgVal`: The value that is counted as selected for `Type = "mdg"`,
  can be a range (either 0-97 or 0 THRU 97)
- `MdgMissLab`: The Label that is written for the count of cases where
  none of the variables is selected (`Type = mdg`); Default in English:
  “No entry in the selected variables” (see lexikon `cTabNoEntry`)
- `MdgMissValid`: if TRUE missing cases are set to valid
  (`Type = "mdg"`)

### Other arguments

- `RvEmp`: If set to `"EXCLUDE"` Rows of values in the resulting tables
  with 0 counts will be removed.
- `Categories`:
  - For `Type = mcg` & `cat`, there are two different possibilities to
    use this parameter:

    1.  Space-separated numeric values: Only rows with these values will
        be shown in the valid details of the table.
    2.  A string of the form
        `"subtotal='OVERCODE 1'1,2,subtotal='OVERCODE 2'3,4,subtotal='OTHERS'othernm"`:
        This will add rows with overcodes. Under the first overcode row
        `"OVERCODE 1"`, it will list the rows with the values 1 & 2.
        Under the overcode row `"OVERCODE 2"`, it will list the rows
        with the values 3 & 4. The optional overcode `"OTHERS"` will
        list all the remaining valid codes. For `mcg`, this counts
        maximally once per respondent multiple occurring codes of one
        overcode.

  - For `Type = mdg`:

    - `"q2_1 q2_2:OVERCODE 1,q2_3 q2_4:OVERCODE 2"`: This will add rows
      with overcodes. Under the first overcode row `"OVERCODE 1"`, it
      will list the rows of the variables `q2_1` & `q2_2`. Under the
      overcode row `"OVERCODE 2"`, it will list the rows of the
      variables `q2_3` & `q2_4`.
- `Sort`: Possibility to resort the columns showing the categories in
  the table alphanumerically or by share;\
  Examples:
  - Descending by frequency of items: `ORDER=D KEY=COUNT`
  - Ascending: `ORDER=A`
