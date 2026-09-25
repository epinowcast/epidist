# Validate a response family

Replaces `brms:::validate_family()`. Guarantees that a family given as a
family function, a `brms` family object, a `stats` family object or a
character string is returned as a `brmsfamily` object. Unlike the `brms`
internal it does not handle the `threshold` argument of ordinal
families, which `epidist` never sets, and it also accepts the name of a
family `epidist` defines itself, such as `"gengamma"`.

## Usage

``` r
.validate_family(family, link = NULL)
```

## Arguments

- family:

  A family function, a `brmsfamily` object, a `stats` family object, or
  a character string naming a `brms` or `epidist` family.

- link:

  Optional character string giving the link function. Only used when
  `family` is a character string without a second element.

## Value

A `brmsfamily` object.
