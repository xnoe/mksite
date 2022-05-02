# mksite

`mksite` is a static site generator I have created that is written in OCaml. I made it as I wasn't happy with the other static site generators that were available. 

An example configuration for using `mksite` is as follows:

- source
  - _layouts
    - `default.html`
  - `index.html`

```html
<!-- default.html -->
<!DOCTYPE html>
<html>
  <head>
    <title>{!page.title!} - My Site<title>
  </head>
  <body>
    <h1>{!page.title!}</h1>

    {!content!}
  </body>
</html>
```

```html
<!-- index.html -->
{%
let layout = "default"
let title = "Home"
%}

Welcome to my site!
```

Pages using `mksite` to generate them can use `mksite` script in two different ways, either wrapping the `mksite` script in `{% %}`s or `{! !}`s. When using `{% %}`s, `mksite` script is executed as normal, when using `{! !}`s, it is the equivalent of wrapping the inner content in `{% print() %}`, meaning that the value is immediately inserted in to the page.

`mksite` is still under development and does not have many features, however, its current state allows it to be used for generating static sites.