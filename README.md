# TransversingCSS - Extract fragments of an HTML document using css selectors

The tagline says it all, you pass in a CSS query and an HTML string and 
you get back a list of fragments of the html that match that query.
Only a subset of the spec is supported, full CSS3 support is planned.

For now you can select:

 * By tag name: table td a
 * By class names: .container .content
 * By Id: #oneId
 * By attribute: [hasIt] [exact=match] [contains*=text] [starts^=with] [ends$=with]
 * Union: a, span, p
 * Immediate children: div > p
 * Get jiggy with it: div[data-attr=yeah] > .mon, .foo.bar div, #oneThing

This module was initially thought as part of my web application testing library,
but it may be useful for people doing web scraping too.

Example usage:

Given HTML:

```html
<html>
  <head>
    <title>a title</title>
  </head>
  <body>
    <h1>Hello</h1>
    <p>Links
      <a class="foo big">one</a>
      and
      <a>two</a>
    </p>
    <ul>
      <li>The First</li>
      <li>The <a class="big bar">Second</a></li>
    </ul>
  </body>
</html>
```

You can do:

```haskell
import Text.XML.HXT.TransversingCSS

main :: IO ()
main = do
  html <- readFile "the_html_above.html"

  case findBySelector "a.big, h1" html of
    Left parseError -> putStrLn "Your CSS query was malformed."
    Right results -> print results
 
```

And your result would be:

```haskell
["<a class=\"foo big\">one</a>", "<a class=\"big bar\">Second</a>", "<h1>Hello</h1>"]
```

