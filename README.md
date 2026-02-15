# term2html

generate html from an erlang term (ehtml for those familiar with yaws):

# Installation

Add `term2html` to the dependencies in your `rebar.config`:
```bash
{deps, [{term2html, "1.0.0"}]}.
```
then fetch and compile the dependencies of your project:
```bash
rebar3 compile --deps_only
```

# Example

The following term:
```erlang
Ehtml = {'div', [{class, toto}],
 [{p, [], <<"hëllo world"/utf8>>},
  {img, [{src, <<"https://eptwalabha.com/taco.png">>},
         {alt, <<"a taco">>}]},
  {br},
  <<"cool eh?">>]}.
term2html:expand(Ehtml).
```

will produce the following html:
```html
<div class="toto"><p>hëllo world</p><img src="https://eptwalabha.com/taco.png" alt="a taco"><br>cool eh?</div>
```

