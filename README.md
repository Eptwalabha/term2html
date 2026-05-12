# term2html

Generate HTML from Ehtml terms (a simple format used by [Yaws](https://yaws.hyber.org/)).

[![Erlang CI](https://github.com/Eptwalabha/term2html/actions/workflows/erlang.yml/badge.svg)](https://github.com/Eptwalabha/term2html/actions/workflows/erlang.yml)
[![Hex.pm Version](https://img.shields.io/hexpm/v/term2html.svg)](https://hex.pm/packages/term2html)
[![License](https://img.shields.io/hexpm/l/term2html.svg)](https://github.com/Eptwalabha/term2html/blob/main/LICENSE.md)
[![OTP Version](https://img.shields.io/badge/OTP-26%2B-brightgreen)](https://www.erlang.org/downloads)


# Installation

Add `term2html` to your `rebar.config` dependencies:
```bash
{deps, [{term2html, "1.1.0"}]}.
```
then fetch and compile:
```bash
rebar3 compile --deps_only
```

# Example

A simple example:
```erlang
term2html:expand({'div', [], "hello world!"}).
% Result: <div>hello world!</div>
```

A more complexe example:
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

# ehtml Format

The ehtml term structure comes from Yaws' ehtml format:
```erlang
{TagName}                           % Self-closing tag with no attributes
{TagName, Attributes}               % Tag with attributes, no content
{TagName, Attributes, Content}      % Complete tag (most common)
[Ehtml]                             % A list of any of all above
```

Where:
- **TagName**: Atom representing the HTML tag (e.g., `div`, `p`, …)
- **Attributes**: List of `{Key, Value}` tuples or bare atoms (e.g., `disabled`)
- **Content**: String, binary, number, or nested ehtml terms
