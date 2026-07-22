#!/usr/bin/env python3
"""Generate the text-layout geometry test corpus.
Each test is a fixed-width page using a common web-safe font stack with an
explicit line-height (so vertical rhythm is font-metric independent) and
elements marked with stable ids so geometry is keyed by id.  weft substitutes
its own font, so the harness compares GEOMETRY STRUCTURE (line count, flow y,
alignment fill, break behaviour) with tolerance, never exact glyph pixels."""
import os

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "tests")
os.makedirs(OUT, exist_ok=True)

HEAD = """<!doctype html><html><head><meta charset="utf-8"><style>
*{box-sizing:border-box;}
html,body{margin:0;padding:0;}
body{font-family:Arial,Helvetica,sans-serif;font-size:16px;line-height:20px;color:#000;}
.col{width:400px;padding:0;border:0;margin:0;}
.w200{width:200px;}
.w300{width:300px;}
p{margin:0;}
</style></head><body>
"""
FOOT = "\n</body></html>\n"

LOREM = ("The quick brown fox jumps over the lazy dog while the sun sets slowly "
         "behind the distant hills and a gentle breeze moves through the tall grass.")
LOREM2 = ("Alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu "
          "xi omicron pi rho sigma tau upsilon phi chi psi omega done.")

tests = {}

# ---- Line breaking ----
tests["01-wrap-basic"] = f'<div class="col" id="c"><p id="p">{LOREM}</p></div>'
tests["02-wrap-narrow"] = f'<div class="col w200" id="c"><p id="p">{LOREM}</p></div>'
tests["03-wrap-width300"] = f'<div class="col w300" id="c"><p id="p">{LOREM2}</p></div>'
tests["04-longword-overflow"] = ('<div class="col w200" id="c"><p id="p">'
    'Short then supercalifragilisticexpialidocioussupercalifragilistic done.</p></div>')
tests["05-break-all"] = ('<div class="col w200" id="c"><p id="p" style="word-break:break-all">'
    'Short then supercalifragilisticexpialidocioussupercalifragilistic done here.</p></div>')
tests["06-overflow-wrap"] = ('<div class="col w200" id="c"><p id="p" style="overflow-wrap:break-word">'
    'Short then supercalifragilisticexpialidocioussupercalifragilistic done here.</p></div>')
tests["07-break-word-legacy"] = ('<div class="col w200" id="c"><p id="p" style="word-wrap:break-word">'
    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa end.</p></div>')
tests["08-nbsp-nobreak"] = ('<div class="col w200" id="c"><p id="p">'
    'one two three four&nbsp;five&nbsp;six&nbsp;seven eight nine ten.</p></div>')
tests["09-many-shortwords"] = ('<div class="col w300" id="c"><p id="p">'
    + " ".join(["ab"]*60) + '</p></div>')
tests["10-single-word-fits"] = '<div class="col" id="c"><p id="p">Hello</p></div>'

# ---- white-space ----
tests["11-ws-normal"] = ('<div class="col w300" id="c"><p id="p" style="white-space:normal">'
    'a   b\n  c\t\td   collapse   these    spaces   and    wrap normally here please.</p></div>')
tests["12-ws-nowrap"] = ('<div class="col w200" id="c"><p id="p" style="white-space:nowrap">'
    'This line should never wrap even though it is far too long for the column.</p></div>')
tests["13-ws-pre"] = ('<div class="col" id="c"><pre id="p" style="white-space:pre;margin:0;font-family:Arial">'
    'line one\nline two is here\nline three\n\nline five after blank</pre></div>')
tests["14-ws-pre-wrap"] = ('<div class="col w200" id="c"><p id="p" style="white-space:pre-wrap">'
    'kept   spaces here\nand a newline plus wrapping of a very long portion of text follows.</p></div>')
tests["15-ws-pre-line"] = ('<div class="col w200" id="c"><p id="p" style="white-space:pre-line">'
    'collapsed   spaces\nbut newline kept and this long portion wraps as needed here now.</p></div>')
tests["16-pre-tab"] = ('<div class="col" id="c"><pre id="p" style="white-space:pre;margin:0">'
    'a\tb\tc\nxx\tyy\tzz\nname\tvalue</pre></div>')
tests["17-leading-trailing-ws"] = ('<div class="col" id="c"><p id="p">'
    '     leading and trailing spaces get collapsed at edges     </p></div>')
tests["18-pre-line-collapse-spaces"] = ('<div class="col" id="c"><p id="p" style="white-space:pre-line">'
    'x     y     z</p></div>')

# ---- alignment / spacing ----
tests["19-align-left"] = f'<div class="col" id="c"><p id="p" style="text-align:left">{LOREM}</p></div>'
tests["20-align-right"] = f'<div class="col" id="c"><p id="p" style="text-align:right">{LOREM}</p></div>'
tests["21-align-center"] = f'<div class="col" id="c"><p id="p" style="text-align:center">{LOREM}</p></div>'
tests["22-align-justify"] = f'<div class="col" id="c"><p id="p" style="text-align:justify">{LOREM}</p></div>'
tests["23-justify-narrow"] = f'<div class="col w200" id="c"><p id="p" style="text-align:justify">{LOREM2}</p></div>'
tests["24-text-indent"] = f'<div class="col" id="c"><p id="p" style="text-indent:40px">{LOREM}</p></div>'
tests["25-text-indent-neg"] = f'<div class="col" id="c"><p id="p" style="text-indent:-20px;padding-left:20px">{LOREM}</p></div>'
tests["26-line-height-2"] = f'<div class="col" id="c"><p id="p" style="line-height:32px">{LOREM}</p></div>'
tests["27-line-height-unitless"] = f'<div class="col" id="c"><p id="p" style="line-height:2">{LOREM}</p></div>'
tests["28-letter-spacing"] = f'<div class="col" id="c"><p id="p" style="letter-spacing:3px">{LOREM}</p></div>'
tests["29-word-spacing"] = f'<div class="col" id="c"><p id="p" style="word-spacing:12px">{LOREM}</p></div>'
tests["30-center-short"] = '<div class="col" id="c"><p id="p" style="text-align:center">Centered</p></div>'
tests["31-right-short"] = '<div class="col" id="c"><p id="p" style="text-align:right">Right</p></div>'

# ---- flow ----
tests["32-p-stack"] = ('<div class="col" id="c">'
    '<p id="p1" style="margin:0 0 16px 0">First paragraph here with some words to fill.</p>'
    '<p id="p2" style="margin:0 0 16px 0">Second paragraph with more words following the first.</p>'
    '<p id="p3" style="margin:0">Third paragraph to close it all out neatly.</p></div>')
tests["33-margin-collapse"] = ('<div class="col" id="c">'
    '<p id="p1" style="margin:0 0 20px 0">Top paragraph.</p>'
    '<p id="p2" style="margin:30px 0 0 0">Bottom paragraph, margins collapse to 30.</p></div>')
tests["34-list-wrap"] = ('<div class="col w300" id="c"><ul id="ul" style="margin:0;padding-left:40px">'
    '<li id="li1">A short first item.</li>'
    '<li id="li2">A much longer list item that will certainly wrap onto a second line here.</li>'
    '<li id="li3">Third.</li></ul></div>')
tests["35-float-left"] = ('<div class="col" id="c">'
    '<div id="f" style="float:left;width:100px;height:60px;background:#ccc"></div>'
    f'<p id="p" style="margin:0">{LOREM}</p></div>')
tests["36-float-right"] = ('<div class="col" id="c">'
    '<div id="f" style="float:right;width:100px;height:60px;background:#ccc"></div>'
    f'<p id="p" style="margin:0">{LOREM}</p></div>')
tests["37-pre-block-size"] = ('<div class="col" id="c"><pre id="p" style="margin:0;white-space:pre">'
    'aaaa\nbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\ncc</pre></div>')
tests["38-nested-inline"] = ('<div class="col" id="c"><p id="p">Some text with '
    '<b id="b">bold words</b> and <a id="a" href="#">a link</a> that should not break flow '
    'and continues wrapping past the edge naturally into more lines below.</p></div>')
tests["39-inline-span-color"] = ('<div class="col w300" id="c"><p id="p">Normal '
    '<span id="s" style="color:red">colored inline run kept on flow</span> then normal text '
    'continuing to wrap across several lines within this column here now.</p></div>')
tests["40-two-columns-fixed"] = ('<div id="c" style="width:400px">'
    f'<div id="colA" style="width:180px;float:left">{LOREM2}</div>'
    f'<div id="colB" style="width:180px;float:right">{LOREM}</div></div>')

# ---- direction ----
tests["41-rtl-basic"] = (f'<div class="col" id="c"><p id="p" dir="rtl" style="text-align:right">{LOREM}</p></div>')
tests["42-rtl-default-align"] = (f'<div class="col" id="c"><p id="p" dir="rtl">{LOREM}</p></div>')
tests["43-rtl-short"] = ('<div class="col" id="c"><p id="p" dir="rtl">short line</p></div>')

# ---- extra edge cases ----
tests["44-empty-p"] = '<div class="col" id="c"><p id="p1">Before</p><p id="p2"></p><p id="p3">After</p></div>'
tests["45-br-forced"] = ('<div class="col" id="c"><p id="p">line one<br>line two<br>line three here</p></div>')
tests["46-multiple-br"] = ('<div class="col" id="c"><p id="p">a<br><br>b</p></div>')
tests["47-big-fontsize"] = ('<div class="col" id="c"><p id="p" style="font-size:32px;line-height:40px">'
    'Bigger text wraps sooner across the column than the default size does here.</p></div>')
tests["48-small-fontsize"] = ('<div class="col" id="c"><p id="p" style="font-size:11px;line-height:14px">'
    + LOREM + ' ' + LOREM2 + '</p></div>')
tests["49-hyphens-auto"] = ('<div class="col w200" id="c"><p id="p" lang="en" style="hyphens:auto">'
    'A demonstration of automatic hyphenation applied to extraordinarily complicated terminology.</p></div>')
tests["50-white-space-break-spaces"] = ('<div class="col w200" id="c"><p id="p" style="white-space:pre-wrap">'
    'trailing spaces          then more text that wraps around the narrow column edge here.</p></div>')
tests["51-indent-each-line-no"] = (f'<div class="col" id="c"><p id="p" style="text-indent:60px">'
    'Only the first line is indented and the rest of these wrapped lines start at the left edge.</p></div>')
tests["52-nested-block-margins"] = ('<div class="col" id="c"><div id="outer" style="padding:10px;background:#eee">'
    '<p id="inner" style="margin:0">Text inside a padded block should sit ten pixels in.</p></div></div>')

for name, body in tests.items():
    with open(os.path.join(OUT, name + ".html"), "w") as f:
        f.write(HEAD + body + FOOT)

print(f"wrote {len(tests)} tests to {OUT}")
