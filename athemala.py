# coding: latin1

import cgi, re, inspect, io
import collections

# map -> area
# object -> param
# form, fieldset -> legend
# form -> fieldset, caption, input, label, (select -> optgroup -> option)
# table -> caption, colgroup, thead, tfoot, tbody -> col, tr -> th, td
_empty_inline = ('img',)
_empty_oneline = ('area', 'base', 'col', 'input', 'link', 'meta', 'param') + \
    ('br', 'hr')
_false_block_elements = ('script', 'style', 'object', 'fieldset') + \
                    ('pre',) + ('p', 'div', 'address', 'textarea')
_block_elements = _false_block_elements + ('noscript',) + ('ul', 'ol', 'dl',
    'table', 'thead', 'tfoot', 'tbody', 'colgroup', 'select', 'optgroup') + \
    ('blockquote', 'form', 'html', 'head', 'body', 'map')
_oneline_elements = ('h1', 'h2', 'h3', 'h4', 'h5', 'h6') + ('caption',
    'button') + ('li', 'dt', 'dd', 'tr', 'option', 'legend') + ('title',)
_inline_elements = ('tt', 'i', 'b', 'big', 'small') + ('label',) + \
                    ('em', 'strong', 'dfn', 'code', 'samp', 'kbd', 'var',
                    'cite', 'abbr', 'acronym', 'q') + ('a', 'img') + \
                    ('sub', 'sup') + ('ins', 'del') + ('span',) + ('bdo',) + \
                    ('th', 'td')

qualities = {'empty': ' /', 'tagnl': '\n', 'preendtagnl': '\n',
    'endtagnl': '\n'}
asso = {_empty_inline: ('empty',), _empty_oneline: ('empty', 'tagnl'),
    _block_elements: ('tagnl', 'preendtagnl', 'endtagnl'),
    _oneline_elements: ('endtagnl',),
    _inline_elements: ()}

descendants = {'ul': ('li',), 'ol': ('li',), 'li': ('ul', 'ol'),
    'table': ('tr',), 'tr': ('td', 'th'), 'td': ('table',),
    'th': ('table',), 'form': ('table', 'fieldset'), 'fieldset': ('table',),
    'select': ('option',)}
overwrite = {'ul': ('ol',), 'td': ('th',)}
element_as_attribute = {'table': 'caption', 'fieldset': 'legend'}

# these elements can't be "automatically" parents of subelements, because
# finding an array in array_element() means we use default attributes
# in other words: descendants should be () for these elements
defaultattrs = {'a': ('href', True), 'option': ('value', True, 'selected')}

class dummy(object):
    pass

_elements = {}
for elems, which in asso.items():
    for e in elems:
        _elements[e] = dummy()
        _elements[e].descendants = descendants.get(e)
        _elements[e].overwrite = overwrite.get(e, ())
        _elements[e].defaultattrs = defaultattrs.get(e)
        _elements[e].ele_as_attr = element_as_attribute.get(e)
        for q, v in qualities.items():
            setattr(_elements[e], q, '')
            if q in which:
                setattr(_elements[e], q, v)

del asso, qualities, descendants, overwrite, defaultattrs, element_as_attribute
# example: if _elements[element_name].empty: ...

def _element_fixup(name, data, attrs):
    '''Returns the new name and descendant based on the current environment.

    The decision is based on the current name of the element, the data it
    is about to handle and the current attributes have an influence.
    Returns a 4-tuple with the new name, the new data it should get,
    the new descendant and attributes for the new descendant (which normally
    does not get attributes directly, only by "decorated" data).
    The passed attributes get modified in place to avoid further processing of
    special attributes.
    '''
    for e in _elements[name].overwrite:
        if attrs.pop(e, None):
            name = e
    if iscontainer(data) and _elements[name].defaultattrs:
        for na, val in zip(_elements[name].defaultattrs, data):
            if na == True:
                data = val
            else:
                attrs[na] = val
    desc = name
    desc_a = {}
    if _elements[name].descendants:
        desc = _elements[name].descendants[0]
        for d in _elements[name].descendants[1:]:
            avalue = attrs.pop(d, None)
            if avalue:
                desc = d
                # we acutally don't pass on the value of the descendant indicator
                # desc_a[desc] = avalue
    topass = _elements[desc].ele_as_attr
    value = attrs.pop(topass, None)
    if topass: desc_a[topass] = value
    return name, data, desc, desc_a

def selected(itr, hit):
    if not iscontainer(hit): hit = [hit]
    first = True
    for i in itr:
        select = 0
        if first:
            first = False
            if not hit or hit[0] == None: select = 1
        if i[0] in hit or str(i[0]) in hit:
            select = 1
        if select:
            i = list(i) + [dict(selected=1)]
        yield i

def key(inst):
    if inst[0] != 'submit':
        return inst[1]
    else:
        return None

def iscontainer(obj):
    if isinstance(obj, str):
        return False
    elif isinstance(obj, (tuple, list)):
        return True
    try:
        iter(obj)
    except TypeError:
        return False
    return True

def isdictlike(obj):
    return hasattr(obj, 'items')

class Html(str):
    '''Class representing already escaped (X)HTML.

    It is derived from unicode, so it behaves like unicode strings.
    If something from the html module gets refed to it, no extra
    escaping is done as it is recognized as Html instance.

    Adding: Html + Something --> Html, but only if unicode can add that...

    >>> a=Html('a')
    >>> type(a+a)
    <class '__main__.Html'>
    >>> type(a+'b')
    <type '__main__.Html'>
    >>> type(u'b'+a)
    <type 'unicode'>
    >>> type(a+Html('b'))
    <class '__main__.Html'>
    >>> a+Html('b')
    u'ab'
    '''

    def __add__(self, other):
        if not isinstance(other, self.__class__):
            other = esc(other)
        return self.__class__(str.__add__(self, other))

class d(str):
    ''' for doing u'some cheap ${a_did}DID provider$'.

    >>> u'$' % d()
    Traceback (most recent call last):
    ...
    IndexError: pop from empty list
    >>> u'${' % d()
    Traceback (most recent call last):
    ...
    ValueError: placeholder open

    >>> print u"""Text
    ... Dollar$$.
    ... ${some-tag}Content$
    ... text with stuff to escape < > &;
    ... ${;&}entity
    ... ${a-future}linkless a $$$
    ... Or an ${em}empty one${br{}, or not?$
    ... ${strong}Cascade ${em}with ${Html-ex} special part${}$.
    ... And some ${a-default} attributes ${}${hr_cl}.""" % \
        d({'some-tag': None, 'strong': dict(weak='no'), 'Html-ex': Html('<>'),
    ... 'a-default': 'http://backsla.sh', 'hr_cl': 'blue', 1: 2})
    Text
    Dollar$.
    <some>Content</some>
    text with stuff to escape &lt; &gt; &amp;;
    &;&;entity
    <a>linkless a $</a>
    Or an <em>empty one<br />, or not?</em>
    <strong weak="no">Cascade <em>with <> special part</em></strong>.
    And some <a href="http://backsla.sh"> attributes </a><hr class="blue" />.
    >>>
    '''
    pat = re.compile('''
        (?P<text>[^$]+) |
        (?P<literal>[$][$]) |
        (?P<named>[$][{]  (?P<id>[^}]*)  (?P<closed>[}])? ) |
        (?P<end>[$])''',
                    re.VERBOSE)
    id_sanitize = re.compile('[a-z]*')

    def __new__(cls, *pos, **kwd):
        return super(d, cls).__new__(cls)

    def __init__(self, output, flush_func, *pos, **kwd):
        self.out = output
        self.flush = flush_func
        self.dic = dict(*pos, **kwd)

    def __rmod__(self, format):
        '''
        -search in format for $...
        -
        '''
        matches = self.pat.finditer(format)
        tagstack = []
        for m in matches:
            groups = m.groupdict()
            id = groups['id']
            if groups['text']:
                self.out.write(esc(groups['text']))
            elif groups['literal']:
                self.out.write('$')
            elif groups['end']:
                self.out.endtag(tagstack.pop())
            elif groups['named']:
                if not groups['closed']:
                    raise ValueError('placeholder open')
                elif not id: # extensive end marker
                    self.out.endtag(tagstack.pop())
                else: # we have an id for the marker
                    id_sane = self.id_sanitize.match(id).group()
                    if id not in self.dic:
                        if id_sane in _elements:
                            self.out.tag(id_sane)
                            if not _elements[id_sane].empty:
                                tagstack.append(id_sane)
                        else: # id refers to an entity
                            self.out.write('&' + id + ';')
                    else: # id is in self.dic
                        obj = self.dic[id]
                        if isinstance(obj, Html):
                            self.out.write(obj)
                        elif isinstance(obj, str):
                            if id_sane == 'a':
                                stdattr = 'href'
                            else:
                                stdattr = 'class'
                            self.out.tag(id_sane, {stdattr: obj})
                            if not _elements[id_sane].empty:
                                tagstack.append(id_sane)
                        else:
                            self.out.tag(id_sane, obj)
                            if not _elements[id_sane].empty:
                                tagstack.append(id_sane)
            else:
                assert False, 'pat should not match groupless'
        if self.flush:
            return self.flush()

def join_dicts(*pos, **kw):
    '''Returns /one/ new joined dictionary.

    Joins positional arguments and keyword arguments into one mapping.
    Keyword arguments count as a dictionary. Arguments which evaluate
    to False get ignored.
    Later arguments and especially the keyword dict overwrite earlier.

    >>> join_dicts()
    {}
    >>> join_dicts({}, {})
    {}
    >>> join_dicts(None, 0, [], dict(a=1))
    {'a': 1}
    >>> join_dicts({'a': 1}, {'a': 2})
    {'a': 2}
    >>> join_dicts({'a': 1}, {'a': 2}, a=3)
    {'a': 3}
    >>> join_dicts({'a': 1}, {'b': 2}, a=3)
    {'a': 3, 'b': 2}
    >>> r = join_dicts({'a': 1}, {'b': 2}, a=3).items()
    >>> r.sort()
    >>> r
    [('a', 3), ('b', 2)]
    '''
    all = pos + (kw,)
    bigdic = {}
    for dict in all:
        if dict: bigdic.update(dict)
    return bigdic

def esc(obj, nbsp=False, quote=False):
    '''Returns an HTML/XML escaped unicode string.

    It does not return an Html object anymore. - This gets done in the last
    stage in the String return object.
    It returns what it gets if it is fed by
    an Html, because this should already be an escaped unicode object.

    keyword arguments:
    quote	-- if true the escaping will fit for inclusion as attribute
        value, default is false: quote only for use outside of tags
    nbsp	-- if true spaces get rewritten to &nbsp;s

    >>> esc('hallo')
    u'hallo'
    >>> x = esc('hallo')
    >>> type(x)
    <class '__main__.Html'>
    >>> x
    u'hallo'
    >>> x == esc(x)
    True
    >>> esc(u'>  <')
    u'&gt;  &lt;'
    >>> esc(u'>  <', nbsp=True)
    u'&gt;&nbsp;&nbsp;&lt;'
    >>> esc(u'valu"e')
    u'valu"e'
    >>> esc(u'valu"e', quote=True)
    u'valu&quot;e'
    '''
    if isinstance(obj, Html):
        return obj
    elif obj == None:
        return ''
    s = cgi.escape(str(obj), quote)
    if nbsp: s = re.sub(' ', '&nbsp;', s)
    return s

def attrs(adic):
    '''Returns a proper string of attributes for inclusion in start tags.

    The only arguments should be a dictionary. Keys terminating with an
    underscore overwrite the corresponding one without. E.g. "class_"
    overwrites "class". This makes it possible to pass reserved words
    outside a real dictionary as keyword arguments (but not here...).

    Keys with an intermediate underscore overwrite ones with dashes, e.g.
    "accept_charset" overwrites "accept-charset".

    Attribute with fixed values get written in XHTML style if and only if
    the corresponding value in the dictionary is not of type basestring:
    {'selected': 1} --> selected="selected"
    {'selected': 0} --> nothing
    ...and if it is of False value it gets ignored.

    [see examples for tag()]
    '''
    overwrite = {}
    copy = {}
    for key, val in list(adic.items()):
        nk = key
        if key[-1] == '_':
            nk = key[:-1]
        nk = nk.replace('_', '-')
        if nk in ('selected', 'checked', 'declare', 'defer', 'disabled',
            'ismap', 'multiple', 'nohref', 'readonly') and \
            not isinstance(val, str):
            if val:
                val = nk
            else:
                continue
        if key != nk:
            overwrite[nk] = val
        else:
            copy[nk] = val
    copy.update(overwrite)
    adic = copy
    if adic:
        a = [key + '="' + esc(adic[key], quote=1) + '"' for key in adic]
        a.sort()
        return ' ' + ' '.join(a)
    else:
        return ''

def decompose_element_attr(obj, prefer_container=0):
    '''Returns a 2-tuple: the element data and its attribute mapping.

    This function is a helper for table() and posibly other html
    wrapper functions that somehow accept lists or otherwise multiple
    data. The goal is to allow a singe element of the list or iterator
    or whatever to have /optinally/ an attribute mapping with it.

    If you feed table() with generater functions only, it would be quite
    easy to enforce an attribute mapping on every data (row or cell) and
    allow it to be empty.

    But what about a simple table() call like:
    table([[1, 2, 3], [1, 2, 3]]) It would be awful to wrap every cell
    /and/ every row with an empty mapping-padding. The solution to have
    two functions like table() and fancytable() does not allow mixtures.
    Imagine the last example and only the last cell needs special marking:
    table([[1, 2, 3], [1, 2, [3, dict(id='last')]]])
    Or an easy table with heading:
    table([[['Region', 'Price'], {'th':1}], ['Germany', 0], ['Spain', 0]])
    Looks fair to me...

    So, until I get a clashing case the implementation works as follows:
    -if you don't use an extra attribute mapping, make sure the data does
    not have a dict-like member on its last position.
    Last position is: [-1]
    Dict-like is: isdictlike(obj) - This checking allows a huge variety
    of fancy_attribute_supplier_objects_having_items() classes to be used.

    Other implementations could narrow down the allowed attribute dicts
    to support border cases to be data.

    You should be able to decorate a single thing:
    'thing' becomes ('thing', dict(...))
    This means we have a container that we didn't had before. So it would be
    natural to pick out the dict() and unwrap the 'thing'.
    No consider a container that needs decoration. We have two options:
    [1, 2, 3, dict()] or
    [[1 ,2, 3], dict()]
    While the second one is logically correct it looks very verbose and I
    prefer the first notation.
    To resolve this, the implementation always tries to remove a wrapping,
    but only if it can. This raises no problem with the logical correct
    second notation (it removes the wrapping, because it can), but raises
    an issue with the first if it could remove a wrapping, but it shouldn't!:
    [1, dict()] with the meaning of a decorated [1,]

    This implementation addionally does not unwrap the remainder of obj if
    it looks already unwrapped. This (misfeature?) supports things like
    this:
    [1, 2, 3, {stuff}]
    meaning:
    [[1, 2, 3], {stuff}]
    If you don't use that notation, no problem. But if you do, beware, that
    you cant use containers of len()==1 as opaque data and then append the
    attribute dictionary, except if they stay the same after unpacking.
    -strings of any kind: no problem; you couldn't append a dictionary
    here, could you?
    -lists like ['one-item'] are a problem, if the function processing
    the item expects luples. Pass "inline_attr=1" to the function in this
    case if you can...

    >>> decompose_element_attr(1)
    (1, {})
    >>> decompose_element_attr([1, 2])
    ([1, 2], {})
    >>> decompose_element_attr('cell')
    ('cell', {})
    >>> decompose_element_attr(('cell', dict(with='attr')))
    ('cell', {'with': 'attr'})
    >>> decompose_element_attr((['row cell1', 'cell2', 'cell3'], dict(with='attr')))
    (['row cell1', 'cell2', 'cell3'], {'with': 'attr'})
    >>> decompose_element_attr(['row cell1', 'cell2', 'cell3', dict(with='attr')])
    (['row cell1', 'cell2', 'cell3'], {'with': 'attr'})
    >>> decompose_element_attr(['row cell1', 'cell2', 'cell3'])
    (['row cell1', 'cell2', 'cell3'], {})
    >>> decompose_element_attr(['row cell1', ('cell2', {'th': 1}), 'cell3'])
    (['row cell1', ('cell2', {'th': 1}), 'cell3'], {})
    >>> decompose_element_attr(('cell2', {'th': 1}))
    ('cell2', {'th': 1})
    >>> decompose_element_attr(['row cell1', (['ce', 'll'], {'th': 1}), 'cell3'])
    (['row cell1', (['ce', 'll'], {'th': 1}), 'cell3'], {})
    >>> decompose_element_attr((['ce', 'll'], {'th': 1}))
    (['ce', 'll'], {'th': 1})
    >>> decompose_element_attr(('ce', 'll', {'th': 1}))
    (('ce', 'll'), {'th': 1})
    '''
    try:
        attr = obj[-1]
    except (IndexError, TypeError):
        attr = {}
    else:
        if isdictlike(attr):
            obj = obj[:-1]
            # and unwrap it once...
            if len(obj) == 1 and not prefer_container:
                obj = obj[0]
        else:
            attr = {}
    return obj, attr

class Output(object):
    def __init__(self, file):
        self.file = file
        self.write = self.file.write

    def form_rows(self, layout, scaffold, prefill={}, errors=[]):
        scf = []
        hiddenfirst = False
        for s in scaffold:
            if s[0] == 'hidden' and scf:
                scf[-1].append(s)
            elif s[0] == 'hidden':
                scf.append([s])
                hiddenfirst = True
            elif hiddenfirst:
                scf[-1].append(s)
                hiddenfirst = False
            else:
                scf.append([s])
        if 'general' in errors:
            m = 'Un error ha ocurrido. Por favor, intente con otros valores.'
            err = len(errors) - 1
            if layout == 'rows' and err:
                cs = 3
            elif layout == 'rows':
                cs = 2
            else: # layout == 'cols'
                cs = len(scf)
            yield [m, dict(colspan=cs)]
        else:
            err = len(errors)
        if layout == 'rows': # emit rows like: label, form element, error
            for s in scf:
                y = [lambda s=s: self.form_label(s),
                    lambda s=s: self.form_control(s, prefill)]
                if err: y.append(lambda s=s: self.form_error(s, errors))
                yield y
        else: # emit form in cols: 1 row lables, 1 row form elements, 1r errors
            yield self.form_labels(scf)
            yield self.form_controls(scf, prefill)
            if err: yield self.form_errors(scf, errors)

    def form_labels(self, scf):
        for s in scf:
            yield lambda s=s: self.form_label(s)
    def form_label(self, instructs):
        for w in instructs:
            if w[0] in ('text', 'password', 'dropdown'):
                # type, name, Label(, ddarray)
                self.label(w[2], for_=w[1])

    def form_controls(self, scf, prefill={}):
        for s in scf:
            yield lambda s=s: self.form_control(s, prefill)
    def form_control(self, instructs, prefill={}):
        for w in instructs:
            w1 = str(w[1])   # to handle only with names of string type
            if w[0] in ('text', 'password'):
                # type, name, Label(, size)
                d = dict(id=w1, name=w1, type=w[0], size=30)
                if len(w) >= 4: d['size'] = w[3]
                if d['name'] in prefill: d['value'] = prefill[d['name']]
                self.input(d)
            elif w[0] == 'submit':
                # type, Label _or_ type, name, value/Label
                # can't receive prefill
                if len(w) >= 3:
                    self.input(type=w[0], name=w1, value=w[2])
                else:
                    self.input(type=w[0], value=w1)
            elif w[0] == 'hidden':
                # type, name(, value)
                d = dict(type=w[0], name=w1)
                if len(w) >= 3:
                    d['value'] = w[2]
                elif d['name'] in prefill:
                    d['value'] = prefill[d['name']]
                self.input(d)
            elif w[0] == 'dropdown':
                # dropdown, name, Label, array
                d = dict(id=w1, name=w1, size=1)
                self.select(selected(w[3], prefill.get(d['name'], None)), d)

    def form_errors(self, scf, errors):
        for s in scf:
            yield lambda s=s: self.form_error(s, errors)
    def form_error(self, instructs, errors):
        p = []
        for w in instructs:
            for e in errors:
                k = key(w)
                if k and e.endswith(k):
                    if e.startswith('toolong'):
                        p.append('Campo demasiado largo.')
                    elif e.startswith('outofrange'):
                        p.append('Valor fuera del rango.')
                    elif e.startswith('invalid'):
                        p += ['No se puede interpretar el valor.']
        self.p(p, class_='error')

    def tag(self, element, attributes=None, **keywords):
        '''Writes the (start) tag of element with attributes attached.

        If the element is empty by definition, it includes the closing
        slash "... />". element gets used as is. attributes are optional
        and can be supplied with a dictionary, keyword arguments or both.
        keyword arguments overwrite the dictionary. Attributes should follow
        the guidelines from attrs().

        >>> a = tag('a')
        >>> a
        u'<a>'
        >>> type(a)
        <class '__main__.Html'>
        >>> tag('br')
        u'<br />'
        >>> tag('img', id='one')
        u'<img id="one" />'
        >>> dic = {'one': 1, 'two': 2}
        >>> tag('unknown', dic)
        u'<unknown one="1" two="2">'
        >>> tag('p', dic, class_='green')
        u'<p class="green" one="1" two="2">'
        >>> tag('p', {'class_': 'normal'}, class_='one s')
        u'<p class="one s">'
        >>> tag('p', {'class': 'normal'}, class_='underscore and "')
        u'<p class="underscore and &quot;">'
        >>> tag('p', x='zero', x__='two')
        u'<p x="zero" x_="two">'
        >>> tag('p', x='zero', x_='&')
        u'<p x="&amp;">'
        '''
        x = _elements[element]
        ats = join_dicts(attributes, keywords)
        self.write('<%s%s%s>%s' % (element, attrs(ats), x.empty, x.tagnl))

    def endtag(self, element):
        '''Writes the closing tag for %element.

        This function is not able to close empty elements.
        >>> endtag('woohoo')
        u'</woohoo>'
        >>> endtag('a')
        u'</a>\\n'
        >>> endtag('br')
        Traceback (most recent call last):
        ...
        ValueError: I cant close empty elements.
        '''
        x = _elements[element]
        if x.empty: raise ValueError('I cant close empty elements.')
        self.write('%s</%s>%s' % (x.preendtagnl, element, x.endtagnl))

    def start(self, title, style_url=None, style=None):
        self.write('''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
        "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">\n<html><head>\n''')

        self.title(title)
        if style_url:
            self.link(href=style_url, rel='stylesheet')
        if style:
            self.style(style, {'type': 'text/css'})
        self.write('</head><body>\n')

    def end(self):
        self.write('</body></html>\n')

    def filled_element(self, element, filling, atdic=None, **keywords):
        '''Return a completed element node, but does more if it gets a
        container...

        >>> filled_element('a', 'b')
        u'<a>b</a>'
        >>> filled_element('th', 'b c')
        u'<th>b&nbsp;c</th>'
        >>> filled_element('br', '')
        Traceback (most recent call last):
        ...
        ValueError: I cant close empty elements.
        >>> filled_element('p', 'Paragraph ', class_='example')
        u'<p class="example">Paragraph </p>'
        >>> filled_element('a', 'Execute it again!', {'id': 'unique', 'href': \
        'autoexec.bat'}, class_='example')
        u'<a class="example" href="autoexec.bat" id="unique">Execute it again!</a>'
        >>> type(filled_element('b', ''))
        <class '__main__.Html'>
        '''
        if not iscontainer(filling):
            filling = [filling]
        for fill in filling:
            self.tag(element, atdic, **keywords)
            if isinstance(fill, collections.Callable):
                fill()
            else:
                self.write(esc(fill))
            self.endtag(element)

    def element_array(self, __name, *container, **key):
        '''Wraps recursive containers up as Html.

        The first tag level is dictated by %element. This does not mean that
        the container elements get wrapped in that. It just means that the
        Html code starts with %element. The content of the could get wrapped
        in subsequent elements.

        The function uses some heuristic with known elements and works
        recursive. With unknown element names, every index gets wrapped in
        that element or the function descends recurively if a container is
        detected.

        If %obj is no container, it gets treated as one-element-container.

        element_array('table', row_iterator(), cap='My table!')
        element_array('p', p1, p2, p3, class_='news')
        element_array('p', p4)
        vector = [p8, p9, p10]
        element_array('p', vector)
        vec2 = (p11, p12, p13, dict())
        element_array('p', vector)
        element_array('ul', 4)
        element_array('tr', 'hallo')
        Known elements:
        td: wraps in td, except if some array element is decorated by th
            containers get called with table
        th: like td
        tr: tr wrapped, containers: td or th if decorated as such
        table: depends on length: 1 --> tr
            2 --> thead, tbody
            3 --> thead, tbody, tfoot
            4 and more --> error
        li: wraps in li, containers get called from decorator or hint
        ul: --> li hinted with ul
        ol: like ul

        Otherwise it iters over obj, expecting optionally "attribute
        enhenced" cell data. If the row attributes contain the special "th"
        which evaluates to True, the default for all cells change from <td>
        to <th> wrapping.

        Cell attributes get passed on. One exceptions: "th" overwrites
        row default wrapping.
        '''
        # print __name, container
        super_elements = ('table', 'ul', 'ol', 'select', 'form', 'fieldset')
        # step 1: no wrapping for free, one positional argument or several
        # We get positional arguments in a list, even when received just one.
        if len(container) == 1:
            container = container[0]
        # step 2: we work only with container. As a convenient, we wrap up.
        if not iscontainer(container):
            container = [container]
        # step 3: super_elements yes, get a wrapping for free
        # they have to omit they natural wrapping
        if __name in super_elements:
            container = [container]
        container, main_attrs = decompose_element_attr(container, prefer_container=1)
        main_attrs = join_dicts(main_attrs, key)
        for one in container:
            cdata, cattrs = decompose_element_attr(one)
            all_attrs = join_dicts(main_attrs, cattrs)
            __name, cdata, desc, desc_at = _element_fixup(__name, cdata, all_attrs)
            if iscontainer(cdata):
                eaa = _elements[__name].ele_as_attr
                eaav = all_attrs.pop(eaa, None)
                self.tag(__name, all_attrs)
                if eaa and eaav != None:
                    self.filled_element(eaa, eaav)
                self.element_array(desc, cdata, **desc_at)
                self.endtag(__name)
            else:
                self.filled_element(__name, cdata, all_attrs)

    def a(self, url, text, adic=None, **attr):
        attr['href'] = url
        self.filled_element('a', text, adic, **attr)

    def __getattr__(self, name):
        # print '__getattr__ called for:', name
        if name in _elements:
            if _elements[name].empty:
                f = lambda self, *pos, **key: self.tag(name, *pos, **key)
            else:
                f = lambda self, *pos, **key: self.element_array(name, *pos, **key)
            setattr(self.__class__, name, f)
            return getattr(self, name)
        else:
            raise AttributeError

    def d(self, *pos, **key):
        return d(self, None, *pos, **key)

# have a normal class html.Output(file-like-obj-to-write-to)
# and a html.String() if you just want to get strings...

class String(object):
    def __init__(self):
        self._buffer = io.StringIO()
        self._output = Output(self._buffer)
        self.write = self._buffer.write

    def __getattr__(self, name):
        if name in self.redirect:
            setattr(self.__class__, name,
                lambda self, *pos, **key: self._call_wrapper(name, *pos, **key))
            return getattr(self, name)
        else:
            raise AttributeError

    #def __getattribute__(self, name):
    #    def get(what): return object.__getattribute__(self, what)
    #    cw = get('_call_wrapper')
    #    if name in get('redirect'):
    #        return lambda *pos, **key: cw(name, *pos, **key)
    #    else:
    #        return get(name)

    def _flush(self):
        ret = self._buffer.getvalue()
        self._buffer.truncate(0)
        return Html(ret)

    def _call_wrapper(self, methodname, *pos, **key):
        # print self, methodname, pos, key
        getattr(self._output, methodname)(*pos, **key)
        return self._flush()

    def d(self, *pos, **key):
        return d(self._output, self._flush, *pos, **key)

    names = [x[0] for x in inspect.getmembers(Output, inspect.ismethod)]
    names = [x for x in names if x[0] != '_']  # no "private" members
    redirect = set(names).union(_elements)
    del names

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    _test()
