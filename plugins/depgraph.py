#!/usr/bin/python2

# TODO going to have to redo the hashed id stuff
#      to have more than one graph per page...

# if you see "Couldn't import dot_parser..." in pages, try this:
# http://stackoverflow.com/questions/15951748

import argparse
import BeautifulSoup
import os
import pydot
import subprocess
import sys
import urllib2
import util
import xml.dom.minidom as md


###########
# dotgraph
###########

def glob_targets(args, lines):
    targets = set()
    for line in lines:
        line = line.strip()
        paths = util.line2paths(line, args['repository_path'], args['uri'])
        for path in paths:
            path = util.path2link(path, args['repository_path'])
            targets.add(path)
    return targets

def dotgraph(args, targets):
    'constructs the initial dotgraph'

    # figure out which nodes and edges will be in the graph
    nodes = set()
    edges = set()
    for dst in util.allpages(args['repository_path']):
        # load dependencies for current page
        meta = util.metadata(dst, args['repository_path'])
        meta.setdefault('follows', [])
        raw = meta['follows']
        deps = set()
        for r in raw:
            for d in r.split(' '):
                if len(d) == 0:
                    continue
                d = os.path.join(os.path.dirname(dst), d)
                deps.add(d)

        # if it's a target, add it plus all dependencies
        if dst in targets:
            nodes.add(dst)
            nodes.update(deps)
            for src in deps:
                edges.add((src, dst))
        # if it depends on the target, add it (but not other dependencies)
        else:
            deps = set(targets).intersection(deps)
            if deps:
                nodes.add(dst)
            for src in deps:
                edges.add((src, dst))

    # create the graph
    g = pydot.Dot(graph_name='depgraph', ratio='compress')
    for n in nodes:
        meta = util.metadata(n, args['repository_path'])
        try:
            name = meta['title'][0]
        except:
            name = os.path.splitext(os.path.basename(n))[0]
        g.add_node(pydot.Node(n, label=name, href=n))
    for e in edges:
        g.add_edge(pydot.Edge(*e))

    # TODO constrain width and height if passed as args
    # constrain width but not height
    # see http://stackoverflow.com/questions/3795200/size-of-picture
    # not sure the exact args matter; more orders of magnitude?
    g.set_size('9,999!')

    g = theme(args, g)
    g = stagger(args, g)
    return g

def stagger(args, graph):
    'stagger nodes of the same rank by piping a dotgraph through unflatten'
    c = ['unflatten', '-l %d' % args['stagger'], '-f']
    p = subprocess.Popen(c, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    o = p.communicate(input=graph.to_string())[0]
    return pydot.graph_from_dot_data(o)

def is_new_node(args, name):
    days = util.daysold(util.link2path(name, args['repository_path']))
    return days is not None and days < 90

def colorize(args, node):
    'choose node color based on days since the page changed'
    orig = args['color']
    name = node.get_name()[1:-1]
    if is_new_node(args, name):
        return args['color']
    return '#CCCCCC'

def theme(args, graph):
    graph = pydot.graph_from_dot_data(graph.to_string())
    'set colors, sizes, shapes etc'
    attr = lambda t,a: t.obj_dict['attributes'].update(a)
    for e in graph.get_edges():
        attr(e, {'color':'#888888'})
    for n in graph.get_nodes():
        c = colorize(args, n)
        attr(n, {'penwidth':2, 'fontsize':11, 'color':c})
        attr(n, {'shape':'rectangle'})
    graph.set_rankdir(args['rankdir'])
    return graph


###########
# imagemap
###########

def svg_contents(args, graph):
    h = hash(graph.to_string())
    p = args['cache_dir']
    if not os.path.exists(p):
        os.mkdir(p)
    p = os.path.join(p, 'depgraph%s.svg' % h)
    if not os.path.exists(p):
        graph.write_svg(p)
        fix_generated_svg(args, p)
    with open(p, 'rb') as f:
        return f.read()

def fix_generated_svg(args, path):
    'fixes the output of Dot.write_svg so it works as an imagemap'
    # see http://demosthenes.info/blog/696/Using-SVG-as-an-Alternative-To-Imagemaps
    xml = md.parse(path)
    # fix the root svg element
    e = xml.getElementsByTagName('svg')[0]
    e.removeAttribute('width')
    e.removeAttribute('height')
    e.setAttribute('preserveAspectRatio', 'xMinYMin meet')
    e.setAttribute('class', 'svg-content')

    # add the css stylesheet
    #t = 'type="text/css" href="%s"' % os.path.join(args['cache_dir'], 'depgraph.css')
    #s = md.ProcessingInstruction('xml-stylesheet', t)
    #xml.insertBefore(s, e)

    # change class and fill for all the graph elements
    for g in xml.getElementsByTagName('g'):
        c = g.getAttribute('class')
        if c == 'node':
            t = str(g.childNodes[0].childNodes[0].data)
            if is_new_node(args, t):
                g.setAttribute('class', 'newnode')
            else:
                g.setAttribute('class', 'oldnode')
        elif c == 'graph':
            g.setAttribute('fill', 'white')
        elif c == 'edge':
            g.setAttribute('fill', '#888888')
    # remove node fill so it doesn't override css
    for p in xml.getElementsByTagName('polygon'):
        # TODO hash names
        p.removeAttribute('fill')
    # but then purposely override some
    for t in xml.getElementsByTagName('text'):
        t.setAttribute('fill', 'black')
    # write fixed svg only (not the xml header)
    with open(path, 'wb') as f:
        svg = xml.childNodes[3]
        f.write(svg.toxml())

def svg_container(args, svg):
    'takes the raw svg and puts it into an svg-container div'
    return '<div class="svg-container">%s</div>' % svg


############
# interface
############

def parse_args():
    'parse command line args'
    args = sys.argv[1:]
    ap = argparse.ArgumentParser()
    ap.add_argument('--rankdir', choices=['TB', 'BT', 'RL', 'LR', None], default='TB')
    ap.add_argument('--color'  , type=str, default='#61BD4D') # green
    ap.add_argument('--width'  , type=int, default=850)
    ap.add_argument('--stagger', type=int, default=3  )
    ap.add_argument('--repository-path', type=str)
    ap.add_argument('--templates-dir'  , type=str)
    ap.add_argument('--cache-dir'      , type=str)
    ap.add_argument('--uri'            , type=str)
    parsed, skipped = ap.parse_known_args(args)
    parsed = parsed.__dict__
    parsed['uri'] = util.sanitize_uri(parsed['uri'])
    return parsed

if __name__ == '__main__':
    args = parse_args()
    lines = list(sys.stdin.readlines())
    targets = glob_targets(args, lines)
    if len(targets) > 0:
      dotg = dotgraph(args, targets)
      svg = svg_contents(args, dotg)
      svg = svg_container(args, svg)
      print svg
