#!/usr/bin/python2

import argparse
import glob
import os
import pystache
import sys
import util

def page_title(args, page):
    meta = util.metadata(page, args['repository_path'])
    try:
        title = meta['title'][0]
    except:
        title = os.path.basename(page)
    return title

def li_info(args, line):
    paths = util.line2paths(line, args['repository_path'], args['uri'])
    for path in paths:
        link = util.path2link(path, args['repository_path'])
        title = page_title(args, link)

        # TODO handle folders properly so they show up as folders
        if os.path.isdir(path):
            title = os.path.basename(path)
            link = link + '/'

        yield {'title': title, 'link': link}

def ul_info(args, iterlines):
    info = {'pages': []}
    for line in iterlines:
        line = line.strip()#.lstrip('/')
        if len(line) == 0:
            continue
        for page in li_info(args, line):
            info['pages'].append(page)
    info['pages'] = sorted(info['pages'], key=lambda x: x['title'])
    return info

def render_ul(args, info):
    path = os.path.join(args['templates_dir'], 'globlist.mustache')
    with open(path, 'rb') as f:
        template = f.read()
    return pystache.render(template, info)

def parse_args(args):
    expected = [ '--repository-path', '--templates-dir', '--uri']
    parser = argparse.ArgumentParser()
    for arg in expected:
        parser.add_argument(arg)
    parsed, skipped = parser.parse_known_args(args)
    return parsed.__dict__

if __name__ == '__main__':
    args = parse_args(sys.argv)
    info = ul_info(args, sys.stdin)
    print render_ul(args, info)
