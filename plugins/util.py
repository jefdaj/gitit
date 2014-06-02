#!/usr/bin/python2

# TODO rewrite metadata to use YAML parser? nah, not unless there's a need

import datetime
import glob
import fnmatch
import os
import re

def link2path(link, repo):
    'convert a gitit-style link to a path'
    path = sanitize_uri(link)
    path = path.lstrip('/')
    path = os.path.join(repo, path)
    path = path + '.page'
    return path

def path2link(path, repo):
    'convert a path to an absolute gitit-style link'
    link = os.path.relpath(path, repo)
    link = re.sub('\.page$', '', link)
    link = '/' + link
    return link

def link2abs(link, uri):
    'convert a relative link to an absolute one'
    uri = sanitize_uri(uri)
    link = os.path.join(os.path.dirname(uri), link)
    return link

def line2paths(line, repo, uri, exclude=['about.page']):
    'glob for paths matching a line'
    if line.startswith('/'):
        # it starts with the repository_path
        patn = repo + line
    else:
        # it start with the uri dirname
        uri = sanitize_uri(uri)
        patn = '/'.join((repo, os.path.dirname(uri).lstrip('/'), line))
    paths = glob.glob(patn)
    paths = [p for p in paths if not os.path.basename(p) in exclude]
    return paths

def sanitize_uri(uri):
    return re.sub('_[a-z]*\/', '', uri)

def allpages(repo):
    'get gitit-style links to all wiki pages'
    # see http://stackoverflow.com/questions/2186525
    links = set()
    for root, dirnames, filenames in os.walk(repo):
        for filename in fnmatch.filter(filenames, '*.page'):
            links.add(path2link(os.path.join(root, filename), repo))
    return sorted(links)

def metadata(link, repo):
    'extracts a metadata dict from a page'
    meta = {}
    key  = None
    try:
        fhdl = open(link2path(link, repo), 'rb')
    except:
        return meta
    try:
        if fhdl.readline().strip() != '---':
            # no metadata
            return meta
        while True:
            line = fhdl.readline().strip()
            if line == '...':
                # end of metadata
                return meta
            if ':' in line:
                # new key
                key, val = [s.strip() for s in line.split(':', 1)]
                if len(val) > 0:
                    meta[key] = val.split(',')
            else:
                # new val for last key
                meta[key] += line.split(',')
    except:
        raise
    finally:
        fhdl.close()

def daysold(path):
    'get the age of a file in days'
    try:
        then = [int(s) for s in os.path.basename(path).split('_')[0].split('-')]
        then = datetime.datetime(*then)
        now  = datetime.datetime.now()
        diff = now - then
        return diff.days
    except:
        return None
